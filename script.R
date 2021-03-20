library(tidyverse)
library(Hmisc)
library(lubridate)
library(googledrive)
library(googlesheets4)
library(shiny)
library(progress)

updated <- function(rating, mov, prior) {
  x <- (pnorm( mov + 1 , mean = rld$mean[rld$cumNetRating == rating], sd = rld$sd[rld$cumNetRating == rating]) - pnorm(mov, mean = rld$mean[rld$cumNetRating == rating], sd = rld$sd[rld$cumNetRating == rating])) * prior
  if (length(x) == 0) {
    x = 1*10^-13
    return(x)
  } else {
    return(x)
  }
} 

#read in files 
{files <- list()
  for (i in gs4_get(drive_get('ratingApp backend files'))[[6]][[1]]) {
    files[[i]] <- drive_get('ratingApp backend files') %>%
      range_read(i)
    Sys.sleep(8)
  }
  list2env(files, envir = .GlobalEnv)
  remove(files)}

#update chance2
{
  scores <- nbastatR::days_scores(game_dates = as.character(Sys.Date() - 1) , include_standings = F, return_message = T)[[2]][[2]] %>%
    select(idGame, slugTeam, pts) 
  daily <- filter(newSchedule, idGame %in% scores$idGame)
  daily <- daily %>%
    filter(!is.na(teamScore))
  for (i in 1:length(unique(scores$idGame))) {
    daily$teamScore[daily$team == daily$team[i] & daily$idGame == daily$idGame[i]] <- scores$pts[scores$idGame == daily$idGame[i] & scores$slugTeam == daily$team[i]]
    daily$opponentScore[daily$opponent == daily$opponent[i] & daily$idGame == daily$idGame[i]] <- scores$pts[scores$idGame == daily$idGame[i] & scores$slugTeam == daily$opponent[i]]
  }
  daily <- daily %>%
    mutate(mov = teamScore - opponentScore) %>%
    drop_na(.)
  for (i in 1:nrow(daily)) {
    if (daily$mov[i] > 0) {
      wins$wins[wins$team == daily$team[i]] <- wins$wins[wins$team == daily$team[i]] + 1
    } else if (daily$mov[i] < 0) {
      wins$wins[wins$team == daily$opponent[i]] <- wins$wins[wins$team == daily$opponent[i]] + 1
    }
    chance2$likelihood[chance2$team == daily$team[i]] <- pmap_dbl(list(chance2$ratings[chance2$team == daily$team[i]], daily$mov[i], chance2$likelihood[chance2$team == daily$team[i]]), updated) / sum(pmap_dbl(list(chance2$ratings[chance2$team == daily$team[i]], daily$mov[i], chance2$likelihood[chance2$team == daily$team[i]]), updated))
    chance2$likelihood[chance2$team == daily$opponent[i]] <- pmap_dbl(list(chance2$ratings[chance2$team == daily$opponent[i]], (-1*daily$mov[i]), chance2$likelihood[chance2$team == daily$opponent[i]]), updated) / sum(pmap_dbl(list(chance2$ratings[chance2$team == daily$opponent[i]], (-1*daily$mov[i]), chance2$likelihood[chance2$team == daily$opponent[i]]), updated))
  }
  
  chanceCurrent <- chance2
  schedule2021two <- schedule2021two %>%
    filter(idGame %nin% daily$idGame)
  simulatedWins <- conf
  simulatedRating <- conf
  n <- 3
  currentRatings <- chance2 %>%
    group_by(team) %>%
    summarise(current = sum(likelihood*ratings))
}
#generate daily game predictions
{
  projections <- projections[1,]
  dailyGames <- nbastatR::days_scores(game_dates = as.character(Sys.Date()))[[2]][[2]] %>%
    summarise(games = unique(idGame))
  for (i in 1:nrow(dailyGames)) {
    td <- schedule2021two$team[schedule2021two$idGame == dailyGames$games[i]]
    od <- schedule2021two$opponent[schedule2021two$idGame == dailyGames$games[i]]
    
    diff2 <- round(currentRatings$current[currentRatings$team == td] - currentRatings$current[currentRatings$team == od])
    spread <- round(outcomeDistributions$mean[outcomeDistributions$CNRdiff == diff2])
    winning <- round(pnorm(0, mean = outcomeDistributions$mean[outcomeDistributions$CNRdiff == diff2], sd = outcomeDistributions$sd[outcomeDistributions$CNRdiff == diff2], lower.tail = F), 2)
    
    projections <- add_row(projections, team = td, teamWinning = winning, opponent = od, opponentWinning = 1 - winning, spread = -1 * spread)
  }
  range_clear(drive_get('ratingApp'), sheet = 'games')
  Sys.sleep(8)
  range_write(drive_get('ratingApp'), projections, sheet = 'games')
}

#simulate regular season
for (k in 1:1000) {
  
  simulatedWins[, paste0('wins', k, sep = '')] <- wins$wins
  for (i in 1:nrow(schedule2021two)) {
    t <- schedule2021two$team[i]
    o <- schedule2021two$opponent[i]
    
    diff <- round(sum(chance2$likelihood[chance2$team == t] * chance2$ratings[chance2$team == t]) - sum(chance2$likelihood[chance2$team == o] * chance2$ratings[chance2$team == o]), 0)
    
    mov <- round(rnorm(1, mean = outcomeDistributions$mean[outcomeDistributions$CNRdiff == diff], sd = outcomeDistributions$sd[outcomeDistributions$CNRdiff == diff]), 0)
    
    if (mov == 0) {
      mov <- sample(c(-1, 1), 1)
    }
    
    if (mov > 0) {
      simulatedWins[simulatedWins$team == t, n] <- simulatedWins[simulatedWins$team == t, n] + 1
      simulatedWins[simulatedWins$team == o, n] <- simulatedWins[simulatedWins$team == o, n] 
    } else if ( mov < 0) {
      simulatedWins[simulatedWins$team == o, n] <- simulatedWins[simulatedWins$team == o, n] + 1
      simulatedWins[simulatedWins$team == t, n] <- simulatedWins[simulatedWins$team == t, n] 
    }
    
    nmov <- mov*-1
    
    chance2$likelihood[chance2$team == t] <- pmap_dbl(list(chance2$ratings[chance2$team == t], mov, chance2$likelihood[chance2$team == t]), updated) / sum(pmap_dbl(list(chance2$ratings[chance2$team == t], mov, chance2$likelihood[chance2$team == t]), updated))
    chance2$likelihood[chance2$team == o] <- pmap_dbl(list(chance2$ratings[chance2$team == o], nmov, chance2$likelihood[chance2$team == o]), updated) / sum(pmap_dbl(list(chance2$ratings[chance2$team == o], nmov, chance2$likelihood[chance2$team == o]), updated))
    
  }
  simulatedRating[, paste0('rating', k, sep = '')] <- chance2 %>%
    group_by(team) %>%
    summarise(sum = sum(likelihood*ratings)) %>%
    select(sum)
  
  chance2 <- chanceCurrent
  n <- n + 1
  pb$tick()
} 

#simulate playoffs
{playoffs <- simulatedRating %>%
    gather(key = 'season', value = 'rating', -c(conf, team)) %>%
    group_by(season, conf) %>%
    slice_max(order_by = rating, n = 8)
  finals <- tibble('seed' = NA, 'team' = NA, 'conf' = NA, 'season' = NA, 'rating' = NA, 'wins' = NA)
  
  for (s in unique(playoffs$season)) {
    set <- filter(playoffs, season == s)
    
    set <- bind_cols(seed = c(1:16), set, wins = 0)
    set <- set[match(c(1,8,4,5,2,7,3,6,9,16,12,13,10,15,11,14), set$seed), ]
    set$seed[set$conf == 'W'] <- set$seed[set$conf == 'W'] - 8
    
    for (k in c(15, 7, 3, 1)) {
      for (i in seq(1, k, 2)) {
        while (set$wins[i] < 4 && set$wins[i+1] < 4 ) {
          diffq <- round(set$rating[i] - set$rating[i+1])
          movq <- round(rnorm(1, mean = outcomeDistributions$mean[outcomeDistributions$CNRdiff == diffq], sd = outcomeDistributions$sd[outcomeDistributions$CNRdiff == diffq]))
          if (movq == 0) {
            movq <- sample(c(-1, 1), 1)
          }
          if (movq > 0) {
            set$wins[i] <- set$wins[i] + 1
          } else if (movq < 0) {
            set$wins[i+1] <- set$wins[i+1] + 1
          }
        }
      }
      set <- set %>%
        filter(wins == 4) 
      set$wins <- 0
    }
    finals <- bind_rows(finals, set)
  }}

# generate simulated results table
{as_tibble(plyr::join_all(list( conf,
                                currentRatings, 
                                simulatedRating %>%
                                  gather(., key = 'season', value = 'rating', -c(team, conf)) %>%
                                  group_by(team) %>%
                                  summarise(projectedRating = mean(rating)),
                                simulatedWins %>%
                                  gather(., key = 'season', value = 'wins', -c(team, conf)) %>%
                                  group_by(team) %>%
                                  summarise(projectedWins = mean(wins)),
                                simulatedRating %>%
                                  gather(key = 'season', value = 'rating', -c(team, conf)) %>%
                                  group_by(season, conf) %>%
                                  slice_max(order_by = rating, n = 8) %>%
                                  ungroup() %>%
                                  count(team) %>%
                                  mutate(playoffs = n / (ncol(simulatedRating)-2), .keep = 'unused'),
                                finals %>%
                                  count(team) %>%
                                  mutate(finals = n / (nrow(finals)-1), .keep = 'unused')) , by = 'team'), type = 'left') %>%
    range_write(drive_get('ratingApp'), ., sheet = 'seasonProjections')
}

# update backend files 
{
  drive_get('ratingApp backend files') %>%
    range_write(., chance2, 'chance2')
  Sys.sleep(8)
  drive_get('ratingApp backend files') %>%
    range_clear('schedule2021two')
  Sys.sleep(8)
  drive_get('ratingApp backend files') %>%
    range_write(., schedule2021two, 'schedule2021two')
}