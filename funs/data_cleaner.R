library(tidyverse)
library(lubridate)

# Iterate through the data directory to combine all the boxscores into one dataframe
comps <- dir("data")
all_data <- c()
for (c in comps){
  years <- dir(paste0("data/",c,"/stats/"))
  for (i in years){
    files <- dir(paste0("data/",c,"/stats/",i))
    for (n in files){
      data <- read_csv(paste0("data/",c,"/stats/",i,"/",n))
      if (any(str_detect(colnames(data), pattern = "Starter"))){
        data <- data %>% rename(Player = Starter)
      }
      all_data <- rbind(all_data, data)
    }
  }
}

# extract the year from the Date
all_data <- all_data %>% 
  mutate(Year = year(Date)) 

#Remove the total line and reserves lines from box score
all_data <- all_data %>% 
  filter(!Player == "Totals", !Player == "Reserves")

# change the column names to be more interpretable by R
all_data <- janitor::clean_names(all_data)

# Coutries with hyphens ahve been spelt in two different ways. 
# This updates them so they are seen as teh same coutry
# Could be done with str_replace() but this coudl have uninteneded consequences
# This is slower but in this circumstance more accurate
all_data$country[all_data$country == "great-britain"] <- "great britain"
all_data$country[all_data$country == "united-states"] <- "united states"
all_data$country[all_data$country == "czech-republic"] <- "czech republic"
all_data$country[all_data$country == "dominican-republic"] <- "dominican republic"
all_data$country[all_data$country == "ivory-coast"] <- "ivory coast"
all_data$country[all_data$country == "new-zealand"] <- "new zealand"
all_data$country[all_data$country == "puerto-rico	"] <- "puerto rico"

# The rows with reserves had charaters in them so teh columsn were seen as chr
# This converts them to numeric
all_data <- all_data %>% 
  mutate(across(mp:pts, as.numeric))

# some of the NA observations were when there was no value enterered. 
# After checking the box scores this was determined to be a 0 for that variable
all_data[is.na(all_data)] <- 0

# Summarising the player data into their country to see total figures for each game
game_total <- all_data %>% 
  group_by(year, id, country, date, tournament_stage,game_number, competition) %>% 
  summarise(across(.cols = mp:pts, sum)) %>% 
  ungroup()

# Create an empty column for results
game_total <- game_total %>% 
  group_by(id) %>% 
  mutate(results = rep("-", length(id)),
         mov = rep(0, length(id))) %>% 
  ungroup()

# The games are in groups of two and can be found using the id variable
# Iterate through the every second row to assess if the points scored are more or 
# less than fowollowing team then attribute a Win, Loss or Draw to that team
for (i in seq(1,length(game_total$id),2)){
  if (game_total$pts[i] > game_total$pts[i+1]){
    game_total[i,"results"] = "W"
    game_total[i+1,"results"] = "L"
    game_total[i,"mov"] = game_total$pts[i] - game_total$pts[i+1]
    game_total[i+1,"mov"] = game_total$pts[i] - game_total$pts[i+1]
  }
  else if (game_total$pts[i] < game_total$pts[i+1]){
    game_total[i,"results"] = "L"
    game_total[i+1,"results"] = "W"
    game_total[i,"mov"] = game_total$pts[i+1] - game_total$pts[i]
    game_total[i+1,"mov"] = game_total$pts[i+1] - game_total$pts[i]
  }
  else{
    game_total[i,"results"] = "D"
    game_total[i+1,"results"] = "D"
    game_total[i,"mov"] = game_total$pts[i+1] - game_total$pts[i]
    game_total[i+1,"mov"] = game_total$pts[i+1] - game_total$pts[i]
  }
}

# Calculating league totals so VOP, factor and DRB_p can be calculted for PER calculation
lg_totals <- game_total %>% 
  ungroup() %>% 
  summarise(lg_pts = sum(pts),
            lg_FGA = sum(fga),
            lg_ORB = sum(orb),
            lg_TRB = sum(trb),
            lg_TOV = sum(tov),
            lg_FTA = sum(fta),
            lg_FG = sum(fg),
            lg_AST = sum(ast),
            lg_FT = sum(ft),
            lg_PF = sum(pf),
            VOP = lg_pts / (lg_FGA - lg_ORB + lg_TOV + 0.44 * lg_FTA),
            factor = (2/3) - (0.5 * (lg_AST/lg_FG)) / (2*(lg_FG / lg_FT)),
            DRB_p = (lg_TRB - lg_ORB) / lg_TRB)

game_total <- game_total %>% 
  mutate(fg_p = fg/fga,
         x3P_p = x3p/x3pa,
         ft_p = ft/fta,
         tsa = fga + 0.44 * fta,
         ts_p = pts/(2*tsa),
         efg_p = (fg +0.5 *x3p)/fga,
         tov_p = 100 * tov/(fga + 0.44 * fta + tov),
         VOP = lg_totals$lg_pts / (lg_totals$lg_FGA - lg_totals$lg_ORB + lg_totals$lg_TOV + 
                                     0.44 * lg_totals$lg_FTA))

# adding defensive rebounds
game_total <- game_total %>% 
  mutate(drb = trb - orb)

# Calculating possessions
poss <- c()

for (i in seq(1,length(game_total$id),2)){
  poss[i] <- 0.5 * ((game_total$fga[i] + 0.4 * game_total$fta[i] - 1.07 * 
                       (game_total$orb[i] / (game_total$orb[i] + game_total$drb[i+1])) * 
                       (game_total$fga[i] - game_total$fg[i]) + game_total$tov[i]) + 
                      (game_total$fga[i+1] + 0.4 * game_total$fta[i+1] - 1.07 *
                         (game_total$orb[i+1] / (game_total$drb[i+1]
                                                 + game_total$drb[i])) *
                         (game_total$fga[i+1] - game_total$fg[i+1])
                       + game_total$tov[i+1]))
  
  poss[i+1] <- 0.5 * ((game_total$fga[i+1] + 0.4 * game_total$fta[i+1] - 1.07 * 
                         (game_total$orb[i+1] / (game_total$orb[i+1] + game_total$drb[i])) * 
                         (game_total$fga[i+1] - game_total$fg[i+1]) + game_total$tov[i+1]) + 
                        (game_total$fga[i] + 0.4 * game_total$fta[i] - 1.07 *
                           (game_total$orb[i] / (game_total$drb[i]
                                                 + game_total$drb[i+1])) *
                           (game_total$fga[i] - game_total$fg[i])
                         + game_total$tov[i]))
}

game_total$poss <- as.numeric(poss)

# calcuting pace
pace <- c()

for (i in seq(1,length(game_total$id),2)){
  pace[i] <- 40*((game_total$poss[i] + game_total$poss[i+1]) / (2*(game_total$mp[i]/5)))
  pace[i+1] <- 40*((game_total$poss[i+1] + game_total$poss[i]) / (2*(game_total$mp[i+1]/5)))
  }

game_total$pace <- as.numeric(pace)

team_pace <- game_total %>% 
  group_by(id) %>% 
  summarise(team_pace = mean(pace)) %>% 
  ungroup()

all_data <- left_join(all_data, team_pace)
lg_pace <- mean(all_data$team_pace)
all_data$lg_pace <- lg_pace

# Calculating variables for PER
VOP <- lg_totals$VOP[1]
factor <- lg_totals$factor[1]
DRB_p <- lg_totals$DRB_p[1]
lg_FT <- lg_totals$lg_FT[1]
lg_PF <- lg_totals$lg_PF[1]
lg_FTA <- lg_totals$lg_FTA[1]

all_data <- game_total %>% 
  group_by(id, country) %>% 
  summarise(team_AST = sum(ast),
            team_FG = sum(fg)) %>% 
  ungroup() %>% 
  left_join(., all_data)

# Calculating PER
all_data <- all_data %>% 
  group_by(player) %>% 
  mutate(PER = (1/mp) * x3p + 
           (2/3) * ast + 
           (2 - factor * (team_AST / team_FG)) * fg + 
           (ft * 0.5 * (1 + (1 - (team_AST / team_FG)) + (2/3) * (team_AST/team_FG))) - 
           VOP * tov - 
           VOP * DRB_p * (fga - fg) - 
           VOP * 0.44 * (0.44 + (0.56 * DRB_p)) * (fta - ft) + 
           VOP * (1 - DRB_p) * (trb - orb) + 
           VOP * DRB_p * orb + 
           VOP * stl + 
           VOP * DRB_p  * blk - 
           pf * ((lg_FT / lg_PF) - 0.44 * (lg_FTA / lg_PF) * VOP)) %>%
  mutate(PER = (lg_pace/ team_pace)*PER) %>% 
  ungroup() %>% 
  mutate(PER = if_else(is.na(PER), 0 , PER)) 

# summarising mean PER by game id
mean_PER <- all_data %>% 
  group_by(id) %>% 
  summarise(mean_PER = mean(PER)) %>% 
  ungroup()

# adding mean PER to game totals
game_total <- left_join(game_total, mean_PER)

#removing VOP
game_total <- game_total %>% 
  select(!VOP)

# Changing tournament stage to give a more consistent grouping across WC and olympics
game_total <- game_total %>% 
  mutate(tournament_stage = if_else(tournament_stage == "Group A" | 
                              tournament_stage == "Group B", "Group",
                            if_else(tournament_stage == "Semi-Finals" , "Semifinals",
                            if_else(tournament_stage == "Classification"|
                              tournament_stage == "Consolation",
                                                    "Consolations/ Classification",
                            if_else(tournament_stage == "Round of 16" |
                              tournament_stage == "Placement", "Knockout Stage",
                            if_else(tournament_stage == "First Round" | 
                              tournament_stage == "Second Round" |
                              tournament_stage == "Group Play", "Group",
                            if_else(tournament_stage == "Quarter-Finals" | 
                              tournament_stage =="Quarterfinals", "Knockout Stage",
                              tournament_stage)))))))

# Renaming the tournament stage games to better represent the tournament stage to represent the new groupings
game_total$tournament_stage[game_total$id == 2000092630] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2000092631] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2004082430] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2004082431] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2000093038] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2000093039] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2004082838] <- "Consolations/ Classification"
game_total$tournament_stage[game_total$id == 2004082839] <- "Consolations/ Classification"


game_total$tournament_stage[game_total$id == 2010091279] <- "Final Round"
game_total$tournament_stage[game_total$id == 2010091278] <- "Final Round"

game_total$tournament_stage[game_total$id == 2014091475] <- "Final Round"
game_total$tournament_stage[game_total$id == 2014091374] <- "Final Round"

game_total$tournament_stage[game_total$id == 2019091591] <- "Final Round"
game_total$tournament_stage[game_total$id == 2019091590] <- "Final Round"

# if a player has 0 fta then the ft_p returned is nan as it is the 0 division rule
# this changes the nan to be 0%
game_total <- game_total %>% 
  mutate(ft_p = if_else(is.nan(ft_p), 0, ft_p))

# adding a new variable to represent if the team reached the final 4 teams of the tournament
top4_by_year <- game_total %>% 
  group_by(country, tournament_stage) %>% 
  filter(tournament_stage == "Final Round") %>% 
  ungroup %>% 
  select(year, country) %>% 
  mutate(top4 = "Finished in the Top 4")

game_total <- left_join(game_total, top4_by_year)

game_total$top4[is.na(game_total$top4)] <- "Did not finish in the Top 4"

# write the data to the out folder
write_csv(all_data, "out/all_data.csv")
write_csv(game_total, "out/game_total.csv")
