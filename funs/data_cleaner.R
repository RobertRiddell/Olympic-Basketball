library(tidyverse)
library(lubridate)

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

all_data <- all_data %>% 
  mutate(Year = year(Date)) 

all_data <- all_data %>% 
  filter(!Player == "Totals", !Player == "Reserves")

all_data <- janitor::clean_names(all_data)

all_data$country[all_data$country == "great-britain"] <- "great britain"
all_data$country[all_data$country == "united-states"] <- "united states"
all_data <- all_data %>% 
  mutate(across(mp:pts, as.numeric))

all_data[is.na(all_data)] <- 0

all_data
# PER

game_total <- all_data %>% 
  group_by(year, id, country, date, tournament_stage,game_number, competition) %>% 
  summarise(across(.cols = mp:pts, sum)) %>% 
  ungroup()

game_total <- game_total %>% 
  group_by(id) %>% 
  mutate(results = rep("-", length(id)),
         mov = rep(0, length(id))) %>% 
  ungroup()

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

game_total <- game_total %>% 
  mutate(drb = trb - orb)

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

all_data
game_total
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

mean_PER <- all_data %>% 
  group_by(id) %>% 
  summarise(mean_PER = mean(PER)) %>% 
  ungroup()

game_total <- left_join(game_total, mean_PER)

game_total <- game_total %>% 
  select(!VOP)

game_total <- game_total %>% 
  mutate(tournament_stage = if_else(tournament_stage == "Group A" | 
                              tournament_stage == "Group B", "Group",
                            if_else(tournament_stage == "Semi-Finals" |
                              tournament_stage == "Final Round", "Semifinals",
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


game_total <- game_total %>% 
  mutate(ft_p = if_else(is.nan(ft_p), 0, ft_p))




write_csv(all_data, "out/all_data.csv")
write_csv(game_total, "out/game_total.csv")
