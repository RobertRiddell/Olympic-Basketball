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


game_total <- all_data %>% 
  group_by(year, id, country, date, tournament_stage,game_number, competition) %>% 
  summarise(across(.cols = mp:pts, sum)) %>% 
  ungroup()

game_total <- game_total %>% 
  group_by(id) %>% 
  mutate(results = rep("-", length(id)),
         mov = rep(0, length(id)))

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

game_total <- game_total %>% 
  mutate(fg_p = fg/fga,
         x3P_p = x3p/x3pa,
         ft_p = ft/fta,
         tsa = fga + 0.44 * fta,
         ts_p = pts/(2*tsa),
         efg_p = (fg +0.5 *x3p)/fga,
         tov_p = 100 * tov/(fga + 0.44 * fta + tov))



write_csv(all_data, "out/all_data.csv")
write_csv(game_total, "out/game_total.csv")
