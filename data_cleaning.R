library(dplyr)
library(stringr)

df <- read.csv("https://raw.githubusercontent.com/GuilleDiaz7/Automatic-Web-Scraping-of-Teledeporte/main/data/teledeporte.csv",
               fileEncoding = "UTF-8")

df %>% head()
df$gender <- NA

df <- df %>% 
  mutate(
    sport = case_when(
      str_detect(sp_title, "WTA") ~ "Tennis",
      str_detect(sp_title, "ATP") ~ "Tennis",
      str_detect(sp_title, regex("Estudio estadio", ignore_case = T)) ~ "Football",
      str_detect(sp_title, "UEFA") ~ "Football",
      str_detect(sp_title, "motociclismo") ~ "Motorbikes",
      str_detect(sp_title, regex("rallycross", ignore_case = T)) ~ "Cars",
      str_detect(sp_title, regex("basket", ignore_case = T)) ~ "Basket",
      TRUE ~ sport
    ),
    category = case_when(
      str_detect(sp_title, "FIA") ~ "Motor sports",
      str_detect(sp_title, regex("motociclismo", ignore_case = T)) ~ "Motor sports",
      str_detect(sp_title, regex("wta|atp", ignore_case = T)) ~ "Racket sports",
      str_detect(sp_title, regex("uefa|fifa", ignore_case = T)) ~ "Team sports",
      TRUE ~ category
    ),
    gender = case_when(
      str_detect(sp_title, "WTA") ~ "Female",
      str_detect(sp_title, "ATP") ~ "Male",
      str_detect(sp_title, regex("mujer|femenino", ignore_case = TRUE)) ~ "Female",
      TRUE ~ gender
    )
  )

View(df)

# a ver deberia tener: tournament, program_type, sports_category (team, motor, racket, etc), sport, subsport, gender, broadcast_type (live, repeated)
