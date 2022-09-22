#### LIBRARIES ####
library(rvest)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(tibble)

#### LOAD URL ####
html_data <- read_html("https://www.elmundo.es/television/programacion-tv/teledeporte.html")

df <- tibble(date = html_data %>% 
               html_nodes("li.programa-canal") %>% 
               html_attr("name"),
             time = html_data %>% 
               html_nodes(".hora-categoria") %>% 
               html_text2(),
             channel = "Teledeporte",
             sp_title = html_data %>% 
               html_nodes(".nombre-programa a") %>% 
               html_text2(), 
             sport = html_data %>% 
               html_nodes(".titulo-categoria") %>% 
               html_text2(),
             description = html_data %>% 
               html_nodes(".sinopsis-programa") %>% 
               html_text2(),
             url = html_data %>% 
               html_nodes(".nombre-programa a") %>%
               html_attr("href")
)

df_clean <- df %>% 
  filter(sp_title != "Teledeporte") %>% 
  mutate(date = ymd(substr(date, 1, 8)),
  ) %>%
  filter(date == Sys.Date() ) %>%
  transmute(date_time = ymd_hm(paste(date, time)),
            channel = channel,
            sp_title = sp_title, 
            sport = sport,
            description = description,
            url = url
  )

library(tibble)
df_clean <- df_clean %>%
  add_column(category = NA) %>%
  add_column(gender = NA) %>% 
  add_column(subsport = NA) %>% 
  


#### Perform the second part of the scraping ####
nav_results_list <- tibble(
  html_result = map(df_clean$url,
                    ~ {
                      Sys.sleep(2)
                      .x %>% 
                        read_html()
                    }),
  url = df_clean$url
)

results_by_film_url <- tibble(url = nav_results_list$url,
                              length = map(nav_results_list$html_result,
                                          ~ .x %>%
                                            html_nodes("tr:nth-child(5) .ficha-txt-descripcion") %>%
                                            html_text2()
                              )
)

df_final <- cbind(df_clean, results_by_film_url)
df_final <- df_final %>% 
  select(-url) %>% 
  mutate(length = parse_number(unlist(length))) %>% 
  relocate(category, .before = sport) %>% 
  relocate(c("subsport", "gender", "length"), .after = sport)


#### APPEND DATA DAY TO DAY TO A .CSV FILE ####
write.table(df_final, "data/teledeporte.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
