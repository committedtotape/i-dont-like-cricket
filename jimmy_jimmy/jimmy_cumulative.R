library(tidyverse)
library(here)
library(janitor)
library(polite)
library(rvest)
library(ggforce)
library(glue)
library(ggtext)

#getPlayerData(profile = 8608, dir = here("data"), type = "bowling", file = "james_anderson.csv")

#jimmy <- cleanBowlerData(here("data","james_anderson.csv"))

jimmy_url <- "https://stats.espncricinfo.com/ci/engine/player/8608.html?class=1;template=results;type=bowling;view=innings"
#jimmy_page <- read_html(jimmy_url)

jimmy_table <- jimmy_url %>% 
  bow() %>% 
  scrape() %>% 
  html_table(fill = TRUE) %>% 
  .[4] %>% 
  .[[1]] %>% 
  select(-8, -12)

# jimmy_table <- jimmy_page %>% 
#   html_nodes("table") %>% 
#   .[4] %>% 
#   html_table() %>% 
#   .[[1]] %>% 
#   select(-8, -12)

jimmy_clean <- jimmy_table %>% 
  clean_names() %>% 
  mutate_at(vars(1:7), as.numeric) %>% 
  mutate(innings_no = cumsum(!is.na(overs)),
         match_no = cumsum(start_date != lag(start_date, default="")),
         overs = replace_na(overs, 0),
         balls = floor(overs)*6 + (overs - floor(overs))*10,
         cum_balls = cumsum(balls),
         cum_overs = floor(cum_balls/6) + (balls %% 6) / 10 ,
         cum_runs = cumsum(replace_na(runs, 0)),
         cum_wkts = cumsum(replace_na(wkts, 0)),
         cum_avg  = round(cum_runs / cum_wkts, 2),#rounded to 2 dec places
         cum_econ = floor(100 * cum_runs / (cum_balls/6))/ 100,#rounded to floor of 2 dec places
         year = str_sub(start_date, -4),
         label = glue("Match {match_no}\n{opposition}, {year}"))

# just keep end of match records
jimmy_by_match <- jimmy_clean %>% 
  group_by(match_no) %>% 
  filter(row_number() == max(row_number())) %>% 
  ungroup() 

# annotations
jimmy_text <- jimmy_by_match %>% 
  filter(match_no %in% c(20,49,158)) %>% 
  mutate(desc = case_when(match_no == 49 ~ glue("Avg: {cum_avg} ER: {cum_econ}\nRecords Best Match Figures of 11/71"),
                          TRUE ~ glue("Avg: {cum_avg} ER: {cum_econ}")))

jimmy_by_match %>% 
  filter(match_no >= 20) %>% 
  ggplot(aes(x = cum_econ, y = cum_avg)) +
  geom_path(aes(colour = match_no), alpha = 0.7, size = 1.5, lineend = "round") +
  geom_point(aes(fill = match_no), colour = "white", shape = 21) +
  geom_mark_ellipse(data = jimmy_text,
                    aes(label = label,
                        description = desc, group = match_no),
                    label.fontsize = 11,
                    label.family = "Avenir Next Condensed",
                    label.colour = c("#050833", "#808080"),
                    con.colour = "#808080",
                    colour = "#808080",
                    con.cap = 0) +
  labs(caption = "Graphic: @committedtotape | Source: espncricinfo") +
  annotate("text", 2.5, 39.1, label = "James Anderson, like a fine wine...", 
           family = "Avenir Next Condensed Bold", color = "#050833", hjust = 0, vjust = 0,
           size = 8) +
  annotate("text", 2.5, 38.1, label = "Cumulative Test Bowling Average and Economy Rate (since 20th Match)", 
           family = "Avenir Next Condensed Bold", color = "#808080", hjust = 0, vjust = 0,
           size = 4.2, fontface = "italic") +
  annotate("text", 2.5, 37.4, label = "\"He's like a fine wine - getting better and better\" - Joe Root after a sensational\nbowling spell from James Anderson helps England claim victory in his 158th Test Match", 
           family = "Avenir Next Condensed", color = "#808080", hjust = 0, vjust = 1,
           size = 4.2, fontface = "italic") +
  scale_x_continuous("Economy Rate (Cumulative)",limits = c(2.5, 4), breaks = seq(2.5,4,0.5)) +
  scale_y_continuous("Average (Cumulative)",limits = c(24, 42), breaks = seq(24,42,2)) +
  scale_colour_gradient(low = "#050833", high = "#3AA3F6") +
  scale_fill_gradient(low = "#050833", high = "#3AA3F6") +
  theme_minimal(base_family = "Avenir Next Condensed") +
  theme(legend.position = "none",
        plot.title = element_text(family = "Avenir Next Condensed", color = "#050833", hjust = 0.2,
                                 size = 24, lineheight = 2 ,face = "bold"),
        panel.grid.major = element_line(colour = "gray92"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 12, margin = margin(10,0,10,0)),
        axis.title.y = element_text(size = 12, margin = margin(0,10,0,0)),
        plot.caption = element_text(size = 10),
        plot.margin = margin(10, 20, 10, 20))

ggsave(here("jimmy_jimmy", "plots", "jimmy_bowling_connect.png"), width = 7.4, height = 9, dpi = 320)

# jimmy_by_match %>% 
#   mutate(wkts_10 = cum_wkts - lag(cum_wkts, 10, default = 0),
#          runs_10 = cum_runs - lag(cum_runs, 10, default = 0),
#          balls_10 = cum_balls - lag(cum_balls, 10, default = 0),
#          avg_10  = round(runs_10 / wkts_10, 2),
#          econ_10 = floor(100 * runs_10 / (balls_10/6))/ 100) %>%  
#   filter(match_no >= 10) %>% 
#   ggplot(aes(x = match_no, y = avg_10)) +
#   geom_line()
