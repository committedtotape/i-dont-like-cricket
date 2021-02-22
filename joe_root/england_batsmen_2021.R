library(tidyverse)
library(here)
library(janitor)
library(polite)
library(rvest)
library(ggforce)
library(glue)
library(ggtext)
library(ggrepel)
library(shadowtext)
library(hrbrthemes)

players <- tribble(~player, ~id,
                   "Root", 303669,
                   "Bairstow", 297433,
                   "Buttler", 308967,
                   "Burns", 398778,
                   "Crawley", 665053,
                   "Stokes", 311158,
                   "Pope", 887207,
                   "Sibley", 519082,
                   "Lawrence", 641423)

url_scrape <- function (player_url) {
  player_url %>% 
    polite::bow() %>% 
    polite::scrape() %>% 
    html_table(fill = TRUE) %>% 
    .[[4]] %>% 
    select(1, 4) %>% 
    clean_names()
}

#test <- map_dfr(players_url$player_url, url_scrape)

players_data <- players %>%
  mutate(player_url = glue("https://stats.espncricinfo.com/ci/engine/player/{id}.html?class=1;template=results;type=batting;view=cumulative")) %>%
  mutate(player_data = map(player_url, url_scrape)) %>%
  unnest(player_data)

# url <- "https://stats.espncricinfo.com/ci/engine/player/308967.html?class=1;template=results;type=batting;view=cumulative"
# 
# url_table <- url %>%
#   polite::bow() %>%
#   polite::scrape() %>%
#   html_table(fill = TRUE) %>%
#   .[[4]] %>%
#   select(-14,-17) %>%
#   clean_names()

players_data_filter <- players_data %>% 
  filter(!player %in% c("Lawrence", "Buttler"))

labels <- players_data_filter %>% 
  group_by(player) %>% 
  filter(mat == max(mat))

col_pal <- c(
"Root" = "#D55E00",
"Bairstow" = "#CC79A7",
"Buttler" = "#0072B2",
"Burns" = "#009E73",
"Crawley" = "#56B4E9",
"Stokes" = "#0072B2",
"Pope" = "#E69F00",
"Sibley" = "#000000")

ggplot(players_data_filter, aes(mat, runs, colour = player)) +
  geom_line() +
  geom_shadowtext(data = labels,
                   aes(label = player),
                  nudge_x = 0.5,
                  hjust = 0,
                  bg.colour = "white",
                  family = "Avenir Next Condensed Bold") +
  geom_point(data = labels, aes(fill = player), colour = "white", shape = 21, size = 2) +
  scale_x_continuous("Matches Played", breaks = seq(0,100,20)) +
  scale_y_continuous("Runs (Cumulative)", breaks = seq(0, 8000, 1000), labels = scales::comma) +
  scale_colour_manual(values = col_pal) +
  scale_fill_manual(values = col_pal) +
  labs(title = "Reliance on <span style = 'color:#D55E00;'>**Root**</span>",
       subtitle = "Cumulative Test Runs for the England Batsmen most likely to feature in 3rd Test (min. 10 matches)<br>
       <span style = 'color:#CC79A7;'>**Bairstow**</span> looks set to replace the inexperienced Lawrence, and <span style = 'color:#56B4E9;'>**Crawley**</span> could be in line to replace <span style = 'color:#009E73;'>**Burns**</span> who has struggled for runs recently",
       caption = "Graphic: @committedtotape | Source: espncricinfo") +
  theme_ipsum(base_family = "Avenir Next Condensed") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_line(colour = "gray95"),
        plot.title = element_markdown(size = 24),
        plot.subtitle = element_markdown(size = 15, lineheight = 1.4),
        plot.title.position = "plot")


# Burns 78 runs in last 5 matches since 18th match

ggsave(here("joe_root", "plots", "reliance_on_root.png"), width = 11, height = 8, dpi = 320)
