library(tidyverse)
library(ggtext)
library(glue)
library(here)

wins <- tibble(india = 0:4, england = 0:4)

outcomes <- expand(wins, india, england) %>% 
  filter(india + england <= 4) %>% 
  mutate(finalist = case_when(england >= 3 ~ "ENG",
                              england >= india ~ "AUS",
                              india == 1 & england == 0 ~ "AUS",
                              TRUE ~ "IND"))

current_score <- outcomes %>% 
  filter(england == 1, india == 0)

possible_outcomes <- outcomes %>% 
  mutate(not_possible = england < current_score$england |
         india < current_score$india)

col_pal <- c("#ECD42E", "#042B56", "#ADDCE7")

ggplot() +
  geom_tile(data = filter(possible_outcomes, not_possible == 0),
                   aes(x = india, y = england, fill = finalist), 
                   colour = "white") +
  geom_tile(data = filter(possible_outcomes, not_possible == 1),
                   aes(x = india, y = england, fill = finalist), 
                   colour = "white", alpha = 0.5) +
  geom_tile(data = current_score, 
            aes(x = india, y = england, fill = finalist), 
            colour = "white", size = 1.5, linetype = 2) +
  geom_richtext(data = current_score, 
            aes(x = india, y = england, label = "**Current<br>Scoreline**"), 
            fill = NA, label.color = NA, colour = "white", size = 4.5,
            family = "Avenir Next Condensed") +
  geom_richtext(data = current_score, 
                aes(x = 2, y = 0, label = "*Results no longer possible*"), 
                fill = NA, label.color = NA, colour = "gray60", size = 4.5, 
                family = "Avenir Next Condensed") +
  geom_richtext(data = NULL, 
                aes(x = 5.4, y = 3.7, 
                    label = "<span style='color:#042B56'>**England need to win at least 3 of the 4 matches to qualify**</span><br><br>
                             <span style='color:#ECD42E'>**Australia qualify if England win the series less convincingly,<br>the series is drawn, or India win it 1-0**</span><br><br>
                             <span style='color:#ADDCE7'>**India need to claim the series with at least 2 wins**</span>"), 
                fill = NA, label.color = NA, size = 5, 
                family = "Avenir Next Condensed", hjust = 1) +
  scale_fill_manual(values = col_pal) +
  scale_x_continuous("India Wins", breaks = 0:4) +
  scale_y_continuous("England Wins") +
  labs(title = "Who will be joining NZ in the Inaugural World Test Championship Final?",
       subtitle = glue("With the South Africa-Australia Series called off, New Zealand have qualified for the final, but who will join them?
                       The result of the current India-England Series will determine their opponents"),
       caption = "Graphic: @committedtotape | Source: icc-cricket.com") +
  coord_fixed() +
  theme_minimal(base_family = "Avenir Next Condensed") +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 1, size = 22, face = "bold", margin = margin(10,0,20,0)),
        plot.subtitle = element_text(hjust = 1, size = 16, face = "italic", margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 10),
        axis.title.y = element_text(angle = 0, size = 12, face = "bold"),
        axis.title.x = element_text(hjust = 1, size = 12, face = "bold"),
        axis.text = element_text(size = 12))

ggsave(here("world_test_finals","plots","world_test_championship.png"), width = 13, height = 8)
