library(tidyverse)
library(rvest)
library(polite)
library(janitor)
library(gghighlight)
library(hrbrthemes)
library(patchwork)
library(ggtext)
library(here)
library(shadowtext)

eng_spin_1_2 <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;innings_number=1;innings_number=2;orderby=wickets;size=200;team=1;template=results;type=bowling"

eng_spin_1_2_table <- eng_spin_1_2 %>%
  polite::bow() %>%
  polite::scrape() %>%
  html_table(fill = TRUE) %>%
  .[[3]] %>%
  select(-15) %>%
  clean_names()

eng_spin_3_4 <- "https://stats.espncricinfo.com/ci/engine/stats/index.html?bowling_pacespin=2;class=1;filter=advanced;innings_number=3;innings_number=4;orderby=wickets;size=200;team=1;template=results;type=bowling"

eng_spin_3_4_table <- eng_spin_3_4 %>%
  polite::bow() %>%
  polite::scrape() %>%
  html_table(fill = TRUE) %>%
  .[[3]] %>%
  select(-15) %>%
  clean_names()

first_inns <- eng_spin_1_2_table %>% 
  mutate(bowl_ins = 1) %>% 
  mutate(across(c(4:7, 10:14), as.numeric)) %>% 
  filter(wkts >= 20)
         
second_inns <- eng_spin_3_4_table %>% 
  mutate(bowl_ins = 2) %>% 
  mutate(across(c(4:7, 10:14), as.numeric)) %>% 
  filter(wkts >= 20)

both_inns <- add_row(first_inns, second_inns) %>% 
  arrange(player, bowl_ins) %>% 
  add_count(player, name = "featured") %>% 
  filter(featured == 2) %>% 
  mutate(is_jack = player == "MJ Leach")

# data for y-axis labels on right hand side - is there another way to add both left and right labels on y-axis?
y_ax <- tibble(x = 2.1, y = seq(15,50,5))

(p1 <- ggplot(both_inns, aes(x = bowl_ins, y = ave)) +
  geom_line(aes(group = player, colour = is_jack), size = 1) +
  geom_point(aes(fill = is_jack), colour = "white", shape = 21, size = 3) + 
  gghighlight(is_jack, use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("gray80", 0.6))) +
  geom_text(data = y_ax, aes(x = x, y = y, label = y), family = "Avenir Next Condensed",
            colour = "gray20", hjust = -0.1) +
  geom_shadowtext(data = filter(both_inns, is_jack), 
            aes(x = bowl_ins, y = ave, label = ave), family = "Avenir Next Condensed",
              colour = "#ff5349", nudge_x = c(-0.01,0.02), nudge_y = c(1, -1), fontface = "bold",
            bg.colour = "white") +
  scale_colour_manual(values = "#ff5349") +
  scale_fill_manual(values = "#ff5349") +
  scale_x_continuous(limits = c(0.9,2.1), expand = c(0,0), breaks = c(1,2),
                     labels = c("1st Innings", "2nd Innings")) +
  scale_y_continuous(limits = c(12,52), expand = c(0,0), breaks = seq(15, 50, 5)) +
  coord_cartesian(clip = 'off') +
  labs(subtitle = "Bowling Average of England Test Spinners in their 1st and 2nd innings\n(min. 20 wickets in both innings)") +
  theme_ipsum(base_family = "Avenir Next Condensed") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(),
        axis.text.y.left = element_text(),
        axis.text.y.right = element_text(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90", linetype = 1),
        panel.grid.minor.y = element_line(colour = "gray80", linetype = 2),
        plot.subtitle = element_text(hjust = 0),
        plot.title.position = "plot")
)

ave_wide <- both_inns %>% 
  select(player, bowl_ins, ave) %>% 
  pivot_wider(names_from = bowl_ins, values_from = ave, names_prefix = "inns_") %>% 
  mutate(abs_change = inns_2 - inns_1,
         rel_change = abs_change / inns_1,
         player = fct_reorder(player, -abs_change),
         is_jack = player == "MJ Leach",
         lab_pos = if_else(abs_change < 0, 1.7, -1.7)) %>% 
  arrange(abs_change)

(p2 <- ggplot(ave_wide, aes(x = abs_change, y = player)) +
  geom_col(aes(fill = is_jack), show.legend = FALSE) +
  geom_text(aes(label = scales::percent(rel_change, accuracy = 1), 
                x = lab_pos,
                colour = is_jack),
            show.legend = FALSE) +
  gghighlight(is_jack, use_direct_label = FALSE,
              unhighlighted_params = list(fill = "gray90", colour = "gray60")) +
  scale_colour_manual(values = "#ff5349") +
  scale_fill_manual(values = "#ff5349") +
  scale_x_continuous(breaks = seq(-25,10,5)) +
  labs(subtitle = "Difference in their average between the 2 innings (and percentage change)\n ") +
  theme_ipsum(base_family = "Avenir Next Condensed") +
  theme(panel.ontop = TRUE,
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(hjust = 0),
        plot.title.position = "plot")
)

p1 + p2 + 
  plot_layout(widths = c(1, 1.3)) +
  plot_annotation(title = "<span style = 'color:#ff5349;'>**Jack Leach**</span> has been far more penetrative in his 2nd bowling innings in Test Match Cricket ",
                  subtitle = "*Despite taking best 1st innings bowling figures of 4/54 in latest match of India series, the disparity in Jack Leach's bowling average across the 2 innings is still stark*",
                  caption = "Graphic: @committedtotape | Source: espncricinfo",
                  theme = theme(plot.title = ggtext::element_markdown(hjust = 0, family = "Avenir Next Condensed", size = 22, lineheight = 2,
                                                                      margin = margin(10,0,5,0)),
                                plot.subtitle = element_markdown(size = 14, family = "Avenir Next Condensed", margin = margin(10,0,0,0)),
                                plot.caption = element_text(family = "Avenir Next Condensed")))

ggsave(here("jack_leach", "plots", "jack_leach_1v2.png"), width = 12, height = 8, dpi = 320)
