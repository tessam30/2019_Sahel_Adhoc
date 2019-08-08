# Process Acled data for mapping
# Author: Tim Essam
# Date: 2019_08_06
# Notes:


# Read and explore data ---------------------------------------------------


acled <- read_csv(file.path(datapath, "1997-01-01-2019-08-06.csv")) %>% 
  filter(str_detect(.$region, "Africa"))

acled %>% names()
acled %>% 
  count(country, event_type) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

sahel <- c("Niger", "Mauritania", "Mali", "Burkina Faso", "Chad")

conflict <- 
  acled %>% 
  mutate(Sahel_flag = ifelse(country %in% sahel, 1, 0),
         year_filter = case_when(
           year < 2010 ~ "pre-2010",
           year >= 2010 ~ "2010-present"
         ))


write_csv(conflict, file.path(dataout, "ACLED_conflict_Africa.csv"))
write_csv(conflict %>% filter(Sahel_flag == 1), file.path(dataout, "ACLED_conflict_Sahel.csv"))

conflict2 <- 
  conflict %>% 
  filter(Sahel_flag == 1) %>% 
  group_by(year, country) %>% 
  summarise(count = n()) %>% 
  mutate(color_flag = ifelse(count >100, grey10K, grey80K)) %>% 
  ungroup()

# Heatmap of acled events
conflict2 %>% 
  ggplot(aes(x = year, y = country, fill = count)) +
  geom_tile(colour = "white", size = 0.25) +
  scale_fill_viridis_c(option = "A", direction = -1, alpha = 1,
                       name = "Conflict events")  +
  scale_x_continuous(breaks = seq(1997, 2020, 5)) +
  theme_xygrid() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        plot.margin = margin(0, -5, 0, 0),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_line(),
        # Moving the country titles closer to graph, not sure this is the best approach
        axis.text.y.left = element_text(margin = margin(r = -20)),
        text = element_text(size = 14,  family = "Lato"), 
        plot.title = element_text(size = 14, family = "Lato", colour = grey60K),
        plot.caption = element_text(size = 10, family = "Lato", colour = grey60K,
                                    hjust = 0.9)) +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "",
       title = "Conflict events in the Sahel",
       caption = "Armed Conflict and Event Location Data (ACLED)") +
  geom_text(aes(label = count, color = color_flag), family = "Lato", size = 3) +
  scale_color_manual(values = c(grey50K, "white")) +
  guides(colour = FALSE, fill = FALSE)

ggsave(file.path(imagepath, "Sahel_Acled_events_1997_2019.png"),
       plot = last_plot(),
       width = 11,
       height = 6.25,
       units = c("in"),
       device = "png",
       dpi = "retina")

# OR what does a timeseries plot look like?

conflict2 %>% 
  ggplot(aes(x = year, y = count, group = country, color = country)) +
  geom_line() + 
  facet_wrap(~country, nrow = 5, scale = "free_y") + 
  theme_line +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(breaks = seq(1997, 2022, 5)) +
  labs(x = "", y = "", caption = "Armed Conflict and Event Location Data (ACLED)",
       title = "Armed Conflict Events Across the Sahel")

ggsave(filename = file.path(imagepath, "ACLED_Sahel_Time_Series.png"), 
       plot = last_plot(),
       height = 8, 
       width = 11.5, 
       units = "in",
       device = "png")  


  

