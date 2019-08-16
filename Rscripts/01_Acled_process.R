# Process Acled data for mapping
# Author: Tim Essam
# Date: 2019_08_06
# Notes:


# Set up colors -----------------------------------------------------------
library(ggmap)


preview_colors <- function(x = "BurgYl", y = 30) {
  # x is the color scheme from carto db
  # y is the number of steps needed in the new color palette
  
  cust_colors <- rcartocolor::carto_pal(7, x)
  nc <- palette(colorRampPalette(cust_colors)(y))
  plot(1:y, 1:y, col = 1:y, pch = 19, cex = 5)
  return(nc)
}

# Cannot get enough color perception in the lower in ends, using viridis
preview_colors("SunsetDark", 30)

nc <- viridis::magma(n = 30, direction = -1, begin = 0.15)

# Old colors 
#custom_pink <- c(#f0c6c3")
#custom_red  <- c(#d46780")

min_nc <- c("#f9cdcf")
max_nc <- nc[15]

palette(colorRampPalette(viridis::magma(n = 30, direction = -1, begin = 0.2))(30)) 
plot(1:30, 1:30, col = 1:30, pch = 19, cex = 5)


# Captions for plots ------------------------------------------------------

acled_caption <- c("Source: Armed Conflict and Event Location Data (ACLED)")
coup_caption <-  c("Source: https://www.jonathanmpowell.com/coup-detat-dataset.html")


# Read and explore data ---------------------------------------------------
acled <- read_csv(file.path(datapath, "1997-01-01-2019-08-06.csv")) %>% 
  filter(str_detect(.$region, "Africa"))

acled %>% names()
acled %>% 
  count(country, event_type) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

sahel <- c("Niger", "Mauritania", "Mali", "Burkina Faso", "Chad", "Sudan", "Eritrea", "South Sudan",
           "Senegal", "Nigeria")

sub_event_filter <- c("Armed clash", "Attack", "Violent demonstration",
                      "Mob violence", "Remote explosive/landmine/IED",
                      "ir/drone strike",
                      "Shelling/artillery/missile attack",
                      "Grenade", "Non-state actor overtakes territory",
                      "Suicide bomb", "Chemical weapon")

conflict <- 
  acled %>% 
  mutate(Sahel_flag = ifelse(country %in% sahel, 1, 0),
         year_filter = case_when(
           year < 2010 ~ "pre-2010",
           year >= 2010 ~ "2010-present"
         ),
         time_variable = lubridate::dmy(event_date),
         event_filter = ifelse(sub_event_type %in% sub_event_filter, 1, 0)) %>% 
  filter(event_filter == 1)

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
conflict_plot <- 
  conflict2 %>% 
  ggplot(aes(x = year, y = country, fill = count)) +
  geom_tile(colour = "white", size = 0.25) +
  scale_fill_gradientn(colours = nc) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1964, 2020)) +
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
       caption = acled_caption, 
       fill = "Number of conflict") 
  #geom_text(aes(label = count, color = color_flag), family = "Lato", size = 3) +
  #scale_color_manual(values = c(grey50K, "white")) 
  #guides(colour = FALSE, fill = FALSE)

ggsave(file.path(imagepath, "Sahel_Acled_events_1997_2019.png"),
       plot = conflict_plot,
       width = 11,
       height = 6.25,
       units = c("in"),
       device = "png",
       dpi = "retina")

# OR what does a timeseries plot look like?

acled_ts <- 
  conflict2 %>% 
  ggplot(aes(x = year, y = count, group = country, color = country)) +
  geom_line() + 
  facet_wrap(~country, nrow = 5, scales = "free_y") + 
  theme_line +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(breaks = seq(1997, 2022, 5)) +
  labs(x = "", y = "", caption = acled_caption,
       title = "Armed Conflict Events Across the Sahel")

ggsave(filename = file.path(imagepath, "ACLED_Sahel_Time_Series.png"), 
       plot = acled_ts,
       height = 8, 
       width = 11.5, 
       units = "in",
       device = "png")  


  
# Read in coup data -------------------------------------------------------
# From: https://www.jonathanmpowell.com/coup-detat-dataset.html

# Create an blank row for 1965 for each country
# This can also be used to add in countries not in the coup database, but in ACLED
# Can enter this by hand/copy paste in Excel and then use R-studio Addins (datapasta) to paste as tibble
pre_coup <- tibble::tribble(
  ~country, ~ccode, ~year, ~month, ~day, ~coup,
  "Burkina Faso",    439,  1965,      1,    1,    NA,
  "Mali",    432,  1965,      1,    1,    NA,
  "Niger",    436,  1965,      1,    1,    NA,
  "Chad",    483,  1965,      1,    1,    NA,
  "Mauritania",    435,  1965,      1,    1,    NA,
  "Burkina Faso",    439,  2020,      1,    1,    NA,
  "Mali",    432,  2020,      1,    1,    NA,
  "Niger",    436,  2020,      1,    1,    NA,
  "Chad",    483,  2020,      1,    1,    NA,
  "Mauritania",    435,  2020,      1,    1,    NA,
  "Burkina Faso",    439,  2016,     10,    8,     1,
  "Senegal", 433, 1965, 1, 1, NA,
  "Sudan",  625, 1965, 1, 1, NA,
  "Nigeria", 475, 1965, 1, 1, NA,
  "Eritrea", NA, 1965, 1, 1, NA,
  "South Sudan", NA, 1965, 1, 1, NA
)


coup_url <- c("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt")
coups <- read_tsv(url(coup_url)) %>% 
  filter(country %in% c("Burkina Faso", "Chad", "Mali", "Mauritania", "Niger",
                        "Senegal", "Sudan", "Nigeria")) %>%
  bind_rows(., pre_coup) %>% 
  complete(country, year = full_seq(year, 1)) %>% 
  mutate(coup_success = ifelse(is.na(coup), 0, coup),
         coup_success = ifelse(country == "Niger" & year %in% c(1976, 1983), 0, coup_success))

coup_plot <- 
coups %>% 
  ggplot(aes(x = year, y = country, fill = factor(coup_success))) +
  geom_tile(colour = "white", size = 0.25) +
  scale_fill_manual(values = c(grey10K, min_nc, max_nc),
                    labels = c("No Coup Attemp", "Unsuccessful Coup", "Successful Coup"),
                    name = "") +
  scale_x_continuous(breaks = seq(1965, 2015, 5), limits = c(1964, 2020)) +
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
       title = "Political instability in the Sahel",
       caption = coup_caption) 

instability <- ggarrange(coup_plot, conflict_plot, nrow = 2,
          align = "v")

ggsave(file.path(imagepath, "Sahel_instability_history.pdf"),
       plot = instability,
       width = 16,
       height = 9,
       units = c("in"),
       device = "pdf",
       dpi = "retina",
       useDingbats = FALSE)


# Acled data processing for Ghana -----------------------------------------

ghana_group <- c("Ghana", "Togo", "Ivory Coast", "Benin", "Burkina Faso")
ghana_acled <- 
  acled %>% 
  mutate(ghana_flag = ifelse(country %in% ghana_group, 1, 0),
         year_filter = case_when(
           year < 2010 ~ "pre-2010",
           year >= 2010 ~ "2010-present"
         ),
         time_variable = lubridate::dmy(event_date)) %>% 
  filter(ghana_flag == 1)

ghana_acled %>% 
  ggplot(aes(x = longitude, y = latitude, colour = event_type, group = country)) + geom_point() +
  facet_wrap(~event_type)

world <- map_data("world")

# Map with poloygons - Cost
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), 
               fill = "#bababa", colour = "#FFFFFF", size = 0.1) +
  geom_point(data = ghana_acled, aes(longitude, latitude, colour = event_type, group = country),
             size = 1, alpha = 0.7)  +
  coord_map("gilbert", xlim = c(-10, 10),
            ylim = c(0, 15)) +
  theme(legend.position = "none") +
  scale_colour_carto_d(palette = "Vivid") +
  facet_wrap(~event_type)
 
