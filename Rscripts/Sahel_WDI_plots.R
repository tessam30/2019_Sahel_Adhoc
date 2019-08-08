# Process Process and plot indicator data
# Author: Tim Essam
# Date: 2019_08_06
# Notes:


# Reusable parts of code --------------------------------------------------

# Set ggplot themes up
theme_line <- theme_xygrid(projector = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0, size = 12)) 

plot_specs <- list(scale_y_continuous(labels = scales::percent_format(accuracy = 1)))
  

# simple function to return the year from the string version of date
return_year <- function(x) {
  as.Date(x, "%Y") %>% lubridate::year()
}

caption <- c("Source: World Development Indicators")

country_colors <- c("#88ccee", "#d17c78", "#94aa5b", "#a25899", "#44aa99")

# Scale the y-axis
scaleFUN <- function(x) sprintf("%.2f", x)


# Loading data ------------------------------------------------------------



econ <- read_csv(file.path(datapath, "Sahel_econgrowthpc.csv"), skip = 4) %>% 
  gather(year, econ_growth, `1960`:`2018`) %>% 
  mutate(year = return_year(year)) %>% 
  arrange(`Country Name`, year)

pop  <- read_csv(file.path(datapath, "Sahel_popgrowth.csv")) %>% 
  gather(year, pop_growth, `1960`:`2018`) %>% 
  mutate(year = return_year(year)) %>% 
  arrange(`Country Name`, year)


debt <- read_csv(file.path(datapath, "Sahel_total_debt_service.csv"), skip = 4) %>% 
  gather(year, debt, `1960`:`2018`) %>% 
  mutate(year = return_year(year)) %>% 
  arrange(`Country Name`, year)

# Function to create ggplots

line_plots <- function(df, x, y) {
  df %>% 
    ggplot(aes(x = {{ x }}, y = {{ y }} / 100, 
               group = `Country Name`, 
               colour = `Country Name`)) +
    geom_line() +
    facet_wrap(~`Country Name`, nrow = 5) +
    theme_line +
    scale_color_manual(values = country_colors) +
    scale_x_continuous(breaks = seq(1960, 2020, 10)) +
    labs(x = "", y = "", caption = caption)
}


econ_growth <- 
  line_plots(econ, year, econ_growth) +
  labs(title = "Economic Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


pop_growth <- 
  line_plots(pop, year, pop_growth) +
  labs(title = "Population Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.05))
  
debt_service <- 
  line_plots(debt, year, debt) +
  labs(title = "Total Debt Services") +
  plot_specs

plot_list <- list(econ_growth, pop_growth, debt_service)

for (i in 1:length(plot_list)) {
  ggsave(filename = file.path(imagepath, sprintf("plot%d.png", i)), 
         plot = plot_list[[i]],
         height = 11.5, 
         width = 8, 
         units = "in",
         device = "png")
}


# Read in coup data -------------------------------------------------------
# From: https://www.jonathanmpowell.com/coup-detat-dataset.html

# Create an blank row for 1965 for each country
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
              "Burkina Faso",    439,  2016,     10,    8,     1
              )


coup_url <- c("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt")
coups <- read_tsv(url(coup_url)) %>% 
  filter(country %in% c("Burkina Faso", "Chad", "Mali", "Mauritania", "Niger")) %>%
  bind_rows(., pre_coup) %>% 
  complete(country, year = full_seq(year, 1)) %>% 
  mutate(coup_success = ifelse(is.na(coup), 0, coup),
         coup_success = ifelse(country == "Niger" & year %in% c(1976, 1983), 0, coup_success))

coups %>% 
  ggplot(aes(x = year, y = country, fill = factor(coup_success))) +
  geom_tile(colour = "white", size = 0.25) +
  scale_fill_manual(values = c(grey10K, "#f0c6c3","#d46780"),
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
       caption = "Source: https://www.jonathanmpowell.com/coup-detat-dataset.html") 


  ggsave(file.path(imagepath, "Sahel_coup_history.png"),
         plot = last_plot(),
         width = 11,
         height = 6.25,
         units = c("in"),
         device = "png",
         dpi = "retina")

  


  

