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


# store plots -------------------------------------------------------------


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



# export plots ------------------------------------------------------------


plot_list <- list(econ_growth, pop_growth, debt_service)

for (i in 1:length(plot_list)) {
  ggsave(filename = file.path(imagepath, sprintf("plot%d.png", i)), 
         plot = plot_list[[i]],
         height = 11.5, 
         width = 8, 
         units = "in",
         device = "png")
}



# Experimenting with coups and growth -------------------------------------


# Area plot
econ %>% 
  rename(country = `Country Name`) %>% 
  left_join(coups_subset) %>% 
  mutate(coup_year = 
         y_max = ifelse(econ_growth > 0, econ_growth, 0),
         y_min = ifelse(econ_growth < 0, econ_growth, 0),
         pos_growth = ifelse(econ_growth > 0, "positive", "negative")) %>% 
  ggplot() +
  geom_vline(xintercept = lubridate::year(year_date))+
  geom_col(aes(x = year, y = econ_growth / 100, fill = pos_growth)) +
  facet_wrap(~ country, nrow = 5) 
+
  scale_fill_manual(values = c("#EDBB8A", "#B4C8A8")) +
  theme_line +
  theme(legend.position = "top") +
  labs(x = "", y = "",
       fill = "Direction of economic growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 


# Set a coup lag to get line to show
coups$year_date <- lubridate::ymd(sprintf("%d-01-01", coups$year))

coups_subset %>% 
  ggplot(aes(x = year_date, y = coup_success, group = country)) +
  theme_line + geom_col() +
  facet_wrap(~country, nrow = 5) +
  scale_x_date(breaks = "10 years") 

  

