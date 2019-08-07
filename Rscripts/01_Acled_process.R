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
  group_by(country, event_type, year, admin3) %>%
  summarise(event_count = n()) %>% 
  mutate(Sahel_flag = ifelse(country %in% sahel, 1, 0))






  

mutate(country = fct_reorder(country, event_count)) %>% 
  ggplot(aes(x = year, y = event_count, colour = country)) +
  geom_line() + 
  facet_wrap(~event_type, scales = "free_y") + theme_minimal()
