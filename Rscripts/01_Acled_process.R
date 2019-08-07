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

conflict %>% 
  group_by(year, country, event_type) %>% 
  summarise(count = n()) %>% 
  spread(year, count)



  

