# load packages
library(tidyverse)
library(janitor)
library(maps)

# read raw data from csv created by 'readdata.R'
# specify column types, as default guesses 'link' and 'collector'
# to be of type col_logical(); c = col_character() and d = col_double()
wb_all <- read_csv("rawData.csv", col_types = "cdcccddccccccccc")

# strains by host
host <- wb_all %>% 
  group_by(host_binomial) %>%
  summarise(n = length(labcode)) %>% 
  arrange(-n)

# strains by species
p_py <- wb_all %>% 
  filter(py_binomial != "NA") %>% 
  tabyl(py_binomial) %>% 
  arrange(-n) %>% 
  select(-percent)

# strains by lineage
p_lin <- wb_all %>% 
  filter(py_lineage != "NA") %>% 
  tabyl(py_lineage) %>%  
  arrange(-n) %>% 
  select(-percent)

# ------------------
# map plot
# ------------------

# import country borders
world <- map_data(map = "world",
                  region = ".",
                  exact = FALSE,
                  # projection = "cylindrical",
                  boundary = FALSE,
                  wrap = c(-180, 180),
                  ylim = c(-60, 90)) # 

# plot strains on map
map <- ggplot(world, aes(x = long, y = lat), group = group) +
  geom_path(aes(group = group), size = 0.2) +
  # geom_polygon(aes(group = group)) +
  geom_point(data = wb_all, aes(x = lon, y = lat, fill = wheat), size = 1.5, shape = "circle filled") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  scale_fill_viridis_d(option = "D", begin = 0.35, end = 0.7) +
  labs(title = "Sampling locations of Pyricularia strains compiled in Pyblastr") +
  theme_void() + # void, light, minimalist
  theme(legend.title = element_blank(),
        legend.position = "top")
# map
ggsave("map.png", plot = map, width = 20, height = 10, units = "cm", dpi = 300)
