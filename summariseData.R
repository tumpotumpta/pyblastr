# load packages
library(janitor)
library(maps)
library(mapproj)

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
world <- map_data("world") %>%
  arrange(order)

# plot strains on map
map <- ggplot(world, aes(x = long, y = lat), group = group) +
  # geom_path(aes(group = group)) +
  geom_polygon(aes(group = group)) +
  geom_point(data = wb_all, aes(x = lon, y = lat, colour = wheat), size = 1) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  scale_colour_viridis_d(begin = 0.65, end = 0.3) +
  coord_map("mercator", ylim = c(-60, 90)) +
  theme_light() # classic, minimalist, bw
# map
ggsave("map.png", plot = map, width = 20, height = 15, units = "cm", dpi = 300)
