library(tidyverse)
library(tidytuesdayR)

theme_set(theme_minimal(
  base_size = 12, 
  base_family = "Volvo Antikva"
))

tuesdata <- tidytuesdayR::tt_load("2024-04-23")

data <- tuesdata$outer_space_objects

str(data)

# first plot
data %>% 
  count(Entity, wt = num_objects, sort = TRUE) %>% view()
  slice(1:10) %>% 
  mutate(Entity = fct_reorder(Entity, n)) %>% 
  ggplot(aes(n, Entity, fill = Entity))  +
  geom_col() +
  geom_text(aes(label = n, hjust = -0.05)) +
  scale_x_continuous(limits = c(0,20000))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    title = element_text(size = 16)
  ) +
  labs(title = "Objects launched into outer space - top 10 entities/countries",
       x = "# objects launched into outer space",
       y = element_blank()
       )
  
  
# trend of objects per country per year
data %>% 
  add_count(Entity, wt = num_objects) %>% 
  filter(n >= 40) %>% 
  mutate(Entity = fct_reorder(Entity, -num_objects)) %>% 
  ggplot(aes(Year, num_objects, color = Entity)) +
  geom_line() +
  facet_wrap(~Entity, scales = "free")

## looks like world is a total of other entities
data %>% 
  filter(Entity != "World") %>% 
  count(Year, wt = num_objects) %>% 
  left_join(data %>% filter(Entity == "World") %>% mutate(world = num_objects) %>% select(Year, world)) %>% 
  filter(n != world)

data %>% filter(Year == "1965")
  
  
library(countrycode)

countrycode(origin = data[,"Entity"] %>% unique(), origin = "country.name", destination = "region")

