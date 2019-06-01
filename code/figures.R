# Figures: Summarizing the Bikeshare Dataset


# We'd like to make a map of the United States where currently operating 
# bikeshare programs are plotted as dots, with the size of the system 
# mapped to the size of the dot and the type of the system mapped to the color 
# of the dot, as well as a table giving basic summary statistics on the data.

# Loading libraries and data
library(tidyverse)
library(maps)
load("data/bikeshare_raw.Rda")

# We need to transform `bikeshare_raw.Rda` such that every row is a unique 
# program and docking type (either docked or dockless), with the associated 
# count attached to each.

# first, group the observations up into unique program ids and types
bikeshare_sum <- bikeshare_raw %>%
  group_by(program_id, type) %>%
  summarize(n = n(),
            mean_lon = mean(lon),
            mean_lat = mean(lat)) %>%
  # only include the continental U.S.
  filter(mean_lat > 26 & mean_lat < 50) %>%
  filter(mean_lon > -125 & mean_lon < -68)

# now, we want systems that have both docked and dockless bikes
# to be grouped together
bikeshare_sum <- bikeshare_sum %>%
  group_by(program_id) %>%
  summarize(count_type = case_when(n() == 1 ~ type[1],
                                   n() > 1 ~ "Both"),
            count = sum(n),
            mean_lon = mean(mean_lon),
            mean_lat = mean(mean_lat))

bikeshare_sum$count_type[bikeshare_sum$count_type == "station"] <- "Docked"
bikeshare_sum$count_type[bikeshare_sum$count_type == "bike"] <- "Dockless"

bikeshare_sum$count_type <- as.factor(bikeshare_sum$count_type)

bikeshare_sum$count_type <- fct_relevel(bikeshare_sum$count_type,
                                        c("Docked", "Both", "Dockless"))

head(bikeshare_sum)

#Now for plotting using `ggplot` and `usmap`!

us <- map_data("state")

# only include the continental U.S.
us_bikes <- ggplot() + 
  geom_polygon(data = us,
               mapping = aes(x = long,
                             y = lat,
                             group = group),
               fill = "white",
               color = "black",
               size = 0.1) + 
  theme_minimal() +
  guides(fill = FALSE,
         size = guide_legend(title = "Number of Bikes"),
         color = guide_legend(title = "Storage Type")) +
  geom_point(data = bikeshare_sum,
             aes(x = mean_lon, 
                 y = mean_lat,
                 size = count,
                 col = count_type),
             alpha = .65) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank()) +
  #labs(x = "", y = "") +
  scale_size_continuous(breaks = c(100, 500, 1000)) +
  scale_color_manual(values = c("#8d44c4", "#44bdc4", "#7dc444")) +
  theme(legend.direction = "horizontal", legend.position = "bottom",
       legend.box = "vertical", legend.spacing.y = unit(-.5, "lines"),
       legend.box.margin = margin(t = -30),
       text = element_text(family = "serif"))

us_bikes

ggsave(plot = us_bikes,
       filename = "data/us_bikes.png",
       width = 6, height = 4, units = "in")

# We'd also like to make a table giving some basic summaries of the data.

# for one, we want stats on the number and size of systems
bikeshare_sum_systems <- bikeshare_raw %>%
  uncount(num_bikes_available) %>%
  group_by(program_id, type) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(type) %>%
  summarize(system_count = n(),
            pct_25 = quantile(n, .25),
            pct_50 = quantile(n, .50),
            pct_75 = quantile(n, .75))

# we also want the number of free and stationed bikes
bikeshare_sum_bikes <- bikeshare_raw %>%
  uncount(num_bikes_available) %>%
  group_by(type) %>%
  summarize(bike_count = n())

# join these tables!
bikeshare_table <- cbind(bikeshare_sum_bikes, bikeshare_sum_systems[2:5])

bikeshare_table$type <- c("Dockless", "Docked")

# pretty things up a bit
colnames(bikeshare_table) <- c("Storage Type", "Bike Count", "Number of Systems", 
                               "Percentile", "Bikes Per", "System")


bikeshare_table <- rbind(c("", "",
                               "",
                               "25%", 
                               "50%",
                               "75%"),
                         bikeshare_table)

# naturally, we would like to quantify whether the mean
# bikes per system is different between docked and dockless
bikeshare_sum_t <- bikeshare_raw %>%
  uncount(num_bikes_available) %>%
  group_by(program_id, type) %>%
  summarize(mean_n = mean(n()))
  
t.test(mean_n ~ type, bikeshare_sum_t)
