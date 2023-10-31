library(nycflights13)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

data("flights")

# Some cursory looks at the data
ggplot(subset(flights, dep_delay >= -50 & dep_delay <= 100), aes(x = dep_delay)) +
  geom_histogram(binwidth = 5, fill = "#4C72B0", color = "#3C3C3C", alpha = 0.8) +
  xlim(-50, 100) +
  labs(title = "Histogram of dep_delay (-50 to 100)", x = "Departure Delay (minutes)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Which airlines leave early the most
flights |>
  filter(dep_delay < 0) |>
  group_by(carrier) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  ggplot(aes(x = reorder(carrier, count), y = count, fill = carrier)) +
  geom_col(width = 0.6, color = "black") +
  labs(title = "Airlines with Most Early Departures (Negative dep_delay)", 
       x = "Carrier", y = "Count of Negative Departure Delays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none")

# Joining with Airlines data set to get full names of the Carriers
flights_with_names <- flights |>
  inner_join(airlines, by = c("carrier" = "carrier")) |>
  filter(dep_delay < 0) |>
  group_by(name) |>
  summarize(count = n()) |>
  arrange(desc(count))

ggplot(flights_with_names, aes(x = reorder(name, count), y = count, fill = name)) +
  geom_col(width = 0.6, color = "black") +
  labs(title = "Airlines with Most Early Departures (Negative dep_delay)", 
       x = "Carrier", y = "Count of Negative Departure Delays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "none")

# Which Airports have departures traveling the farthest?
top_origins <- flights %>%
  group_by(origin) %>%
  summarize(total_distance = sum(distance)) %>%
  arrange(desc(total_distance)) %>%
  mutate(total_distance = scales::comma(total_distance),
         total_distance = paste0(total_distance, " Miles"))

kable(top_origins, 
      col.names = c("Origin", "Total Distance"), 
      caption = "Top Origins with the Largest Flight Distances in Miles") %>%
  kable_styling(full_width = FALSE)

# Number of flights by Month
flights_by_month <- flights %>%
  group_by(month) %>%
  summarize(number_of_flights = n())

ggplot(flights_by_month, aes(x = month, y = number_of_flights, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Number of Flights by Month",
       x = "Month",
       y = "Number of Flights") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal()





