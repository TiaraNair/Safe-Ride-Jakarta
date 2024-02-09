modes_of_transport <- df[, c("Bus", "Train", "MRT", "ERide", "Taxi")]
total_rows <- nrow(modes_of_transport)

# Calculate the frequency of 0's/non-passengers in each column
non_passengers <- colSums(modes_of_transport == 0)
print(non_passengers)

# Calculate the frequency of non-zero values (i.e., number of people who ride each mode of transport)
passenger_counts <- total_rows - non_passengers
print(passenger_counts)

library(ggplot2)
# Create a dataframe with passenger counts and mode of transport names
passenger_data <- data.frame(Transport = names(passenger_counts), Count = passenger_counts)

# Calculate total passenger counts
total_passengers <- sum(passenger_counts)

# Calculate percentages
passenger_data$Percentage <- passenger_data$Count / total_passengers * 100

# Create donut chart with labels
donut_chart <- ggplot(passenger_data, aes(x = '', y = Percentage, fill = Transport, label = Count)) +
  geom_bar(stat = "identity", width = 1, color = "grey21") +
  geom_text(position = position_stack(vjust = 0.5), size = 4) +  # Add labels to segments
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Bus" = "lightblue", "Train" = "lightgreen", "MRT" = "lightpink1", "ERide" = "burlywood1", "Taxi" = "lightsalmon1")) +
  ggtitle("Passenger Counts by Mode of Transport") +
  theme(plot.title = element_text(hjust=0.5))+
  theme(plot.title = element_text(vjust=-7))+
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10)) +
  theme(legend.position = c(0,0.5))+
  theme(legend.box = "horizontal")

# Display donut chart with labels
print(donut_chart)
