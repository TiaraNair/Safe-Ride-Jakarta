########################### RECEIVED HELP ######################################

# Preprocess data
df$Harassment <- ifelse(df$Harassment == "Iya", "Yes", 
                        ifelse(df$Harassment == "Tidak (lanjut ke no.21)", "No", df$Harassment))
df$GotHelp <- ifelse(df$GotHelp == "Iya", "Yes", 
                        ifelse(df$GotHelp == "Tidak", "No", df$GotHelp))

# Filter the dataframe to exclude rows where Harassment is "No"
filtered_harassment <- df[df$Harassment == "Yes", ]

# Count the number of people who received help (GotHelp == "Yes") and the number who didn't (GotHelp == "No")
got_help_counts <- table(filtered_harassment$GotHelp)

# Print the counts
print(got_help_counts)

library(ggplot2)
# Convert counts to a dataframe
got_help_counts_df <- as.data.frame(got_help_counts)
names(got_help_counts_df) <- c("GotHelp", "Count")

# Calculate percentages
got_help_counts_df$Percentage <- got_help_counts_df$Count / sum(got_help_counts_df$Count) * 100

# Create a pie chart
pie_chart <- ggplot(got_help_counts_df, aes(x = "", y = Count, fill = GotHelp)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(fill = "Got Help") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("darkseagreen3", "lightgoldenrod")) +  # Change colors
  theme_void() +
  ggtitle("Proportion of People who Received Help\n Post-Harassment on Public Transport") +  # Add title
  theme(plot.title = element_text(hjust = 0.5))  # Center title
print(pie_chart)

########################### RECEIVED HELP HOW??? ##################################

# Filter the dataframe to include only rows where GotHelp is "Yes"
filtered_gothelp <- df[df$GotHelp == "Yes", ]

# Explore how these people received help using the "How" column
help_method <- table(filtered_gothelp$How)

# Filter rows in the "How" column containing the relevant Methods
helpmethod_df <- filtered_gothelp[grepl("Memanggil Polisi|Konfrontasi dengan Pelaku Pelecehan|Mendapat Dukungan dari Orang-orang di Sekeliling", filtered_gothelp$How), ]

# Count the occurrences of each act
helpmethod_counts <- table(helpmethod_df$How)

# Convert the act counts to a data frame
method_counts_df <- as.data.frame(helpmethod_counts)
colnames(method_counts_df) <- c("Method", "Counts")  # Rename to include "Counts"

# Print the unique counts
print(method_counts_df)

# Define the counts for each method
counts <- c(61, 92, 113)  # Insert the counts for each method here
methods <- c("Memanggil Polisi", "Konfrontasi dengan Pelaku Pelecehan", "Mendapat Dukungan dari Orang-orang di Sekeliling")

# Create a dataframe
data <- data.frame(Method = methods, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Method factor variable based on the sorted data
data$Method <- factor(data$Method, levels = data$Method)

# Load the ggplot2 library
library(ggplot2)

# Create a horizontal bar plot with descending bars and counts displayed on them
ggplot(data, aes(x = Count, y = reorder(Method, Count), fill = Method, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +  # Add labels to bars
  scale_fill_manual(values = c("steelblue1","palevioletred2","mediumpurple2")) +  # Specify colors manually
  labs(title = "Methods of Help Received Following Harassment",
       x = "Count",
       y = "") +  # Remove the y axis label
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical",  # Make the legend vertical
        plot.title = element_text(hjust = 0.5, vjust = 1.5))  # Adjust title position
