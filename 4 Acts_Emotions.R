##################### Uncomfortable Actions / Behaviours #######################
# Filter rows in the "Action" column containing the relevant acts
uncomfortable_action_df <- df[grepl("Menatap dengan penuh nafsu|Komentar atau suara yang bersifat seksual|
                                    Menyentuh/meraba-raba|Mendorong tubuh Anda/menggosokkan tubuh|
                                    Seseorang menggaruk area pribadi terutama untuk membuat Anda merasa tidak nyaman", 
                                    df$What), ]

# Count the occurrences of each act
act_counts <- table(uncomfortable_action_df$What)

# Convert the act counts to a data frame
act_counts_df <- as.data.frame(act_counts)
colnames(act_counts_df) <- c("Act", "Count")

# Sort the acts by count in descending order
act_counts_df <- act_counts_df[order(-act_counts_df$Count), ]

# Print the unique counts
print(act_counts_df)

# Define the counts for each act
counts <- c(126,144,76,111,36)  # Insert the counts for each act here
acts <- c("Menatap dengan penuh nafsu", "Komentar atau suara yang bersifat seksual", "Menyentuh/meraba-raba", "Mendorong tubuh Anda/menggosokkan tubuh", "Seseorang menggaruk area pribadi terutama untuk membuat Anda merasa tidak nyaman")

# Create a dataframe
data <- data.frame(Act = acts, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Act factor variable based on the sorted data
data$Act <- factor(data$Act, levels = data$Act)

# Load the ggplot2 library
library(ggplot2)

# Create a horizontal bar plot with descending bars and counts displayed on them
ggplot(data, aes(x = Count, y = reorder(Act, Count), fill = Act, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +  # Add labels to bars
  scale_fill_manual(values = c("#B19CD9", "#A2D2FF", "#FFABAB", "#FFD3B6", "#A0E7E5")) +  # Specify colors manually
  labs(title = "Actions or Behaviours Causing Discomfort in Public Transport",
       x = "Count",
       y = "") +  # Remove the y axis label
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical")  # Make the legend vertical

############################## EMOTIONS ########################################

# Filter rows in the "Emotion" column containing the relevant Emotions
emotions_df <- df[grepl("Kesal|Marah|Jijik|Ketakutan|Merasa dihina|
                        Merasa tidak berdaya|Sedih|Merasa malu atau bersalah|Bingung", 
                        df$Emotion), ]

# Count the occurrences of each act
emotion_counts <- table(emotions_df$Emotion)

# Convert the act counts to a data frame
emotion_counts_df <- as.data.frame(emotion_counts)
colnames(emotion_counts_df) <- c("Emotion", "Count")

# Sort the acts by count in descending order
emotion_counts_df <- emotion_counts_df[order(-emotion_counts_df$Count), ]

# Print the unique counts
print(emotion_counts_df)

# Define the counts for each act
counts <- c(164,182,206,99,67,32,51,27,76)  # Insert the counts for each act here
emotions <- c("Kesal", "Marah", "Jijik", "Ketakutan", "Merasa dihina", "Merasa tidak berdaya", 
              "Sedih", "Merasa malu atau bersalah", "Bingung")

# Create a dataframe
data <- data.frame(Emotion = emotions, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Act factor variable based on the sorted data
data$Emotion <- factor(data$Emotion, levels = data$Emotion)

# Load the ggplot2 library
library(ggplot2)

# Create a horizontal bar plot with descending bars and counts displayed on them
ggplot(data, aes(x = Count, y = reorder(Emotion, Count), fill = Emotion, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +  # Add labels to bars
  scale_fill_manual(values = c("#B19CD9", "#A2D2FF", "#FFABAB", "#FFD3B6", "#A0E7E5", 
                               "#F3B6E3", "#E3F3B6", "#C7CEEA", "#F4C2C2")) +  # Specify colors manually
  labs(title = "Emotions Experienced Following Harassment",
       x = "Count",
       y = "") +  # Remove the y axis label
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical")  # Make the legend vertical
