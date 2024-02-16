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

########################## REACTIONS #################################

# Filter rows where Harassment is 'Yes'
harassment_df <- df[df$Harassment == "Yes", ]

# Filter rows in the "Action" column containing the relevant reactions
reaction_df <- df[grepl("Menanggapi secara lisan|Menanggapi dengan bahasa tubuh|Mencari bantuan dari penumpang|
                        Melaporkan kepada polisi atau staf bus/metro/ride-share atau layanan transportasi berbagi|
                        Mengabaikan|Saya tidak melakukan apa-apa", df$Action), ]

# Count the occurrences of each reaction
reaction_counts <- table(reaction_df$Action)

# Convert the reaction counts to a data frame
reaction_counts_df <- as.data.frame(reaction_counts)
colnames(reaction_counts_df) <- c("Action", "Count")

# Sort the reactions by count in descending order
reaction_counts_df <- reaction_counts_df[order(-reaction_counts_df$Count), ]

# Print the unique counts
print(reaction_counts_df)

# Define the counts for each reaction
counts <- c(93,127,77,86,50,39)  # Insert the counts for each reaction here
reactions <- c("Menanggapi secara lisan", "Menanggapi dengan bahasa tubuh", "Mencari bantuan dari penumpang", 
               "Melaporkan kepada polisi, staf, atau layanan transportasi", "Mengabaikan",
               "Saya tidak melakukan apa-apa")

# Create a dataframe
data <- data.frame(Action = reactions, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Action factor variable based on the sorted data
data$Action <- factor(data$Action, levels = data$Action)

library(ggplot2)
# Create a bar plot with descending bars and counts displayed on them for the provided data
ggplot(data, aes(x = Count, y = reorder(Action, Count), fill = Action, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +
  scale_fill_manual(values = c("#86C7C1", "#FFD3B6", "#BFD3C1", "#D4A5A5", "#C3B3B6", "#FFC3A0")) +
  labs(title = "Reactions to Harassment",
       x = "Count",
       y = "Action") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical",  # Make the legend vertical
        plot.title = element_text(hjust = 0.5, vjust = 1.5))

