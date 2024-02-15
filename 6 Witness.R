################################# WITNESS ######################################

df$Witness <- ifelse(df$Witness == "Iya, sekali.", "1", 
                        ifelse(df$Witness == "Tidak", "0", 
                               ifelse(df$Witness == "Iya, 2-6 kali.", "2", df$Witness)))

# Show the counts of each value in the "Witness" column
witness_counts <- table(df$Witness)
print(witness_counts)

# Define counts
counts <- c(187, 201, 60)

# Create a dataframe
data <- data.frame(Witness = c("Never", "Yes, once", "Yes, 2-6 times"), Count = counts)

# Load ggplot2 library
library(ggplot2)

# Reorder levels of "Witness" factor
data$Witness <- factor(data$Witness, levels = c("Never", "Yes, once", "Yes, 2-6 times"))

# Calculate percentages
data$Percentage <- scales::percent(data$Count / sum(data$Count), accuracy = 0.1)

# Define fill colors
fill_colors <- c("wheat1","pink","rosybrown")

# Visualize the data
ggplot(data, aes(x = Witness, y = Count, fill = Witness)) +
  geom_bar(stat = "identity", width = 0.5,) +
  geom_text(aes(label = Percentage), vjust = -0.5) +
  labs(title = "Counts of Witnesses",
       x = "Witness Level",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = setNames(fill_colors, levels(data$Witness))) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.5))
                    
################################# Help Others ######################################

# Show the counts of each value in the "HelpOthers" column
helpothers_counts <- table(df$HelpOthers)
print(helpothers_counts)

# Define counts
counts <- c(213, 232)

# Create a dataframe
data <- data.frame(Helped = c("Never", "Yes"), Count = counts)

# Load ggplot2 library
library(ggplot2)

# Visualize the data
ggplot(data, aes(x = Helped, y = Count, fill = Helped, label = scales::percent(Count / sum(Count), accuracy = 0.1))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(Count / sum(Count), accuracy = 0.1)), vjust = -0.5) +  # Add percentage labels on bars
  labs(title = "Counts of Helping Others from Harassment",
       x = "Helped Others",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("yellowgreen", "yellow3")) +  # Manually assign fill colors
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.5))  # Adjust title position

#### IF THEY ANSWERED NO IT COULD JUST BE THAT THEY HAVENT SEEN AN OCCURENCE
#### NOT THAT THEY WERENT WILLING TO HELP PR CHOSE NOT TO


################################# WHY NOT ######################################

# Filter rows with NA in HelpHow column
na_rows <- is.na(df$HelpHow)
helphow_filtered <- df[na_rows, ]

# Combine all "WhyNot" responses into a single string
whynot_responses <- paste(df_filtered$WhyNot, collapse = " ")

# Convert text to lowercase
whynot_responses <- tolower(whynot_responses)

# Remove punctuation and special characters
whynot_responses <- gsub("[[:punct:]]", "", whynot_responses)

# Split text into individual words
word_list <- strsplit(whynot_responses, "\\s+")

# Flatten the list of words
word_list <- unlist(word_list)

# Count the frequency of each word
word_freq <- table(word_list)

# Sort the word frequency table in descending order
word_freq <- sort(word_freq, decreasing = TRUE)

# Print the most common words or themes
head(word_freq, 60)
