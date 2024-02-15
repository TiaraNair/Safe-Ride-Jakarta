############################### Report Knowledge ################################

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your data frame is named df
df %>%
  mutate(across(c(ReportKnowledge_Bus, ReportKnowledge_Train, ReportKnowledge_MRT, ReportKnowledge_ERide, ReportKnowledge_Taxi),
                ~ ifelse(. == "Iya", "Yes", ifelse(. == "Tidak", "No", .)))) %>%
  pivot_longer(cols = starts_with("ReportKnowledge_"), names_to = "Transport", values_to = "Response") %>%
  group_by(Transport, Response, .drop = FALSE) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100,
         Transport = factor(Transport, levels = c("ReportKnowledge_Bus", "ReportKnowledge_Train", "ReportKnowledge_MRT", "ReportKnowledge_ERide", "ReportKnowledge_Taxi"),
                            labels = c("Bus", "Kereta", "MRT", "ERide", "Taksi"))) %>%
  ggplot(aes(x = Transport, y = Percentage, fill = Response, label = paste0(round(Percentage, 1), "%"))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "black") +
  scale_fill_manual(values = c("Yes" = "peachpuff2", "No" = "mediumaquamarine")) +
  labs(title = "Knowledge on How to Report Harassment Cases in Public Transport",
       x = "Transport",
       y = "Percentage",
       fill = "Response") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.5))  # Adjust title position

############################## CAUSE PERCEPTIONS #################################

# Combine all "Causes" responses into a single string
causes_responses <- paste(df$Causes, collapse = " ")

# Convert text to lowercase
causes_responses <- tolower(causes_responses)

# Remove punctuation and special characters
causes_responses <- gsub("[[:punct:]]", "", causes_responses)

# Split text into individual words
word_list <- strsplit(causes_responses, "\\s+")

# Flatten the list of words
word_list <- unlist(word_list)

# Count the frequency of each word
word_freq <- table(word_list)

# Sort the word frequency table in descending order
word_freq <- sort(word_freq, decreasing = TRUE)

# Print the most common words or themes
head(word_freq, 100)
