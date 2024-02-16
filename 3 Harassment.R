################# OCCURENCES AND ITS WHEREABOUTS ######################

# Create a dataframe with the options and corresponding frequencies
places <- c("Kereta", "Peron", "Terminal: tuang luar / dekat pintu masuk & keluar", 
            "Terminal: tempat lainnya", "Bus", "Halte Bus", 
            "Halte Bus: ruang luar / dekat pintu masuk & keluar", 
            "Halte Bus: tempat lainnya", "Di Taksi", 
            "Ride-sharing (GrabCar, GoCar, Maxim Car, etc)", 
            "Sepeda Motor (GrabBike, Gojek, Maxim Ride, etc)", "Pinggir Jalan")
occurrences <- c(99, 39, 54, 35, 84, 47, 47, 33, 29, 30, 19, 73)

where_df <- data.frame(Places = places, Occurrences = occurrences)

library(ggplot2)

# Plot the frequencies using a horizontal bar plot with occurrences labels adjusted
ggplot(where_df, aes(x = Occurrences, y = reorder(Places, Occurrences), fill = Occurrences)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Occurrences), hjust = 1.05, size = 3, color = "black") +  # Adjust hjust parameter
  scale_fill_gradient(low = "#7dd858", high = "#0098b1") +  # Specify the color gradient
  labs(title = "Occurrences of Harassment in Differing Places",
       x = "Occurrences",
       y = "Places") +
  theme_minimal()

############################ GENDER ANALYSIS #################################

library(ggplot2)
# Preprocess data
df$Gender <- factor(df$Gender)
df$Harassment <- ifelse(df$Harassment == "Iya", "Yes", 
                        ifelse(df$Harassment == "Tidak (lanjut ke no.21)", "No", df$Harassment))

# Create a contingency table of Gender and Harassment
gender_harassment_table <- table(df$Gender, df$Harassment)
print(gender_harassment_table)
rowSums(gender_harassment_table)

# Create a dataframe with gender and harassment responses
gender <- c("Male", "Female", "Other")
yes <- c(77, 228, 1)
no <- c(44, 99, 0)
gender_harassment_df <- data.frame(Gender = gender, Yes = yes, No = no)

# Reshape the dataframe for plotting
library(tidyr)
gender_harassment_df <- pivot_longer(gender_harassment_df, cols = c("Yes", "No"), names_to = "Harassment", values_to = "Total")

# Plot side-by-side bar chart with adjusted font size for numeric values
ggplot(gender_harassment_df, aes(x = Gender, y = Total, fill = Harassment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total), position = position_dodge(width = 0.9), vjust = -0.45, size = 3) +  # Adjust font size for numeric values
  labs(title = "Harassment Experiences by Gender",
       x = "Gender",
       y = "Individuals",
       fill = "Experienced") +
  scale_fill_manual(values = c("Yes" = "darkseagreen3", "No" = "plum3")) +  # Specify custom colors
  theme_minimal()

########################### AGE ANALYSIS ######################################
# Create a contingency table of Age and Harassment
age_harassment_table <- table(df$Age, df$Harassment)

# Convert the table to a dataframe
age_harassment_df <- as.data.frame.matrix(age_harassment_table)

# Rename the columns
colnames(age_harassment_df) <- c("No", "Yes")

# Add an additional column for Age_Group
age_harassment_df$Age_Group <- rownames(age_harassment_df)

# Rearrange the rows
age_harassment_df <- age_harassment_df[c(5, 1:4), ]

# Reshape the dataframe for plotting
age_harassment_long <- tidyr::pivot_longer(age_harassment_df, cols = c("No", "Yes"), names_to = "Response")

# Reorder levels of the "Response" variable
age_harassment_long$Response <- factor(age_harassment_long$Response, levels = c("Yes", "No"))

# Define custom colors
custom_colors <- c("#414487", "#2A788E", "#22A884", "#7ad151","#FFD166")

# Create the stacked bar chart with reordered levels and custom colors
ggplot(age_harassment_long, aes(x = value, y = Response, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +  # Adjust the width here
  scale_x_continuous(limits = c(0, 309), breaks = seq(0, 309, by = 50)) +  # Set the x-axis limits and breaks
  geom_text(aes(label = value), position = position_stack(vjust = 0.5), size = 3, color = "white") +  # Add numeric values on top of each bar
  labs(title = "Harassment Experiences by Age Group",
       x = "Amount",
       y = "Response",
       fill = "Age Group") +
  scale_fill_manual(values = custom_colors) +  # Use the custom colors
  theme_minimal()

######################## TRANSPORT USAGE INTENSITY #############################
# Clean the Intensity column
df$Intensity <- ifelse(df$Intensity == "Iya", "Yes",
                       ifelse(df$Intensity == "Tidak", "No", df$Intensity))

# Remove rows where the Harassment column contains "No"
usage_intensity_df <- df[df$Harassment != "No", ]

# Check unique values in the Intensity column
unique_values <- unique(df$Intensity)
print(unique_values)

# Filter out unexpected values if necessary
usage_intensity_df <- usage_intensity_df[usage_intensity_df$Intensity %in% c("Yes", "No"), ]

duplicate_cols <- any(duplicated(names(usage_intensity_df)))

# If there are duplicate column names, rename them
if(duplicate_cols) {
  # Rename duplicate columns with unique names
  names(usage_intensity_df) <- make.unique(names(usage_intensity_df))
}

# Create pie chart
library(ggplot2)

ggplot(data = usage_intensity_df, aes(x = "", fill = Intensity)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Transportation Usage Intensity Impact",
       fill = "Usage Intensity Impact",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("Yes" = "khaki2", "No" = "lightblue2")) +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
  theme_void()

# Combine all "Why" responses into a single string
why_responses <- paste(df$Why, collapse = " ")

# Convert text to lowercase
why_responses <- tolower(why_responses)

# Remove punctuation and special characters
why_responses <- gsub("[[:punct:]]", "", why_responses)

# Split text into individual words
word_list <- strsplit(why_responses, "\\s+")

# Flatten the list of words
word_list <- unlist(word_list)

# Count the frequency of each word
word_freq <- table(word_list)

# Sort the word frequency table in descending order
word_freq <- sort(word_freq, decreasing = TRUE)

# Print the most common words or themes
head(word_freq, 60)

########################## DID NOTHING, WHY #####################################

# Filter rows in the Action column containing "Mengabaikan" or "Saya tidak melakukan apa-apa", including NA reasons
action_df <- df[grepl("Mengabaikan|Saya tidak melakukan apa-apa", df$Action) | is.na(df$Action), ]

# Create a new dataframe with just the "Action" and "NothingWhy" columns
action_nothing_df <- action_df[c("Action", "NothingWhy")]

# Handpick rows to delete
rows_to_delete <- c(5:7,9,28,46,51:55,58,60:62,66,68,69,71:77,80,83,125,127,135,151,
                    152,156,179,186,189,211,224,225,228,242)
# Remove the specified rows from the dataframe
action_nothing_df <- action_nothing_df[-rows_to_delete, ]

rows_to_delete2 <- c(47,56:96,98:102,109:111,113,117:142,144:147,149,151,152,154:175,
                     177:179,181:184,187:191,193:201)
action_nothing_df <- action_nothing_df[-rows_to_delete2, ]
### 21 nothingwhy na

# Count the occurrences of each reason in the "NothingWhy" column
reason_counts <- table(action_nothing_df$NothingWhy)

# Convert the reason counts to a data frame
reason_counts_df <- as.data.frame(reason_counts, stringsAsFactors = FALSE)
colnames(reason_counts_df) <- c("Reason", "Count")

# Sort the reasons by count in descending order
reason_counts_df <- reason_counts_df[order(-reason_counts_df$Count), ]

# Print the unique counts
print(reason_counts_df)

# Print the total count
total_count <- sum(reason_counts_df$Count)
print(paste("Total Count:", total_count))

# Define the counts for each response
counts <- c(5, 8, 11, 14, 23)
reasons <- c("Hal itu tidak lagi mengganggu saya",
             "Hal itu masih mengganggu saya, tapi saya telah belajar untuk hidup dengan itu",
             "Hal itu mengganggu saya, tetapi apa yang bisa saya lakukan seorang diri",
             "Saya takut pada pelaku pelecehan, situasinya bisa menjadi lebih buruk",
             "Saya tidak ingin menghadapi dan membuat keributan")

# Create a dataframe
data <- data.frame(Reason = reasons, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Reason factor variable based on the sorted data
data$Reason <- factor(data$Reason, levels = data$Reason)

# Load the ggplot2 library
library(ggplot2)

# Create a horizontal bar plot with descending bars and counts displayed on them
ggplot(data, aes(x = Count, y = reorder(Reason, Count), fill = Reason, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +  # Add labels to bars
  scale_fill_manual(values = c("#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")) +  # Specify colors manually
  labs(title = "Reasons for Not Taking Action Post-Harassment",
       x = "Count",
       y = "") +  # Remove the y axis label
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical")  # Make the legend vertical

############################# TIME OF OCCURENCE ################################

# Filter rows where Harassment is 'Yes'
harassment_df <- df[df$Harassment == "Yes", ]

# Filter rows in the "Action" column containing the relevant times
time_df <- df[grepl("Pagi|Siang|Sore|Malam", df$Time), ]

# Count the occurrences of each response
time_counts <- table(time_df$Time)

# Convert the act counts to a data frame
time_counts_df <- as.data.frame(time_counts)
colnames(time_counts_df) <- c("Time", "Count")

# Sort the times by count in descending order
time_counts_df <- time_counts_df[order(-time_counts_df$Count), ]

# Print the unique counts
print(time_counts_df)

# Define the counts for each act
counts <- c(52, 94, 172, 101)  # Insert the counts for each act here
times <- c("Pagi", "Siang", "Sore", "Malam")

# Create a dataframe
data <- data.frame(Time = times, Count = counts)

# Sort the data by count in descending order
data <- data[order(-data$Count), ]

# Reorder the levels of the Act factor variable based on the sorted data
data$Time <- factor(data$Time, levels = data$Time)

library(ggplot2)
# Create a bar plot with descending bars and counts displayed on them for the provided data
ggplot(data, aes(x = Count, y = reorder(Time, Count), fill = Time, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = -0.4, size = 3, color = "black") +
  scale_fill_manual(values = c("#FF8C94", "#FFC3A0", "#FFDE8A", "#A2D2FF")) +
  labs(title = "Times When Harassment Occurred",
       x = "Count",
       y = "Time") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove the y axis title
        axis.text.y = element_blank(),   # Remove the y axis text
        legend.position = "right",      # Move the legend to the right
        legend.direction = "vertical",  # Make the legend vertical
        plot.title = element_text(hjust = 0.5, vjust = 1.5))


