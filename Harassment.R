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

# Plot side-by-side bar chart
ggplot(gender_harassment_df, aes(x = Gender, y = Total, fill = Harassment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total), position = position_dodge(width = 0.9), vjust = -0.45) +  # Add numeric values on top of the bars
  labs(title = "Harassment Experiences by Gender",
       x = "Gender",
       y = "Individuals",
       fill = "Experienced") +
  scale_fill_manual(values = c("Yes" = "darkseagreen3", "No" = "plum3")) +  # Specify custom colors
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

###############