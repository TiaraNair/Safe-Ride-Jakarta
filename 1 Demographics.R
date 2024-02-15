rm(list=ls()) # remove memory if any

setwd("/Users/tiara/Desktop/Safe Ride")
install.packages("readxl")

# Load the readxl package
library(readxl)

# Specify the file path of your Excel file
file_path <- "/Users/tiara/Desktop/Safe Ride/Survey Responses/SR.xlsx"

# Read the Excel file into R and store it in a data frame
df <- read_excel(file_path)
# Now, survey_data contains the data from your Excel file and it's stored in R permanently

# Remove column #
df <- df[, !(names(df) == "Timestamp")]

# Changing column names #
colnames(df) <- c("Age","Gender","Profession","Bus","Kereta","MRT","ERide","Taksi","Harassment",
                  "Where","What","Emotion","Action","NothingWhy","Intensity","Why","Time","Frequency",
                  "GotHelp","How","Witness","HelpOthers","HelpHow","WhyNot","ReportKnowledge_Bus",
                  "ReportKnowledge_Train", "ReportKnowledge_MRT","ReportKnowledge_ERide",
                  "ReportKnowledge_Taxi","Causes","Concern","Domicile","Citizen")

############# AGE
# Remove "thn" and "Dibawah" from the 'Age' column
df$Age <- gsub("thn", "", df$Age)
df$Age <- gsub("Dibawah", "Under", df$Age)

# Create a vector containing the age group labels
age_groups <- c("Under 17", "18 - 20", "21 -30", "31 - 40", "41 - 60")

# Count the occurrences of each age group label in the data frame
age_group_counts <- sapply(age_groups, function(label) sum(grepl(label, df$Age)))

# Print age counts
sum(age_group_counts)
print(age_group_counts)

# Create a bar chart for age groups
ax <- barplot(age_group_counts,
        main = "Respondents by Age Group",
        xlab = "Age Group",
        ylab = "Number of Respondents",
        col = "lightgreen",
        ylim = c(0, max(age_group_counts) * 1.1),
        names.arg = age_groups,
        border = "black",
        las = 1)  # Rotate x-axis labels horizontally

# Add counts on top of the bars (centered)
text(x = ax, y = age_group_counts,
     labels = age_group_counts, pos = 3, cex = 0.8, col = "black")

############## Gender
df$Gender <- gsub("Laki-Laki", "Male", df$Gender)
df$Gender <- gsub("Perempuan", "Female", df$Gender)
genders <- c("Male", "Female", "-")
gender_counts <- sapply(genders, function(label) sum(grepl(label, df$Gender)))
sum(gender_counts)
print(gender_counts)

# Create a pie chart for genders
pie(gender_counts,
    main = "Respondents by Gender",
    labels = c("121", "327", "1"),
    col = c("skyblue", "pink", "lightgreen"))

# Add a legend
legend("topright", inset = c(0, 0.05), legend = c("Male", "Female", "Other"), 
       fill = c("skyblue", "pink", "lightgreen"))

######## Professions
# Change profession labels
df$Profession <- gsub("Karyawan di Sektor Swasta", "Karyawan Sektor Swasta", df$Profession)
df$Profession <- gsub("Karyawan di Sektor Pemerintah", "Karyawan Sektor Pemerintah", df$Profession)
df$Profession <- gsub("Pegawai di Organisasi Non-Pemerintah", "Pegawai Organisasi Non-Pemerintah", df$Profession)
df$Profession <- gsub("Freelance", "Pekerja Lepas", df$Profession)

# Generate frequency table for professions
pc <- table(df$Profession)
profession_counts <- sort(pc, decreasing = TRUE)
print(profession_counts)

# Create a pie chart for professions
percentages <- round(100 * profession_counts / sum(profession_counts), 1)
library(RColorBrewer)

# Create a pie chart for professions with percentages as labels
par(xpd=TRUE)
pie(profession_counts,
    main = "Respondents by Profession",
    col = brewer.pal(length(profession_counts), "Set2"),
    labels = ''  )

par(xpd=TRUE)
label <- paste(names(profession_counts), " (", percentages, "%)", sep = "")
legend(1.3,0.6,legend = label,cex=0.7, fill = brewer.pal(length(profession_counts), "Set2"), title = "Profession")

########################## DOMICILE

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# Assuming your data frame is named df
df %>%
  mutate(Domicile = tolower(Domicile)) %>%
  mutate(Domicile = case_when(
    Domicile %in% c("jakarta utara", "jakarta barat", "jakarta timur", 
                    "jakarta selatan", "jakarta pusat") ~ Domicile,
    TRUE ~ "Luar Jakarta"
  )) %>%
  ggplot(aes(x = Domicile, fill = Domicile)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_stack(vjust = 1.05), color = "black") +
  scale_fill_manual(values = brewer.pal(6, "YlGnBu")) +
  labs(title = "Distribution of Domicile",
       x = NULL,
       y = "Count",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))

