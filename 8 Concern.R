# Define response labels
response_labels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

# Calculate the counts of each response in the "Concern" column
concern_counts <- table(df$Concern)

# Calculate the percentages
percentages <- round((concern_counts / sum(concern_counts)) * 100, 1)

# Define softer rainbow colors for the pie chart
library(RColorBrewer)
num_colors <- length(response_labels)
pie_colors <- brewer.pal(num_colors, "Pastel1")

# Set the margins to move the title downwards
par(mar = c(5, 4, 6, 2))

# Create a pie chart
pie(concern_counts, 
    labels = NULL,
    col = pie_colors)


# Adding percentages to the legend with adjusted position
legend("bottomright", 
       legend = paste0(response_labels, ": ", percentages, "%"),
       fill = pie_colors,
       bty = "n", 
       cex = 0.8,
       inset = c(-0.15, 0))

title(main ="Proportion of Concern Towards\n Sexual Harassment on Public Transport",cex.main=1,line = -1)

########################### LOGISTIC REGRESSION ################################

# Merge responses 1 and 2 into a single category (1)
df$Witness <- ifelse(df$Witness %in% c(1, 2), 1, df$Witness)

# Create a binary variable for analysis
df$WitnessBinary <- ifelse(df$Witness > 0, 1, 0)

# Create a binary variable for analysis
df$ConcernBinary <- ifelse(df$Concern %in% c(4, 5), 1, 0)

chisq_result <- chisq.test(table(df$WitnessBinary, df$ConcernBinary))
print(chisq_result)



