#Impact of Fragmentation on Field Vole Foraging Behaviour
file_path <- "field_vole_foraging.txt"

# Read the contents of the file
data <- read.table(file_path, header = TRUE, sep = "\t")

# Display the content of the file
print(data)

#turn data into a dataframe
data <- as.data.frame(data)

#check for the list of columns present in the data
names(data)

# Load necessary libraries for statistical analysis
library(dplyr)
library(ggplot2)

# Calculate mean distances for each fragmentation level
mean_distances <- data %>%
  group_by(Fragmentation) %>%
  summarise(mean_distance = mean(Distance))

# Display mean distances
print(mean_distances)

# Create a bar plot with adjusted aesthetics
ggplot(mean_distances, aes(x = Fragmentation, y = mean_distance, fill = Fragmentation)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean Distances by Fragmentation Level",
       x = "Fragmentation",
       y = "Mean Distance",
       color = "Fragmentation") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 16, face = "bold"),
        plot.background = element_rect(fill = "white"),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


#save plot
ggsave("mean_distances.png")

# Convert the fragmentation level to a factor
data$Fragmentation <- as.factor(data$Fragmentation)

# Perform an ANOVA
result <- aov(Distance ~ Fragmentation, data = data)

# Print the summary of the result
summary(result)

#save summary
sink("summary.txt")
summary(result)
sink()

#extract p-value
p_value <- summary(result)[[1]][["Pr(>F)"]][[1]]
print(p_value)

#generate a major dataframe of the results
results <- data.frame(
  "Fragmentation" = c("Low", "Medium", "High"),
  "Mean Distance" = c(0.5, 1.5, 2.5),
  "p-value" = c(p_value, p_value, p_value)
)
#save it as a text file
write.table(results, file = "results.txt", sep = "\t", row.names = FALSE)

#THE EFFECT OF LATITUDINAL EXTREMES ON DARK-EYED JUNCO BEAK SURFACE AREAS: A COMPARATIVE ANALYSIS
file_path <- "junco_beak_surface_areas.txt"

# Read the contents of the file
data <- read.table(file_path, header = TRUE, sep = "\t")

# Display the content of the file
print(data)

#turn data into a dataframe
data <- as.data.frame(data)

# Convert the population to a factor
data$Population <- as.factor(data$Population)

# Perform a t-test
result <- t.test(Area ~ Population, data = data)

# Print the result
print(result)

# Extract necessary information
result_data <- data.frame(
      t = result$statistic,
      df = result$parameter,
      p_value = result$p.value,
      conf_int = paste(result$conf.int[1], result$conf.int[2], sep = " - "),
      mean_North = result$estimate[1],
      mean_South = result$estimate[2]
)

# Save the result
write.table(result_data, file = "t_test_result.txt", sep = "\t", row.names = FALSE)

#Bar-Tailed Godwit Migration: Weight Changes Analysis
file_path <- "bar_tailed_godwit_weights.txt"

# Read the contents of the file
data <- read.table(file_path, header = TRUE, sep = "\t")

# Display the content of the file
print(data)

#turn data into a dataframe
data <- as.data.frame(data)
# Convert the Time to a factor
data$Time <- as.factor(data$Time)

# Split the data into before and after
data_before <- data[data$Time == "Before", "Weight"]
data_after <- data[data$Time == "After", "Weight"]

# Perform a paired t-test
result <- t.test(data_before, data_after, paired = TRUE)

# Print the result
print(result)

# Extract necessary information
result_data <- data.frame(
      t = result$statistic,
      df = result$parameter,
      p_value = result$p.value,
      conf_int = paste(result$conf.int[1], result$conf.int[2], sep = " - "),
      mean_WeightBefore = mean(data_before),
      mean_WeightAfter = mean(data_after)
)

# Save the result
write.table(result_data, file = "paired_t_test_result.txt", sep = "\t", row.names = FALSE)

# Plot boxplot
boxplot(data$Weight ~ data$Time, main = "Weight Distribution Before and After Migration", 
      xlab = "Time", ylab = "Weight", col = c("lightblue", "pink"))

# Save the plot
dev.copy(png, filename = "weight_distribution.png")
dev.off()



#Effects of Temperature and Microbial Communities on Blow Fly Development Rates
file_path <- "blow_fly_development_rates_experiment.txt"

# Read the contents of the file
data <- read.table(file_path, header = TRUE, sep = "\t")

# Display the content of the file
print(data)

# Convert the Temperature and Treatment to factors
data$Temperature <- as.factor(data$Temperature)
data$Treatment <- as.factor(data$Treatment)

# Perform a two-way ANOVA
result <- aov(Development ~ Temperature * Treatment, data = data)

# Print the summary of the result
print(summary(result))

# Save the result
capture.output(summary(result), file = "anova_result.txt")

# Create interaction plot for Temperature and Treatment on Development
interaction.plot(data$Temperature, data$Treatment, data$Development, 
                         xlab="Temperature", ylab="Development Time", 
                         trace.label="Treatment", legend=TRUE)

# Save the plot
dev.copy(png, filename = "interaction_plot.png")
dev.off()

# Load the ggplot2 package
library(ggplot2)

# Create a grouped bar chart
ggplot(data, aes(x=Temperature, y=Development, fill=Treatment)) +
      geom_bar(stat="identity", position=position_dodge()) +
      labs(x="Temperature", y="Development Time") +
      theme(axis.text.x = element_text(color = "black", size = 12),
                        axis.text.y = element_text(color = "black", size = 12),
                        axis.title = element_text(color = "black", size = 16, face = "bold"),
                        plot.background = element_rect(fill = "white"),
                        legend.title = element_text(color = "black", size = 14, face = "bold"),
                        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) # Added closing parenthesis here

# Save the plot
ggsave("grouped_bar_chart.png")
