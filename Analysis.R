# Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(coin)
library(knitr)
library(dunn.test)
library(car)

# Set working directory 
setwd('/Users/amandatran0727/Desktop/EDST 470/Data')

# Data preprocessing ----
# Load the dataframes
data <- read.csv('/Users/amandatran0727/Desktop/EDST 470/Data/Survey Clean.csv')
demographics <- read.csv('/Users/amandatran0727/Desktop/EDST 470/Data/Banner ID - EDST 470 Survey.csv')

# Match demographic data & survey responses + de-identification
data <- data %>%
  inner_join(demographics, by = "Banner.ID" ) %>%
  select(-Banner.ID)

# Function to split response and extract statement and ranking
split_response <- function(response) {
  split_data <- unlist(strsplit(response, split = ";"))
  statements <- gsub("\\d+\\.\\s*", "", split_data)
  return(statements)
}

# Apply the function to each response in the dataframe
data$Statements <- lapply(data$Please.rank.the.following.statements.in.order.of.importance.to.you..1.being.the.most.important.and.6.being.the.least.important...Each.statement.represents.a.personal.goal.related.to.your.education..., split_response)

# Create separate variables for each statement
data$Statement1 <- sapply(data$Statements, function(x) x[1])
data$Statement2 <- sapply(data$Statements, function(x) x[2])
data$Statement3 <- sapply(data$Statements, function(x) x[3])
data$Statement4 <- sapply(data$Statements, function(x) x[4])
data$Statement5 <- sapply(data$Statements, function(x) x[5])
data$Statement6 <- sapply(data$Statements, function(x) x[6])

# Remove the temporary "Statements" column
data <- subset(data, select = -c(Please.rank.the.following.statements.in.order.of.importance.to.you..1.being.the.most.important.and.6.being.the.least.important...Each.statement.represents.a.personal.goal.related.to.your.education...,Statements))


# Define the search terms for all statements (can be broadened)
search_terms <- c(
  "positive impact on society",
  "achieve a high earning potential",
  "continuously learn and grow",
  "gain recognition in my profession",
  "contribute to the representation of people of my race/nationality",
  "mentor and inspire"
)

# Define column names for the search terms
column_names <- c(
  "Positive_Impact",
  "High_Earning",
  "Profession_Growth",
  "Recognition",
  "Representation",
  "Mentorship"
)

# Initialize the new columns
for (col_name in column_names) {
  data[[paste0("G_", col_name)]] <- NA
}

# Iterate over each row
for (i in 1:nrow(data)) {
  # Iterate over each statement column
  for (j in 1:6) {
    statement_column <- paste0("Statement", j)
    # Use grep to search for the search term in each statement
    for (k in seq_along(search_terms)) {
      search_term <- search_terms[k]
      if (any(grepl(search_term, data[i, statement_column]))) {
        col_name <- paste0("G_", column_names[k])
        data[[col_name]][i] <- j
        break  # Exit the loop once found
      }
    }
  }
}

data <- subset(data, select = -c(Statement1, Statement2, Statement3, Statement4, Statement5, Statement6))

data$Citizenship[data$Citizenship == "DU" | data$Citizenship == "US"] <- "R"

# 1 AF
data$Race[data$Race == "AF" | data$Race == "HS"] <- "AFHS"

# Standardize factor variables
data[, 1:32] <- lapply(data[, 1:32], as.factor)


# EDA ----

# Calculate the average for each group
avg_by_race <- aggregate(data[, grep("^G_", names(data))], by = list(Race = data$Race), mean)
avg_by_citizenship <- aggregate(data[, grep("^G_", names(data))], by = list(Citizenship = data$Citizenship), mean)

# Plotting box plots for Positive Impact, High Earning, and Representation
ggplot(data, aes(x = Race, y = G_Positive_Impact)) +
  geom_boxplot(fill = "skyblue") +
  facet_wrap(~ Citizenship) +
  labs(title = "Positive Impact by Race and Citizenship",
       y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Race, y = G_High_Earning)) +
  geom_boxplot(fill = "salmon") +
  facet_wrap(~ Citizenship) +
  labs(title = "High Earning by Race and Citizenship",
       y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Race, y = G_Representation)) +
  geom_boxplot(fill = "orange") +
  facet_wrap(~ Citizenship) +
  labs(title = "Representation by Race and Citizenship",
       y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Research Question 1: COE (Fisher's Exact Test) ----
COEs <- c(
  "Earn.a.good.salary",
  "Have.a.career.that.my.family.values",
  "Get.respect.from.other.people",
  "Get.a.job.that.is.in.high.demand",
  "Do.work.that.excites.me",
  "Increase.my.self.esteem",
  "Be.supported.by.my.friends",
  "Make.me.feel.that.I..fit.in..with.other.people.in.this.field",
  "Be.supported.by.my.family.members",
  "Make.me.feel.that.my.friends.and.or.family.are.proud.of.me"
)

# Race

# Loop through each preference variable and run Fisher's exact test: Validate assumption for a difference
for (variable in COEs) {
  cat("Fisher's Exact Test for variable:", variable, "\n")
  fisher_result <- fisher.test(table(data$Race, data[[variable]]))
  print(fisher_result)
  cat("\n")
}

# Define racial groups for comparison
racial_groups <- list(
  list(group1 = "WH", group2 = "AFHS"),  # WH vs. AFHS
  list(group1 = "AS", group2 = "AFHS"),  # AS vs. AFHS
  list(group1 = "WH", group2 = "AS")     # WH vs. AS
)

# Create an empty table to store the results
results_KW_race <- matrix(nrow = length(COEs), ncol = length(racial_groups) + 1)
colnames(results_KW_race) <- c("COE","WH vs. AFHS", "AS vs. AFHS", "WH vs. AS")

# Loop through each racial group comparison
for (i in 1:length(racial_groups)) {
  group_comparison <- racial_groups[[i]]
  group1 <- group_comparison$group1
  group2 <- group_comparison$group2
  
  # Loop through each preference variable and perform Fisher's exact test
  for (j in 1:length(COEs)) {
    variable <- COEs[j]
    
    # Create contingency table for the preference variable and race for the two groups
    contingency_table <- table(data$Race, data[[variable]])
    contingency_table <- contingency_table[c(group1, group2), ]
    
    # Perform Fisher's exact test
    fisher_result <- fisher.test(contingency_table)
    
    # Store the p-value in the results table
    results_KW_race[j, 1] <- variable
    results_KW_race[j, i + 1] <- fisher_result$p.value
  }
}

# Further validate with formulas like this: sum(data$Race == "AS" & data$Get.a.job.that.is.in.high.demand == "STEM")/sum(data$Race == "AS")

# Citizenship
results_table_ctz <- matrix(nrow = length(COEs), ncol = 2)
colnames(results_table_ctz) <- c("COE", "p-value for NR vs R")

# Loop through each preference variable and perform Fisher's exact test while controlling for 'Citizenship'
for (i in 1:length(COEs)) {
  variable <- COEs[i]
  
  # Create contingency table for the preference variable and citizenship
  contingency_table <- table(data$Citizenship, data[[variable]])
  
  # Perform Fisher's exact test
  fisher_result <- fisher.test(contingency_table)
  
  # Store the p-value in the results table
  results_table_ctz[i, 1] <- variable
  results_table_ctz[i, 2] <- fisher_result$p.value
}


# Research Question 1: SEA, SEE, IN (Kruskal-Wallis Test/Mann-Whitney U Test)----
# Race (Kruskal-Wallis test)
data_q1_race <- select(data, -c(Citizenship,COEs)) 

# List of variables to exclude from the Kruskal-Wallis test
excluded_variables <- c("Race")

VOI_race <- setdiff(names(data_q1_race), excluded_variables)


# Data frame to store the results
results_KW_race <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Data frame to store the pairwise comparison results
pairwise_q1_race_table <- data.frame(Variable = character(), Comparison = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop over each variable of interest and perform the Kruskal-Wallis test
for (variable in VOI_race) {
  # Convert the variable to numeric (assuming it's stored as a factor)
  data_q1_race[[variable]] <- as.numeric(data_q1_race[[variable]])
  # Handle missing values by excluding them from the analysis
  data_clean <- na.omit(data_q1_race)
  # Perform Kruskal-Wallis test
  kruskal_result <- kruskal_test(as.formula(paste(variable, "~ Race")), data = data_clean)
  # Extract test statistic, degrees of freedom, and p-value
  test_statistic <- as.numeric(statistic(kruskal_result))
  df <- length(levels(data_clean$Race)) - 1  # Degrees of freedom calculation
  p_value <- pvalue(kruskal_result)
  # Add results to the data frame
  results_KW_race <- rbind(results_KW_race, data.frame(Variable = variable, Test_Statistic = test_statistic, Degrees_of_Freedom = df, P_Value = p_value))
}
  
  # Perform pairwise comparisons using Wilcoxon rank sum test
  races <- unique(data_clean$Race)
  for (i in 1:(length(races)-1)) {
    for (j in (i+1):length(races)) {
      race1 <- races[i]
      race2 <- races[j]
      subset1 <- data_clean[data_clean$Race == race1, variable]
      subset2 <- data_clean[data_clean$Race == race2, variable]
      test_result <- wilcox.test(subset1, subset2)
      # Extract relevant information and add to the pairwise results table
      comparison <- paste(race1, "vs", race2)
      pairwise_q1_race_table <- rbind(pairwise_q1_race_table, 
                                      data.frame(Variable = variable, 
                                                 Comparison = comparison, 
                                                 Test_Statistic = test_result$statistic, 
                                                 P_Value = test_result$p.value))
    }
  }

  # Descriptive stats table of significant differences: 
  cat_vars <- c(
    "Understanding.or.interpreting.graphs",
    "Discussing.scientific.or.technical.concepts.",
    "STEM.classes.in.your.major",
    "Thinking.about.topics.that.relate.to.STEM",
    "Watching.videos.or.listening.to.podcasts.about.social.sciences..arts..humanities..or.other...non.STEM.topics",
    "G_Positive_Impact",
    "G_High_Earning",
    "G_Profession_Growth",
    "G_Representation",
    "G_Mentorship"
  )
  
  # Remove duplicate values
  cat_vars <- unique(cat_vars)
  
  # Initialize an empty data frame to store results
  results_median_cat <- data.frame(Race = character(), Variable = character(), Median = numeric(), stringsAsFactors = FALSE)
  
  # Loop over each categorical variable of interest
  for (variable in cat_vars) {
    # Calculate the median by grouping by 'Race'
    median_values <- data_q1_race %>%
      group_by(Race) %>%
      summarize(Variable = variable, Median = median(as.numeric(as.factor(.data[[variable]])), na.rm = TRUE), .groups = 'drop')
    
    # Add results to the data frame
    results_median_cat <- rbind(results_median_cat, median_values)
  }

# Citizenship

data_q1_ctz <- select(data, -c(Race,COEs))
excluded_variables2 <- c("Citizenship")
VOI_ctz <- setdiff(names(data_q1_ctz), excluded_variables2)

# Data frame to store the results
results_wilcoxon_ctz <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop over each variable of interest and perform the Wilcoxon rank sum test
for (variable in VOI_ctz) {
  
  if (!is.numeric(data_q1_ctz[[variable]])) {
    data_q1_ctz[[variable]] <- as.numeric(data_q1_ctz[[variable]])
  }
  
  # Perform Wilcoxon rank sum test (Mann-Whitney U test)
  test_result <- wilcox.test(data_q1_ctz[[variable]] ~ data_q1_ctz$Citizenship)
  
  # Extract relevant information and add to the results data frame
  results_wilcoxon_ctz <- rbind(results_wilcoxon_ctz, 
                                data.frame(Variable = variable, 
                                           Test_Statistic = test_result$statistic, 
                                           P_Value = test_result$p.value))
}

# Print the results table
print(results_wilcoxon_ctz)

ctz_vars <- c(
  "Helping.people.understand.the.importance.of.social.sciences.and.or.arts.and.or.humanities.in.their...daily.lives.",
  "Watching.videos.or.listening.to.podcasts.about.social.sciences..arts..humanities..or.other...non.STEM.topics",
  "G_Positive_Impact",
  "G_High_Earning"
)


# Initialize an empty data frame to store results
results_median_ctz <- data.frame(Citizenship = character(), Variable = character(), Median = numeric(), stringsAsFactors = FALSE)

# Loop over each categorical variable of interest
for (variable in ctz_vars) {
  # Calculate the median by grouping by 'Race'
  median_values <- data_q1_ctz %>%
    group_by(Citizenship) %>%
    summarize(Variable = variable, Median = median(as.numeric(as.factor(.data[[variable]])), na.rm = TRUE), .groups = 'drop')
  
  # Add results to the data frame
  results_median_ctz <- rbind(results_median_ctz, median_values)
}

# Research Question 2: ----
data$Earn.a.good.salary <- ifelse(data$Earn.a.good.salary == "STEM", 1, 0)
data$Get.a.job.that.is.in.high.demand<- ifelse(data$Get.a.job.that.is.in.high.demand == "STEM", 1, 0)
# Change reference category for 'Race' and 'Citizenship'
data$Race <- relevel(data$Race, ref = "WH")
data$Citizenship <- relevel(data$Citizenship, ref = "R")

data$Combo <- as.factor(paste(data$Race, data$Citizenship, sep = "_"))
data$Combo <- relevel(data$Combo, ref = "WH_R")


# Fit logistic regression model
model4a <- glm(Earn.a.good.salary ~ Combo, family = binomial(), data = data)
summary(model4a)


model4d <- glm(Get.a.job.that.is.in.high.demand ~ Combo, family = binomial(), data = data)
summary(model4d)

plot(residuals(model4ad) ~ fitted(model4ad))

model4ad <- glm(Get.a.job.that.is.in.high.demand ~ Race, family = binomial(), data = data)
summary(model4ad)

plot(roc(model4ad$y, fitted(model4ad)))

# Extract summary information
summary_data <- summary(model4ad)

# Extract coefficients
coefficients <- summary_data$coefficients

# Extract deviance statistics
deviance_stats <- c(Null_Deviance = summary_data$null.deviance,
                    Residual_Deviance = summary_data$deviance,
                    AIC = AIC(model4ad),
                    BIC = BIC(model4ad))

# Create a data frame for coefficients and deviance statistics
result_data <- data.frame(Coefficient = rownames(coefficients),
                          Estimate = coefficients[, 1],
                          Std_Error = coefficients[, 2],
                          Z_value = coefficients[, 3],
                          P_value = coefficients[, 4])

deviance_df <- data.frame(Coefficient = names(deviance_stats),
                          Value = as.numeric(deviance_stats))



# Export data to a CSV file
write.csv(result_data, file = "glm_results_4a.csv", row.names = FALSE)
