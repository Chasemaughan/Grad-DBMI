# Chase Maughan
# 
# u0962361



# Risk Factors Associated with Low Infant Birth Weight Dataset Analysis
# 
# Dataset Information
# 
# The dataset used for this analysis is from the study "Risk Factors Associated with Low Infant Birth Weight" by Hosmer, D.W. and Lemeshow, S. (1989) [^1].
# 
# A. How many observations are in the dataset?
  
  # {r}
# Use file.choose() to interactively choose a file
current_directory <- getwd()

# Print the path of the selected file
print(current_directory)

# {r}
# Load the dataset
birthwt <- "HW2/birthwt.txt"
birth_data <- read.table(birthwt, header = TRUE)

# Print the number of observations
num_observations <- nrow(birth_data)
num_observations

# B.

# Examine the features, determine what type of variable each represents, and indicate whether each one is discrete or continuous. Then, go on to determine the distribution or descriptive statistics as appropriate: #
  
# low: Indicator variable of birth weight less than 2.5 kg. Discrete variable.
# 
# age: Mother's age in years. Continuous variable.
# 
# lwt: Mother's weight in pounds at the last menstrual period. Continuous variable.
# 
# race: Mother's race. Discrete variable.
# 
# smoke: Smoking status during pregnancy. Discrete variable.
# 
# ptl: Number of previous premature labors. Discrete variable.
# 
# ht: History of hypertension. Discrete variable.
# 
# ui: Presence of uterine irritability. Discrete variable.
# 
# ftv: Number of physician visits during the first trimester. Discrete variable.
# 
# bwt: Birth weight in grams. Continuous variable.
# 
# {r}

discrete_vars <- c("low", "race", "smoke", "ptl", "ht", "ui", "ftv")
continuous_vars <- c("age", "lwt", "bwt")

for (var in discrete_vars) {
  cat("\nDiscrete Variable:", var, "\n")
  
  # Check if binary OR Normal
  
  if (length(unique(birth_data[[var]])) == 2) {
    cat("Type: Binary\n")
  } else {
    # Check if nominal or ordinal
    if (is.factor(birth_data[[var]])) {
      cat("Type: Nominal\n")
    } else {
      cat("Type: Ordinal\n")
    }
  }
  
  #number of levels
  cat("Levels:", length(unique(birth_data[[var]])), "\n")
}

#Continuous Variables
for (var in continuous_vars) {
  cat("\nContinuous Variable:", var, "\n")
  
  # Calculate mean, standard deviation, and median
  mean_val <- mean(birth_data[[var]])
  sd_val <- sd(birth_data[[var]])
  median_val <- median(birth_data[[var]])
  
  cat("Mean:", mean_val, "\n")
  cat("Standard Deviation:", sd_val, "\n")
  cat("Median:", median_val, "\n")
}

# Risk Factors Associated with Low Infant Birth Weight Dataset Analysis
# 
# Dataset Information
# 
# The dataset used for this analysis is from the study "Risk Factors Associated with Low Infant Birth Weight" by Hosmer, D.W. and Lemeshow, S. (1989) [^1].
# # 
# # A. How many observations are in the dataset?
# # 
# # {r}
# Use file.choose() to interactively choose a file
current_directory <- getwd()

# Print the path of the selected file
print(current_directory)
# 
# {r}
# Load the dataset
birthwt <- "HW2/birthwt.txt"
birth_data <- read.table(birthwt, header = TRUE)

# Print the number of observations
num_observations <- nrow(birth_data)
num_observations
# 
# B.

# Examine the features, determine what type of variable each represents, and indicate whether each one is discrete or continuous. Then, go on to determine the distribution or descriptive statistics as appropriate: #
# 
# low: Indicator variable of birth weight less than 2.5 kg. Discrete variable.
# 
# age: Mother's age in years. Continuous variable.
# 
# lwt: Mother's weight in pounds at the last menstrual period. Continuous variable.
# 
# race: Mother's race. Discrete variable.
# 
# smoke: Smoking status during pregnancy. Discrete variable.
# 
# ptl: Number of previous premature labors. Discrete variable.
# 
# ht: History of hypertension. Discrete variable.
# 
# ui: Presence of uterine irritability. Discrete variable.
# 
# ftv: Number of physician visits during the first trimester. Discrete variable.
# 
# bwt: Birth weight in grams. Continuous variable.
# 
# {r}
discrete_vars <- c("low", "race", "smoke", "ptl", "ht", "ui", "ftv")
continuous_vars <- c("age", "lwt", "bwt")

for (var in discrete_vars) {
  cat("\nDiscrete Variable:", var, "\n")
  
  # Check if binary OR Normal
  
  if (length(unique(birth_data[[var]])) == 2) {
    cat("Type: Binary\n")
  } else {
    # Check if nominal or ordinal
    if (is.factor(birth_data[[var]])) {
      cat("Type: Nominal\n")
    } else {
      cat("Type: Ordinal\n")
    }
  }
  
  #number of levels
  cat("Levels:", length(unique(birth_data[[var]])), "\n")
}

#Continuous Variables
for (var in continuous_vars) {
  cat("\nContinuous Variable:", var, "\n")
  
  # Calculate mean, standard deviation, and median
  mean_val <- mean(birth_data[[var]])
  sd_val <- sd(birth_data[[var]])
  median_val <- median(birth_data[[var]])
  
  cat("Mean:", mean_val, "\n")
  cat("Standard Deviation:", sd_val, "\n")
  cat("Median:", median_val, "\n")
}

# C. How many individuals older than 30 smoke?
# 
# {r}
OlderSmokers <- birth_data[birth_data$age > 30 & birth_data$smoke == 1, ]
nrow(OlderSmokers)
# 
# 1. D. Plot a histogram for birth weight
# 
# {r}
hist(birth_data$bwt, 
     main = "Histogram of Birth Weight",
     xlab = "Birth Weight",
     ylab = "Frequency",
     breaks = 20
)
# 
# G. Calculate the probability of randomly selecting an individual that has either a low birth weight or a mother who was a smoker.
# 
# {r}
# Probability of low birth weight
prob_low_birth_weight <- sum(birth_data$low == 1) / nrow(birth_data)

# Probability of mother being a smoker
prob_mother_smoker <- sum(birth_data$smoke == 1) / nrow(birth_data)

# Probability of both events happening (low birth weight and mother being a smoker)
prob_both <- sum(birth_data$low == 1 & birth_data$smoke == 1) / nrow(birth_data)

# Probability of either event happening
prob_either <- prob_low_birth_weight + prob_mother_smoker - prob_both

# Print the result
print(prob_either)

# H. Calculate the probability of randomly selecting an individual that is white and has more than 3 physician visits during the first trimester.
# 
# {r}
#Probability of both events happening
prob_both <- sum(birth_data$race == 1 & birth_data$ftv > 3) / nrow(birth_data)
print(prob_both)

# Probability Questions/Breast Cancer Naive
# 
# A. What is the probability that given a positive mammogram exam, a woman has a positive cancer diagnosis? Assume that the breast cancer incidence rate is 1%, the positivity rate for the exam if a patient has cancer is 90%, and there is a false positive rate of 8% for the exam. 
# 
# {r}
sensitivity <- 0.9  
cancer_rate <- 0.01  
false_positive_rate <- 0.08
no_cancer_rate <- 1 - cancer_rate

#Positive Rate
positive_rate <- sensitivity * cancer_rate + false_positive_rate * no_cancer_rate

#Prob of Cancer given positive
prob_cancer <- (sensitivity * cancer_rate) / positive_rate


print(prob_cancer)


# B. For every attempt to call your friend, there is a 70% probability of actually speaking with them. Calculate the probability of having exactly 12 successes in 20 attempts. 
# 
# {r}

n <- 20  # attempts
k <- 12  # successes
p <- 0.7 # probability

# binomial coefficient
b_c <- choose(n, k)

# binomial probability
b_prob <- b_c * p^k * (1 - p)^(n - k)


print(b_prob)


# C. If a sample of test scores is normally distributed with a mean of 42 and a standard deviation of 8, what percent of the scores is: 
# 
# {r}
mean <- 42
std_dev <- 8

# (i) Greater than 25?
# 
# {r}
#Calc z score
z_25 <- (25 - mean) / std_dev

#use z score and normal distribution to solve
prob_greater_25 <- pnorm(z_25, lower.tail = FALSE)
print(prob_greater_25)

#  (ii) Smaller than 31?
# 
# {r}
z_31 <- (31 - mean) / std_dev

#use z score and normal distribution to solve
prob_less_31 <- pnorm(z_31)
print(prob_less_31)

# (iii) Between 25 and 31?
# 
# {r}

z_25 <- (25 - mean) / std_dev
z_31 <- (31 - mean) / std_dev

#use z score and normal distribution to solve
prob_between_25_and_31 <- pnorm(z_31) - pnorm(z_25)

# Output the result
print(prob_between_25_and_31)


# 3. Naïve Bayes classifier
# 
#  For this exercise we are going to build a Naïve Bayes classifier to try to predict benign/malignant from measurements taken from breast mass using characteristics of cell nuclei taken from digitized. The dataset should be available on canvas
# 
# {r}

library(naivebayes)


file_path <- "HW2/Breast_cancer_Naive.csv"
breast_cancer_data <- read.csv(file_path)


# Preprocess the data
breast_cancer_data$diagnosis <- as.factor(breast_cancer_data$diagnosis)

# Split the data into training and testing sets
set.seed(22)
split_index <- createDataPartition(breast_cancer_data$diagnosis, p = 0.7, list = FALSE)
train_data <- breast_cancer_data[split_index, ]
test_data <- breast_cancer_data[-split_index, ]

# Build Naïve Bayes model
model <- naive_bayes(train_data[, -1], train_data$diagnosis)

# Make predictions on the test set
predictions <- predict(model, test_data[, -1])

# Evaluate model performance
confusion_matrix <- table(Actual = test_data$diagnosis, Predicted = predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Display results
print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy: ", accuracy))
  
  
# 
# Explain the performance of your classifier.
# 
# I found the initial logic for the Naive Bayes classifier online, built from the breast cancer data provided with the homework and the Naive Bayes library, the data is split into a training and test data sets to check how accurate the classifier is at predicting diagnosis.
# 
# According to the Confusion Matrix generated, we can see how accurate the classifier is.
# 
# True Positives (TP): 61 malignant cases correctly predicted as malignant.
# 
# True Negatives (TN): 101 benign cases correctly predicted as benign.
# 
# False Positives (FP): 6 benign cases incorrectly predicted as malignant.
# 
# False Negatives (FN): 2 malignant cases incorrectly predicted as benign.
# 
# We also can see that it is Accurate about 95% of the time based off of data given.
# 
# Investigate a bit about different types of naïve bayes classifiers, using that information what could you change that might improve the performance of this classifier for this dataset?
# 
# Here are some other forms of Naive Bayes Classifers that are commonly used,
# 
# Gaussian Naïve Bayes:
# 
# Assumes that the features follow a Gaussian distribution.
# 
# Suitable for continuous or numerical data.
# 
# Multinomial Naïve Bayes:
# 
# Assumes that the features are generated from a multinomial distribution.
# 
# Commonly used for document classification tasks, especially when the features represent word frequencies.
# 
# Bernoulli Naïve Bayes:
# 
# Assumes that features are binary (Bernoulli, boolean) variables.
# 
# Appropriate for binary feature data sets, often used in text classification tasks.
# 
# Based off what Ive read I think it might be appropriate to investigate using a different form of naive Bayes classifier such as the Gaussian, as we have a mix of continuous and numerical features in the breast cancer data set.