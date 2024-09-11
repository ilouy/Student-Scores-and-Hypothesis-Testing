library(rstan)
library(ggplot2)

# Load the data
data <- read.csv("StudentsPerformance.csv")

# Levels are explicitly specified to ensure correct order in plotting
data$parental_education <- factor(data$parental.level.of.education, levels = c("some high school", "high school", "some college", "associate's degree", "bachelor's degree", "master's degree"))

# Extract education levels
education_levels <- levels(data$parental_education)

# Prepare data list for Stan
data_list <- list(
  L = length(education_levels),    # Number of distinct education levels
  N = nrow(data),                  # Number of observations
  education = as.integer(data$parental_education),
  reading_score = data$reading.score,
  writing_score = data$writing.score,
  math_score = data$math.score
)

# Define Stan model
model_code <- "
data {
  int L;
  int N;
  int education[N];
  int reading_score[N];
  int writing_score[N];
  int math_score[N];
}
parameters {
  real<lower=0> mu[3, L];     // Means for each subject and parental education level
  real<lower=0> sigma[3, L];  // Standard deviations for each subject and parental education level
}
model {
  // Priors
  for (i in 1:3) {
    for (j in 1:L) {
      mu[i, j] ~ uniform(0, 100); 
      sigma[i, j] ~ uniform(0, 100);
    }
  }
  
  // Likelihoods
  for (n in 1:N) {
    reading_score[n] ~ normal(mu[1, education[n]], sigma[1, education[n]]);
    writing_score[n] ~ normal(mu[2, education[n]], sigma[2, education[n]]);
    math_score[n] ~ normal(mu[3, education[n]], sigma[3, education[n]]);
  }
}
"

# Fit the model
fit <- stan(model_code = model_code, data = data_list, iter = 1000, chains = 4)

# Print summary of the fit and inspect chains
print(fit)        # All Rhats are 1
traceplot(fit)

# Extract posterior samples
posterior_samples <- extract(fit)

# Compute means for mu
mu_mean <- apply(posterior_samples$mu, c(2, 3), mean)

# Calculate credibility intervals for mu
mu_ci <- array(NA, dim = c(3, length(education_levels), 2))

for (i in 1:3) {
  for (j in 1:length(education_levels)) {
    mu_ci[i, j, ] <- quantile(posterior_samples$mu[, i, j], probs = c(0.025, 0.975))
  }
}

# Convert to data frame for plotting
df <- data.frame(
  Subject = rep(c("Reading", "Writing", "Math"), each = length(education_levels)),
  Education = rep(education_levels, times = 3),
  Score = c(mu_mean[1,], mu_mean[2,], mu_mean[3,]),
  Lower = c(mu_ci[1, , 1], mu_ci[2, , 1], mu_ci[3, , 1]),
  Upper = c(mu_ci[1, , 2], mu_ci[2, , 2], mu_ci[3, , 2])
)

df$Education <- factor(df$Education, levels = education_levels) # To ensure proper order in plotting

# Plot the results
ggplot(df, aes(x = Subject, y = Score, fill = Education)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Estimated Test Scores by Parental Level of Education and Subject", y = "Mean Score")
