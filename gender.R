library(rstan)
library(ggplot2)

# Load the data
data <- read.csv("StudentsPerformance.csv")

# Encode gender as a factor
data$gender_factor <- as.factor(data$gender)

# Extract gender levels
gender_levels <- levels(data$gender_factor)

# Prepare data list for Stan
data_list <- list(
  L = length(gender_levels),    # Number of distinct gender levels
  N = nrow(data),             # Number of observations
  gender = as.integer(data$gender_factor),
  reading_score = data$reading.score,
  writing_score = data$writing.score,
  math_score = data$math.score
)

# Define Stan model
model_code <- "
data {
  int L;
  int N;
  int gender[N];
  int reading_score[N];
  int writing_score[N];
  int math_score[N];
}
parameters {
  real<lower=0> mu[3, L];     // Means for each subject and gender
  real<lower=0> sigma[3, L];  // Standard deviations for each subject and gender
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
  for (m in 1:N) {
    reading_score[m] ~ normal(mu[1, gender[m]], sigma[1, gender[m]]);
    writing_score[m] ~ normal(mu[2, gender[m]], sigma[2, gender[m]]);
    math_score[m] ~ normal(mu[3, gender[m]], sigma[3, gender[m]]);
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
mu_ci <- array(NA, dim = c(3, length(gender_levels), 2))

for (i in 1:3) {
  for (j in 1:length(gender_levels)) {
    mu_ci[i, j, ] <- quantile(posterior_samples$mu[, i, j], probs = c(0.025, 0.975))
  }
}

# Convert to data frame for plotting
df <- data.frame(
  Subject = rep(c("Reading", "Writing", "Math"), each = length(gender_levels)),
  Gender = rep(gender_levels, times = 3),
  Score = c(mu_mean[1,], mu_mean[2,], mu_mean[3,]),
  Lower = c(mu_ci[1, , 1], mu_ci[2, , 1], mu_ci[3, , 1]),
  Upper = c(mu_ci[1, , 2], mu_ci[2, , 2], mu_ci[3, , 2])
)

# Plot the results
ggplot(df, aes(x = Subject, y = Score, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Estimated Test Scores by Gender and Subject", y = "Mean Score") +
  geom_text(aes(label = round(Score)), vjust = 1.5, position = position_dodge(width = 0.9))
