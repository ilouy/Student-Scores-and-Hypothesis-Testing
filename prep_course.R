library(rstan)
library(ggplot2)

# Load the data
data <- read.csv("StudentsPerformance.csv")

# Encode test preparation course as a factor
data$test_preparation_course <- as.factor(data$test.preparation.course)

# Extract course levels
course_levels <- levels(data$test_preparation_course)

# Prepare data list for Stan
data_list <- list(
  L = length(course_levels),    # Number of distinct preparation course levels
  N = nrow(data),               # Number of observations
  course = as.integer(data$test_preparation_course),
  reading_score = data$reading.score,
  writing_score = data$writing.score,
  math_score = data$math.score
)

# Define Stan model
model_code <- "
data {
  int L;
  int N;
  int course[N];
  int reading_score[N];
  int writing_score[N];
  int math_score[N];
}
parameters {
  real<lower=0> mu[3, L];     // Means for each subject and test preparation course
  real<lower=0> sigma[3, L];  // Standard deviations for each subject and test preparation course
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
    reading_score[m] ~ normal(mu[1, course[m]], sigma[1, course[m]]);
    writing_score[m] ~ normal(mu[2, course[m]], sigma[2, course[m]]);
    math_score[m] ~ normal(mu[3, course[m]], sigma[3, course[m]]);
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
mu_ci <- array(NA, dim = c(3, length(course_levels), 2))

for (i in 1:3) {
  for (j in 1:length(course_levels)) {
    mu_ci[i, j, ] <- quantile(posterior_samples$mu[, i, j], probs = c(0.025, 0.975))
  }
}

# Convert to data frame for plotting
df <- data.frame(
  Subject = rep(c("Reading", "Writing", "Math"), each = length(course_levels)),
  Course = rep(course_levels, times = 3),
  Score = c(mu_mean[1,], mu_mean[2,], mu_mean[3,]),
  Lower = c(mu_ci[1, , 1], mu_ci[2, , 1], mu_ci[3, , 1]),
  Upper = c(mu_ci[1, , 2], mu_ci[2, , 2], mu_ci[3, , 2])
)

# Plot the results
ggplot(df, aes(x = Subject, y = Score, fill = Course)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Estimated Test Scores by Test Preparation Course and Subject", y = "Mean Score") +
  geom_text(aes(label = round(Score)), vjust = 1.5, position = position_dodge(width = 0.9))
