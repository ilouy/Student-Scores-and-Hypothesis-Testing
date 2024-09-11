library(rstan)
library(ggplot2)

# Load the data
data <- read.csv("StudentsPerformance.csv")

# Prepare data list for Stan
data_list <- list(
  N = nrow(data),              # Number of observations
  reading_score = data$reading.score,
  writing_score = data$writing.score,
  math_score = data$math.score
)

# Define Stan model
model_code <- "
data {
  int N;
  int reading_score[N];
  int writing_score[N];
  int math_score[N];
}
parameters {
  real<lower=0> mu[3];     // Means for each subject
  real<lower=0> sigma[3];  // Standard deviations for each subject
}
model {
  // Priors
  for (s in 1:3) {
    mu[s] ~ uniform(0, 100); 
    sigma[s] ~ uniform(0, 100);
  }
  
  // Likelihoods
  reading_score ~ normal(mu[1], sigma[1]);
  writing_score ~ normal(mu[2], sigma[2]);
  math_score ~ normal(mu[3], sigma[3]);
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
mu_mean <- apply(posterior_samples$mu, 2, mean)

# Calculate credibility intervals for mu
mu_ci <- array(NA, dim = c(3, 2))

for (i in 1:3) {
  mu_ci[i, ] <- quantile(posterior_samples$mu[, i], probs = c(0.025, 0.975))
}

# Convert to data frame for plotting
df <- data.frame(
  Subject = c("Reading", "Writing", "Math"),
  Score = mu_mean,
  Lower = mu_ci[, 1],
  Upper = mu_ci[, 2]
)

# Plot the results
ggplot(df, aes(x = Subject, y = Score, fill = Subject)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Estimated Test Scores by Subject", y = "Mean Score") +
  geom_text(aes(label = round(Score)), vjust = 1.5, position = position_dodge(width = 0.9))
