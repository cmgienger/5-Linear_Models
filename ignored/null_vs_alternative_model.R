library(ggplot2)
library(dplyr)
library(patchwork)

# --- Read and prep data ---
Daphniagrowth <- read.csv("Daphniagrowth.csv")
Daphniagrowth$parasite <- as.factor(Daphniagrowth$parasite)

# Overall mean (H0)
overall_mean <- mean(Daphniagrowth$growth.rate, na.rm = TRUE)

# Group means (H1)
group_means <- Daphniagrowth %>%
  group_by(parasite) %>%
  summarise(mean_growth = mean(growth.rate, na.rm = TRUE))

# Assign index to each observation, sorted by group
Daphniagrowth <- Daphniagrowth %>%
  arrange(parasite) %>%
  mutate(index = row_number()) %>%
  left_join(group_means, by = "parasite")

# For each group, determine index range for horizontal mean line
group_ranges <- Daphniagrowth %>%
  group_by(parasite) %>%
  summarise(
    x_min = min(index) - 0.5,
    x_max = max(index) + 0.5,
    mean_growth = unique(mean_growth)
  )

# --- NULL MODEL (H₀) ---
p_null <- ggplot(Daphniagrowth, aes(x = index, y = growth.rate, color = parasite)) +
  geom_segment(aes(xend = index, yend = overall_mean),
               color = "black", alpha = 0.6, linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = overall_mean, color = "red", linewidth = 1.2) +
  labs(
    title = "Null Hypothesis (Ho): No Effect of Parasite",
    x = "Index", y = "Growth rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# --- ALTERNATIVE MODEL (H₁) ---
p_alt <- ggplot(Daphniagrowth, aes(x = index, y = growth.rate, color = parasite)) +
  # Residual lines to group means
  geom_segment(aes(xend = index, yend = mean_growth),
               color = "black", alpha = 0.6, linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.7) +
  # Group-specific mean segments spanning each group’s index range
  geom_segment(
    data = group_ranges,
    aes(x = x_min, xend = x_max, y = mean_growth, yend = mean_growth, color = parasite),
    linewidth = 1.2
  ) +
  labs(
    title = "Alternative Hypothesis (Ha): Parasite Affects Growth",
    x = "Index", y = "Growth rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Combine side-by-side
p_null + p_alt
