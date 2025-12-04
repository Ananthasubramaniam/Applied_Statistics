# Load libraries
library(dplyr)
library(ggplot2)

# Load dataset
df <- read.csv("players.csv")

# Filter forwards
forwards <- df %>% filter(Position == "Forward", Appearances > 0)

# Goals per game metric
forwards <- forwards %>% mutate(GoalsPerGame = Goals / Appearances)

# Compute mean & median goals
mean_goal <- mean(forwards$Goals)
median_goal <- median(forwards$Goals)

# Create groups
forwards <- forwards %>%
  mutate(Group_Mean = ifelse(Goals > mean_goal, "Above Mean", "Below Mean"),
         Group_Median = ifelse(Goals > median_goal, "Above Median", "Below Median"))

### ---------------------------------------------------------------
### Hypothesis Testing (unchanged)
### ---------------------------------------------------------------

t_mean <- t.test(
  forwards$GoalsPerGame[forwards$Group_Mean == "Above Mean"],
  forwards$GoalsPerGame[forwards$Group_Mean == "Below Mean"],
  alternative = "greater"
)

t_median <- t.test(
  forwards$GoalsPerGame[forwards$Group_Median == "Above Median"],
  forwards$GoalsPerGame[forwards$Group_Median == "Below Median"],
  alternative = "greater"
)

### ---------------------------------------------------------------
### Probability & Variance CI (unchanged)
### ---------------------------------------------------------------

k_mean <- sum(forwards$Goals > mean_goal)
pmean <- prop.test(k_mean, nrow(forwards), correct = FALSE)

k_median <- sum(forwards$Goals > median_goal)
pmedian <- prop.test(k_median, nrow(forwards), correct = FALSE)

n <- nrow(forwards)
s2 <- var(forwards$Goals)

lower_var <- ((n - 1) * s2) / qchisq(0.975, df = n - 1)
upper_var <- ((n - 1) * s2) / qchisq(0.025, df = n - 1)

c(lower_var, upper_var)

### ---------------------------------------------------------------
### NEW PLOTS
### ---------------------------------------------------------------

# A. Histogram of goals
ggplot(forwards, aes(x = Goals)) +
  geom_histogram(bins = 20, fill = "#2E86C1") +
  labs(title = "Distribution of Goals Among EPL Forwards",
       x = "Goals Scored", y = "Number of Forwards")

# B. Mean Group Bar Chart (with error bars)
mean_summary <- forwards %>%
  group_by(Group_Mean) %>%
  summarise(
    avg_gpg = mean(GoalsPerGame),
    sd_gpg = sd(GoalsPerGame),
    n = n(),
    se = sd_gpg / sqrt(n)   # standard error
  )

ggplot(mean_summary, aes(x = Group_Mean, y = avg_gpg, fill = Group_Mean)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = avg_gpg - se, ymax = avg_gpg + se), width = 0.2) +
  labs(title = "Average GoalsPerGame (Above Mean vs Below Mean)",
       x = "", y = "Average Goals Per Game") +
  theme_minimal()

# C. Median Group Bar Chart (with error bars)
median_summary <- forwards %>%
  group_by(Group_Median) %>%
  summarise(
    avg_gpg = mean(GoalsPerGame),
    sd_gpg = sd(GoalsPerGame),
    n = n(),
    se = sd_gpg / sqrt(n)
  )

ggplot(median_summary, aes(x = Group_Median, y = avg_gpg, fill = Group_Median)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = avg_gpg - se, ymax = avg_gpg + se), width = 0.2) +
  labs(title = "Average GoalsPerGame (Above Median vs Below Median)",
       x = "", y = "Average Goals Per Game") +
  theme_minimal()

# D. Scatter plot: Goals vs Shots
ggplot(forwards, aes(x = Shots, y = Goals)) +
  geom_point(alpha = 0.6, color = "#1F618D") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship Between Shots and Goals",
       x = "Shots Taken", y = "Goals Scored") +
  theme_minimal()



