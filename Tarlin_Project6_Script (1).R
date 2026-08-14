#Joseph Tarlin, 05/16/2026, ALY6000
#Consider Boston Red Sox playing stretch of 7 gaves where probability of winning a game is 0.65 and an outcome is the number of wins during those 7 games
#Probability of Red Sox winning exactly 5 games
p_win <- 0.65
n_games <- 7
calculation_exact_wins <- 5
prob1_result <- dbinom(calculation_exact_wins, size = n_games, prob = p_win)
#Creating tibble with each possible outcome and probability of that outcome, for example outcome of 0 wins would be 0.000643, probability of 2 wins would be 0.008364, etc...
prob2_result <- tibble(wins = 0:7, probability = dbinom(0:7, size = 7, prob = 0.65))
#Probability that Red Sox will win fewer than 5 games which is 0.467716
prob3_result <- pbinom(4, size = 7, prob = 0.65)
#Probability that Red Sox win between 3 and 5 games inclusively which is 0.710594
prob4_result <- pbinom(5, size = 7, prob = 0.65) - pbinom(2, size = 7, prob = 0.65)
#Probability that Red Sox win more than 4 games which is 0.532283
prob5_result <- 1 - pbinom(4, size = 7, prob = 0.65)
#Theoretical expected value of number of wins for Red Sox in 7-game series which is 4.55
prob6_result <- 7 * 0.65
#Theoretical variance of number of wins for the Red Sox in 7-game series which is 1.5925
prob7_result <- sum(prob2_result$probability * (prob2_result$wins - prob6_result)^2)
#Generating 1000 random values for number of wins by Red Sox in 7-game series
set.seed(10)
simulation_wins <- rbinom(1000, size = 7, prob = 0.65)
#Sample mean of the 1000 random values which is 4.521
prob9_result <- mean(simulation_wins)
#Computing sample variable of the 1000 randomly generated outcomes which is 1.689248
prob10_result <- var(simulation_wins)
#Consider that when analyzing calls in a call center, the number of calls received each hour follows a Poisson distribution averaging 7 calls per employee per hour
#Probability that an employee will receive exactly 6 calls in the next hour which is 0.149003 
lamda <- 7
call_probability <- 6
prob11_result <- dpois(call_probability, lambda = lamda)
#Probability that an employee will receive 40 or fewer calls in the next 8 hours
lambda_8hours <- 7 * 8
prob12_result <- ppois(40, lambda = lambda_8hours)
#Assuming 5 employees working 8-hour shifts, calculating the probability that they will meet the quota of 275 or more calls during the shift which is 0.625430
lambda_total <- 7 * 8 * 5
prob13_result <-ppois(274, lambda = lambda_total, lower.tail = FALSE)
#Probability of meeting call quota of 275 calls or more with 4 employees over 8 hours which is 0.000540
lambda_sick <- 7 * 8 * 4
prob14_result <- ppois(274, lambda = lambda_sick, lower.tail = FALSE)
#Calculating how many calls a single employee working an 8-hour shift needs for the day to be considered in top 10% of days volume-wise which is 66 calls
lambda_single <- 7 * 8
prob15_result <- qpois(0.90, lambda = lambda_single)
#Generating 1000 random values for the number of calls for a single employee during an 8-hour shift
set.seed(15)
simulation_calls <- rpois(1000, lambda = lambda_single)
#Computing sample mean of the 1000 random values which is 56.303
prob17_result <- mean(simulation_calls)
#Computing sample variance of the 1000 random values which is 54.8300
prob18_result <- var(simulation_calls)
#Life spans of light bulbs at certain manufacturing companies follow a normal distribution with mean life span of 2000 hours and standard deviation of 100 hours
#Percentage of light bulbs with a lifespan of between 1800 and 2200 hours which is 0.9545 or 95.45%
mean_hours <- 2000
sd_hours <- 100
prob19_result <- pnorm(2200, mean = mean_hours, sd = sd_hours) - pnorm(1800, mean = mean_hours, sd = sd_hours)
#Percentage of light bulbs with a life span of more than 2500 hours which is 2.866518e-07
prob20_result <- pnorm(2500, mean = mean_hours, sd = sd_hours, lower.tail = FALSE)
#Maximum hours for a light bulb to be considered defective, so bottom 10%, which is 1872 hours
prob21_result <- ceiling(qnorm(0.10, mean = mean_hours, sd = sd_hours))
#Generating 10000 random values for life spans of manufactured light bulbs
set.seed(25)
lightbulb_population <- rnorm(10000, mean = mean_hours, sd = sd_hours)
#Computing population mean of light bulb life spans which is 1999.71
prob23_result <- mean(lightbulb_population)
#Computing population standard deviation of light bulb life spans which is 100.058
prob24_result <- sd(lightbulb_population)
#Taking 1000 samples of 100 values each and computing sample means
set.seed(1)
prob25_result <- replicate(1000, mean(sample(lightbulb_population, size = 100, replace = TRUE)))
#Creating histogram from results of the previous code on line 67
hist(prob25_result, main   = "Sampling Distribution of Sample Means", xlab   = "Sample Mean (hours)", ylab   = "Frequency", col    = "steelblue", border = "white", breaks = 30)
#Computing mean of values from line 67
prob27_result <- mean(prob25_result)
#loading the penguins dataset
data("penguins")
#Exploring distribution of flipper length of Adélie penguin
adelie <- penguins %>% filter(species == "Adelie")
summary(adelie$flipper_length_mm)
cat("Mean:", mean(adelie$flipper_length_mm, na.rm = TRUE), "\n")
cat("SD:", sd(adelie$flipper_length_mm, na.rm = TRUE), "\n")
cat("Variance:", var(adelie$flipper_length_mm, na.rm = TRUE), "\n")
#Visual evidence of distribution of flipper length of Adélaide penguin
hist(adelie$flipper_length_mm, main   = "Flipper Length Distribution - Adelie Penguins", xlab   = "Flipper Length (mm)", ylab   = "Frequency", col    = "red", border = "white", breaks = 20)
boxplot(adelie$flipper_length_mm, main   = "Flipper Length Boxplot - Adelie Penguins", ylab   = "Flipper Length (mm)", col = "steelblue", border = "black")
ggplot(adelie, aes(x = flipper_length_mm)) + geom_density(fill = "yellow", alpha = 0.5) + stat_function(fun = dnorm, args = list(mean = mean(adelie$flipper_length_mm, na.rm = TRUE), sd = sd(adelie$flipper_length_mm, na.rm = TRUE)), col = "red", lwd = 1) + labs(title = "Density Plot of Adelie Flipper Length", subtitle = "Black = Actual Distribution, Red = Normal Curve", x = "Flipper Length (mm)", y = "Density") + theme_minimal()
ggplot(adelie, aes(x = species, y = flipper_length_mm)) + geom_violin(fill = "brown", alpha = 0.5) + geom_boxplot(width = 0.1, fill = "white") + labs(title = "Violin Plot of Adelie Flipper Length", x = "Species", y = "Flipper Length (mm)") + theme_minimal()
#Statistical test for normality
shapiro.test(adelie$flipper_length_mm)
#Exploring relationship between flipper length and beak depth of the gentoo penguin
gentoo <- penguins %>% filter(species == "Gentoo")
summary(gentoo$flipper_length_mm)
summary(gentoo$bill_depth_mm)
cor_result <- cor(gentoo$flipper_length_mm, gentoo$bill_depth_mm, use = "complete.obs")
cor.test(gentoo$flipper_length_mm, gentoo$bill_depth_mm, use = "complete.obs")
#Visual evidence of relationship between flipper length and beak depth of gentoo penguin
ggplot(gentoo, aes(x = flipper_length_mm, y = bill_depth_mm)) + geom_point(color = "steelblue", alpha = 0.6, size = 2) + geom_smooth(method = "lm", color = "red", se = TRUE) + labs(title = "Flipper Length vs Beak Depth - Gentoo Penguins", subtitle = "Red line = Linear regression fit", x = "Flipper Length (mm)", y = "Beak Depth (mm)") + theme_minimal()
ggplot(gentoo, aes(x = flipper_length_mm, y = bill_depth_mm, color = sex)) + geom_point(alpha = 0.6, size = 2) + geom_smooth(method = "lm", se = TRUE) + scale_color_manual(values = c("female" = "steelblue", "male" = "darkorange")) + labs(title = "Flipper Length vs Beak Depth by Sex - Gentoo Penguins", x = "Flipper Length (mm)", y = "Beak Depth (mm)", color = "Sex") + theme_minimal()
ggplot(gentoo, aes(y = flipper_length_mm)) + geom_boxplot(fill = "purple", color = "black") + labs(title = "Boxplot of Gentoo Flipper Length", y = "Flipper Length (mm)") + theme_minimal()
ggplot(gentoo, aes(y = bill_depth_mm)) + geom_boxplot(fill = "darkgreen", color = "black") + labs(title = "Boxplot of Gentoo Beak Depth", y = "Beak Depth (mm)") + theme_minimal()
ggplot(gentoo, aes(x = flipper_length_mm)) + geom_density(fill = "orange", alpha = 0.5) + labs(title = "Density Plot of Gentoo Flipper Length", x = "Flipper Length (mm)", y = "Density") + theme_minimal()
ggplot(gentoo, aes(x = bill_depth_mm)) + geom_density(fill = "pink", alpha = 0.5) + labs(title = "Density Plot of Gentoo Beak Depth", x = "Beak Depth (mm)", y = "Density") + theme_minimal()
#Linear regression model and residual plot
lm_model <- lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)
summary(lm_model)
gentoo_clean <- gentoo %>% filter(!is.na(flipper_length_mm) & !is.na(bill_depth_mm))
lm_model <- lm(bill_depth_mm ~ flipper_length_mm, data = gentoo_clean)
residual_df <- data.frame(fitted = fitted(lm_model), residuals = residuals(lm_model))
ggplot(residual_df, aes(x = fitted, y = residuals)) + geom_point(color = "lightgreen", alpha = 0.6) + geom_hline(yintercept = 0, color = "red", lwd = 1) + labs(title = "Residual Plot - Gentoo Flipper Length vs Beak Depth", x = "Fitted Values", y = "Residuals") + theme_minimal()