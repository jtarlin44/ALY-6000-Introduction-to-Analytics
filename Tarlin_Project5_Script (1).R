#Joseph Tarlin, 05/10/2026, ALY6000
#Environment reset and loading packages
rm(list = ls())
library(tidyverse)
library(janitor)
ball_data <- read.csv("ball-dataset.csv")
#Creating frequency table that counts for each color of ball
freq_color <- ball_data %>% group_by(color) %>% summarise(count = n()) %>% arrange(desc(count))
#Creating frequency table that contains counts for each label of ball
freq_label <- ball_data %>% group_by(label) %>% summarise(counts = n()) %>% arrange(label)
#Bar chart of ball counts by colors
ggplot(freq_color, aes(x = color, y = count, fill = color)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("blue" = "blue", "green" = "green", "red" = "red", "yellow" = "yellow")) + labs(title = "Color Counts of Balls", x = "Color", y = "Count") + theme_minimal() + theme(legend.position = "none")
#Bar chart of ball counts by labels
ggplot(freq_label, aes(x = label, y = counts, fill = label)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("A" = "pink", "B" = "limegreen", "C" = "darkgreen", "D" = "lightblue", "E" = "purple")) + labs(title = "Label Counts of Balls", x = "Label", y = "Count") + theme_minimal() + theme(legend.position = "none")
#Probability of drawing a green ball which is 0.192
prob6_result <- freq_color %>% filter(color == "green") %>% summarise(probability = count / sum(freq_color$count)) %>% pull(probability)
#Probability of drawing a blue or red ball which is 0.715
prob7_result <- freq_color %>% filter(color %in% c("blue", "red")) %>% summarise(probability = sum(count) / sum(freq_color$count)) %>% pull(probability)
#Probability of drawing a ball with label A or C which is 0.189
prob8_result <- freq_label %>% filter(label %in% c("A", "C")) %>% summarise(probability = sum(counts) / sum(freq_label$counts)) %>% pull(probability)
#Probability of drawing a yellow ball with a label D which is 0.036
prob9_result <- ball_data %>% filter(color == "yellow" & label == "D") %>% summarise(probability = n() / nrow(ball_data)) %>% pull(probability)
#Probability of drawing a yellow ball or a ball with label D which is 0.382
prob10_result <- ball_data %>% summarise(probability = sum(color == "yellow" | label == "D") / n()) %>% pull(probability)
#Probability of drawing a blue ball followed by a red ball without replacement which is 0.1253814
total <- sum(freq_color$count)
p_blue_first <- freq_color %>% filter(color == "blue") %>% pull(count) / total 
p_red_second <- freq_color %>% filter(color == "red") %>% pull(count) / (total - 1)
prob11_result <- p_blue_first * p_red_second
#Probability of drawing four green balls in a row without replacement which is 0.001324826
green_count <- 192
total <- 1000
prob12_result <- prod(sapply(0:3, function(i) (green_count - i) / (total - i)))
#Probability of drawing a red ball followed by a ball with label B without replacement which is 0.124564
p_red_first <- 408 / 1000
p_B_second <- 305 / 999
prob13_result <- p_red_first * p_B_second
#Writing function to compute factorial of given number
my_factorial <- function(n) {if (n == 0) {return(1)} else {return(n * my_factorial(n - 1))}}
#Testing the factorial to see if it works
my_factorial(0)   
my_factorial(3)   
my_factorial(5)
#Manually create all possible outcomes of flipping a coin 4 times
coin_outcomes <- expand.grid(first = c("H", "T"), second = c("H", "T"), third  = c("H", "T"), fourth = c("H", "T")) %>% arrange(first, second, third, fourth)
#Computing probability of each row outcome
p_H <- 0.6
p_T <- 0.4
coin_outcomes <- coin_outcomes %>% mutate(probability = ifelse(first  == "H", p_H, p_T) * ifelse(second == "H", p_H, p_T) * ifelse(third  == "H", p_H, p_T) * ifelse(fourth == "H", p_H, p_T))
#Computing probability of each possible number of heads outcome
num_heads_prob <- coin_outcomes %>% mutate(num_heads = rowSums(across(c(first, second, third, fourth), ~ . == "H"))) %>% group_by(num_heads) %>% summarise(probability = sum(probability))
#Probability of outcome of three heads which is 0.3456
prob18_result <- num_heads_prob %>% filter(num_heads == 3) %>% pull(probability)
#Probability of outcome of two heads or four heads which is 0.4752
prob19_result <- num_heads_prob %>% filter(num_heads %in% c(2, 4)) %>% summarise(probability = sum(probability)) %>% pull(probability)
#Probability of outcome of less than or equal to three heads which is 0.8704
prob20_result <- num_heads_prob %>% filter(num_heads <= 3) %>% summarise(probability = sum(probability)) %>% pull(probability)
#Bar chart for probability distribution of heads for 4 flips
ggplot(num_heads_prob, aes(x = factor(num_heads), y = probability)) + geom_bar(stat = "identity", fill = "cyan", color = "cyan") + labs(title = "Probability Distibution of Heads for 4 flips", x = "Number of Heads", y = "Probability") + theme_minimal()
#Probability of winning exactly 10 games (all 5 home AND all 5 away) which is 0.07415771 
p_home <- 0.75
p_away <- 0.50
prob22_result <- p_home^5 * p_away^5
# Probability of winning more than 1 game out of 10
p_0_wins <- (1 - p_home)^5 * (1 - p_away)^5
p_1_win <- (dbinom(1, size = 5, prob = p_home) * dbinom(0, size = 5, prob = p_away)) + (dbinom(0, size = 5, prob = p_home) * dbinom(1, size = 5, prob = p_away))
prob23_result <- 1 - p_0_wins - p_1_win
#Number of ways to pick 3 home games from 5 total games AND 2 away games from 5 total games
prob24_result <- choose(5, 3) * choose(5, 2)