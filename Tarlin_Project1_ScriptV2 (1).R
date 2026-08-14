#Joseph Tarlin, 04/13/2026, ALY6000
123 * 453 
5 ^ 2 * 40 
TRUE & FALSE 
TRUE | FALSE
75 %% 10 
75 / 10 
first_vector <- c(17, 12, -33, 5)
counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)
second_vector <- 20:1
counting_vector <- 5:15
grades <- c(96, 100, 85, 92, 81, 72)
bonus_points_added <- c(99, 103, 88, 95, 84, 75)
one_to_one_hundred <- c (1:100)
#add 20 to each value in second vector
second_vector + 20
#multiply each value in second vector by 20
second_vector * 20
#Check which elements greater than or equal to 20
second_vector >= 20
#Check which elements not equal to 20
second_vector !=20
total <- sum(one_to_one_hundred)
average_value <- mean(one_to_one_hundred) 
median_value <- median(one_to_one_hundred)
max_value <- max(one_to_one_hundred) 
min_value <- min(one_to_one_hundred)
first_value <-second_vector[1]
first_three_values <- second_vector [1:3]
vector_from_brackets <- second_vector [c(1, 5, 10, 11)]
#TRUE is keep element and FALSE is drop element
vector_from_boolean_brackets <- first_vector [c(FALSE, TRUE, FALSE, TRUE)]
second_vector >= 10
#elements greater than or equal to 10
one_to_one_hundred [one_to_one_hundred >=20]
#elements one to one hundred greater than or equal to 20
lowest_grades_removed <- grades [grades > 85]
middle_grades_removed <- grades [-c(3,4)]
fifth_vector <- second_vector [-c(5,10)]
set.seed (5)
random_vector <- runif(n = 10, min = 0, max = 1000)
sum_vector <- sum(random_vector)
cumsum_vector <-cumsum(random_vector)
mean_vector <- mean(random_vector)
sd_vector <- sd(random_vector)
round_vector <- round(random_vector)
sort_vector <- sort(random_vector)
first_dataframe <- read.csv("ds_salaries.csv")
summary(first_dataframe)
