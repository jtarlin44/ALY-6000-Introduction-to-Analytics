#Joseph Tarlin, 04/19/2026, ALY6000
data_2015 <- read.csv("2015.csv")
names(data_2015)
glimpse(data_2015)
data_2015 <- clean_names(data_2015)
#Selecting the data from the dataset to look at country, region, happiness score, and freedom for each country
happy_df <- data_2015 |> select(country, region, happiness_score, freedom)
#Taking out first ten rows from dataset and storing it in top ten
top_ten_df <- happy_df[1:10, ]
#Filtering dataset values with freedom under 0.20
no_freedom_df <- happy_df[happy_df$freedom < 0.20, ]
#Arranging countries in descending order by their freedom values 
best_freedom_df <- happy_df[order(-happy_df$freedom), ]
#Creating a new column with that represents sum of family, freedom, and generosity values
data_2015$gff_stat <- data_2015$family + data_2015$freedom + data_2015$generosity
#Grouping number of coutries, mean happiness, and mean freedom values by region
regional_stats_df <- happy_df %>% group_by(region) %>% summarise(country_count = n(), mean_happiness = mean(happiness_score, na.rm = TRUE), mean_freedom = mean(freedom, na.rm = TRUE), )
baseball <- read.csv("baseball.csv")
#Filter out any baseball player with 0 at bats
baseball <- baseball[baseball$AB > 0, ]
#Add batting average column to baseball
baseball$BA <- baseball$H / baseball$AB
#Add on base percentage column to baseball
baseball$OBP <- (baseball$H + baseball$BB) / (baseball$AB + baseball$BB)
#Determine 10 players who stuck out the most this season
strikeout_artist <- baseball[order(-baseball$SO), ][1:10, ]
#Add players eligible for end of season awards
eligible_df <- baseball[baseball$AB >= 300 | baseball$G >= 100, ]
#Create histogram from eligible players batting average
hist(eligible_df$BA, main = "Histogram of BA for Eligible Players", xlab = "BA", ylab = "count", col = "green", border = "blue")   
     
