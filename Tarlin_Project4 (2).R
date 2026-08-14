#Joseph Tarlin, 05/03/2026, ALY6000
movies <- read.csv("TMDB_movie_dataset_v11.csv")
movies <- clean_names(movies)
#Using mdy function to convert dates the movies were first released to a common type date
movies <- movies |> mutate(release_date = parse_date_time(release_date, orders = c("mdy", "ymd", "dmy", "my", "y"))) |> filter(!is.na(release_date))
#Making sure all of the data is complete and there are no missing values
movies |> filter(is.na(release_date)) |> select(release_date)
#Extracting the year from the release date for its own column
movies <- movies |> mutate(year= year(release_date))
#Removing specific variables from the dataset that aren't necessary for the analysis
movies <- movies |> select(-c(homepage, backdrop_path, imdb_id, adult, original_title, overview, poster_path, tagline, keywords))
#Renaming columns manually to make the data more presentable
movies <- movies %>% rename(totalvote_count = vote_count, runtime_minutes = runtime)
#Cleaning the budget format to display a standard numeric listing with no exponents
movies <- movies %>% mutate(budget = format(budget, scientific = FALSE))
#Filtering out movies under 1.5 hours and over 200 minutes 
movies <- movies %>% filter(runtime_minutes >= 90, runtime_minutes <= 200)
#Filter out movies to only includes ones released between 1980 and 2020 therefore getting rid of any film unreleased
movies <- movies |> filter(year >= 1980, year <= 2020)
#Removing any row that contains NA
movies <- na.omit(movies)
#Ensuring numeric columns are actually numeric
movies <- movies %>% mutate(budget = as.numeric(budget), revenue = as.numeric(revenue), runtime_minutes = as.numeric(runtime_minutes), vote_average = as.numeric(vote_average))
#Converting status to a categorical factor
movies <- movies %>% mutate(status = as.factor(status))
#Trim whitespace from text columns
movies <- movies %>% mutate(title = str_trim(title), str_trim(original_language, str_trim(runtime_minutes))
#Standardize language codes to uppercase
movies <- movies %>% mutate(original_language = str_to_upper(original_language))
#Remove movies with revenue generated under 100 million and budget under 50 million
movies <- movies %>% filter(revenue > 100000000, budget > 50000000)
#Create a profit column
movies <-movies %>% mutate(profit = revenue - budget)
#Create a return on investment column
movies <- movies %>% mutate(roi = ((revenue - budget)/budget) * 100)
#Providing a long view of the dataset
glimpse(movies)
#Breaking down the statistics in this dataset
summary(movies)
#Determining descriptive statistics for revenue generated, with mean being 388400000 and median being 274700000
summary(movies$revenue)
#Determining descriptive statistics for profit, with the minimum being -58059958 and the maximum being 2686706026 for a wide range between the two
summary(movies$profit)
#Determining descriptive statistics for run time, with the average movie in this study being 120.1 minutes, the 1st quartile is 104 minutes, and 3rd quartile is 132 minutes
summary(movies$runtime_minutes)
#Movie budget vs revenue scatter plot to show the trend between whether higher budgets lead to higher revenue
movies %>% filter(budget > 50000000, revenue > 100000000) %>% ggplot(aes(x = budget, y = revenue)) + geom_point(alpha = 0.3, color = "steelblue") + geom_smooth(method = "lm", color = "red", se = FALSE) + scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) + scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) + labs(title = "Movie Budget vs Revenue", x = "Budget (Millions)", y = "Revenue (Millions)") + theme_minimal()
#Histogram to show the number of movies released per year between 1980 and 2020 in this dataset
movies %>% count(year) %>% ggplot(aes(x = year, y = n, fill = year)) + geom_col(show.legend = FALSE) + labs(title = "Number of Movies Released Per Year", x = "Year", y = "Number of Movies") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Basic box plot of movie run times to see different ranges and outliers
movies %>% ggplot(aes(y = runtime_minutes)) + geom_boxplot(fill = "yellow", color = "black", outlier.alpha = 0.2) + labs(title = "Box Plot of Movie Runtimes",y = "Runtime (Minutes)") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14), axis.text.x = element_blank())                 
#Plot for average movie profits per year over the years 1980 - 2020
movies %>% ggplot(aes(x = year, y = profit)) + geom_line(color = "darkgreen", linewidth = 1) + geom_point(color = "darkgreen", size = 2) + geom_hline(yintercept = 0, color = "red", linetype = "dashed") + scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) + labs(title = "Average Movie Profit Over the Years", subtitle = "1980 - 2020 | Red line = break even point", x = "Year", y = "Average Profit (Millions)") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14))
#Creating a runtime based variable to group even further between 30 minute increments
movies <- movies %>% mutate(runtime_category = case_when(runtime_minutes >= 90 & runtime_minutes < 120 ~ "Standard (90-120 min)", runtime_minutes >= 120 & runtime_minutes < 150 ~ "Long (120-150 min)", runtime_minutes >= 150 & runtime_minutes < 180 ~ "Very Long (150-180 min)", runtime_minutes >= 180 ~ "Epic (180+ min),"))  
#Creating a decade variable
movies <- movies %>% mutate(decade = paste0(floor(year / 10) * 10, "s"),)
#Looking at the profit over the four decades analyzed
decade_profit <- movies %>% filter(budget > 50000000, revenue > 100000000, !is.na(decade)) %>% group_by(decade) %>% summarise(total_profit = sum(profit, na.rm = TRUE), avg_profit = mean(profit, na.rm = TRUE), total_revenue = sum(revenue, na.rm = TRUE),total_budget = sum(budget, na.rm = TRUE), movie_count = n(), profitable_movies = sum(profit > 0, na.rm = TRUE), success_rate = round((profitable_movies / movie_count) * 100, 1)) %>% arrange(decade)
#New dataframe to look at the relationship between runtime and return on investment for these movies
runtime_roi_summary <- movies %>% filter(!is.na(roi), !is.na(runtime_category), budget > 50000000, revenue > 100000000) %>% group_by(runtime_category) %>% summarise(avg_roi = round(mean(roi, na.rm = TRUE), 2), median_roi = round(median(roi, na.rm = TRUE), 2), avg_roi_per_minute = round(mean(roi, na.rm = TRUE), 2),movie_count = n(),profitable_count = sum(roi > 0, na.rm = TRUE), success_rate = round((profitable_count / movie_count) * 100, 1)) %>% arrange(desc(avg_roi))
#Splitting into individual genres
movies_genres <- movies %>% filter(budget > 50000000, revenue > 100000000, !is.na(genres), genres != "") %>% separate_rows(genres, sep = ",") %>% mutate(genres = str_trim(genres)) %>% filter(genres != "")
#Calculating profit stats per genre
genre_profit_stats <- movies_genres %>% group_by(genres) %>% summarise(total_profit = sum(profit, na.rm = TRUE), avg_profit = mean(profit, na.rm = TRUE), median_profit = median(profit, na.rm = TRUE), max_profit = max(profit, na.rm = TRUE), min_profit = min(profit, na.rm = TRUE), sd_profit = sd(profit, na.rm = TRUE), movie_count = n(), profitable_count = sum(profit > 0, na.rm = TRUE), loss_count = sum(profit < 0, na.rm = TRUE), success_rate = round((profitable_count / movie_count) * 100, 1), avg_roi = round(mean(roi, na.rm = TRUE), 2), .groups = "drop") %>% arrange(desc(total_profit))  
#Boxplot for Profit Distribution by Movie Genre
movies_genres %>% filter(genres %in% genre_stats$genres, profit >= -5000000000 & profit <= 15000000000) %>% ggplot(aes(x = reorder(genres, profit, median), y = profit, fill = genres)) + geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2, outlier.size = 1) + coord_flip() + scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) + geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) + labs(title = "Profit Distribution by Movie Genre", subtitle = "Red dashed line = break even point | Ordered by median profit", x = "Genre", y = "Profit (Millions)") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(size = 10, color = "gray50"), axis.text.y = element_text(size = 10))
#Histogram for runtime showing movie count and average roi by runtime
runtime_roi_summary %>% select(runtime_category, movie_count, avg_roi) %>% pivot_longer(cols = c(movie_count, avg_roi), names_to = "metric", values_to = "value") %>% mutate(metric = recode(metric, "movie_count" = "Movie Count", "avg_roi" = "Average ROI (%)")) %>% ggplot(aes(x = runtime_category, y = value, fill = runtime_category)) + geom_col(show.legend = FALSE, color = "white", width = 0.7) + geom_text(aes(label = ifelse(metric == "Movie Count", scales::comma(value), paste0(round(value, 1), "%"))), vjust = -0.5, size = 3, fontface = "bold") + facet_wrap(~ metric, scales = "free_y") + scale_fill_brewer(palette = "Blues") + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + labs(title = "Movie Count and Average ROI by Runtime Category", subtitle = "Side by side comparison of volume and return", x = "Runtime Category", y = "") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(size = 10, color = "gray50"), axis.text.x = element_text(angle = 45, hjust = 1, size = 8), strip.text = element_text(face = "bold", size = 11))
#Line plot showing growth in budget, revenue, and profit over four decades from the 1980s to the 2020s
decade_profit %>% select(decade, total_budget, total_revenue, total_profit) %>% pivot_longer(cols = c(total_budget, total_revenue, total_profit), names_to = "metric", values_to = "amount") %>% mutate(metric = recode(metric, "total_budget"  = "Total Budget", "total_revenue" = "Total Revenue", "total_profit"  = "Total Profit")) %>% ggplot(aes(x = decade, y = amount, color = metric, group = metric)) + geom_line(linewidth = 1.2) + geom_point(size = 3) + geom_text(aes(label = scales::dollar(amount, scale = 1e-9, suffix = "B")), vjust = -1, size = 2.8, fontface = "bold") + scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B"), expand = expansion(mult = c(0.1, 0.2))) + scale_color_manual(values = c("Total Budget" = "coral", "Total Revenue" = "steelblue", "Total Profit" = "darkgreen")) + labs(title = "Growth in Budget, Revenue and Profit Across Decades", subtitle = "1980 - 2020 | Shows overall industry financial growth", x = "Decade", y = "Amount (Billions)", color = "") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(size = 10, color = "gray50"), legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))