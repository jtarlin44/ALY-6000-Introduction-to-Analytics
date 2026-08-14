#Joseph Tarlin, 04/26/2026, ALY6000
books <- read.csv("books.csv")
books <- clean_names(books)
#Converting the dates books were first published to a common type date using mdy function
books <- books |> mutate(first_publish_date = parse_date_time(first_publish_date, orders = c("mdy", "ymd", "dmy", "my", "y"))) |> filter(!is.na(first_publish_date))
#Confirming the data is not missing any values
books |> filter(is.na(first_publish_date)) |> select(first_publish_date)
#Using year function to extract the year from first_publish_date
books <- books |> mutate(year= year(first_publish_date))
#Filtering data to only include books published between 1990 and 2020
books <- books |> filter(year >= 1990 & year <= 2020)
#Removing specific variables from our dataset
books <- books |> select(-c(publish_date, edition, characters, price, genres, setting, isbn))
#Filtering data to keep only books under 700 pages 
books <- books |> filter(pages < 700)
#Removing any row containing NA
books <- na.omit(books)
#Providing long view of the dataset
glimpse(books)
#Breakdown of the statistics of the dataset
summary(books)
#Histogram of the book ratings
ggplot(books, aes(x = rating)) + geom_histogram(fill = "red", binwidth = 0.25) + labs(title = "Histogram of Book Ratings", x= "Rating", y= "Number of Books")
#Boxplot of number of pages per book in dataset
ggplot(books, aes(x= pages)) + geom_boxplot(fill = "red") + labs(title = "Box Plot of Page Counts", x= "Pages")
#Creating data frame that contains a count on number of books by year
by_year <- books |> group_by(year) |> summarise(total_books=n())
#Creating a line plot with points from the by_year data frame from 1990-2020
ggplot(by_year, aes(x=year, y= total_books)) + geom_line() + geom_point() + labs(title = "Total Number of Books Rated Per Year", x= "Year", y="Total Books")
#Creating new data frame with names of each unique publisher with the number of books for each publisher
book_publisher <- books |> group_by(publisher) |> summarise(book_count = n())
#Removing any publisher with fewer than 125 books
book_publisher <- book_publisher |> filter(book_count >= 125)
#Alligning the books in descending order by publisher with over 250 books
book_publisher <- book_publisher |> arrange(desc(book_count))
#Adding column to book_publisher with cumulative sum of book_count column
book_publisher <- book_publisher |> mutate(cum_counts = cumsum(book_count))
#Adding column to book_publisher with relative frequency of values in book_count column
book_publisher <- book_publisher |> mutate(rel_freq =book_count / sum(book_count))
#Adding column to book_publisher with cumulative sum of rel_freq column
book_publisher <- book_publisher |> mutate(cum_freq = cumsum(rel_freq))
#Making publisher column into factor with levels defined by current ordering of publisher column
book_publisher <- book_publisher |> mutate(publisher = factor(publisher, levels = publisher))
#Creating a Pareto Chart with an ogive of cumulative counts from this data
ggplot(book_publisher, aes(x = publisher)) + geom_bar(aes(y = book_count), stat = "identity", fill = "cyan") + geom_line(aes(y = cum_counts, group = 1), color = "red", linewidth = 1) + geom_point(aes(y = cum_counts), color = "red", size = 3) + labs(title = "Book Counts (1990-2020)", x = "Publisher", y = "Number of Books") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Average number of pages by publisher with a bar chart                                                                                                                                                                                        
books |> filter(publisher %in% book_publisher$publisher) |> group_by(publisher) |> summarise(avg_pages = mean(pages, na.rm = TRUE)) |> mutate(publisher = factor(publisher, levels = levels(book_publisher$publisher))) |> ggplot(aes(x = publisher, y = avg_pages)) + geom_bar(stat = "identity", fill = "steelblue") + geom_text(aes(label = round(avg_pages, 0)), vjust = -0.5, size = 3) + labs(title = "Average Number of Pages by Publisher (1990-2020)", x = "Publisher", y = "Average Pages") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Box plot of ratings by year
ggplot(books, aes(x = factor(year), y = rating)) + geom_boxplot(fill = "orange") + labs(title = "Book Ratings by Year", x = "Year", y = "Rating") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
