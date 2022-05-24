
#install.packages("magick", verbose=TRUE)
#install.packages("RWeka")

# Libraries
library(readr)        # reads in CSV
library(ggplot2)      # plot library
library(tidyverse)    # for data manipulation
#library(gridExtra)    # multiple plots in 1
library(scales)       # show the colors
#library(ggrepel)      # for graph repel (labels)
#library(repr)         # resize graphs
#library(hexbin)       # for hive scatter
library(naniar)       # to check for missing data
library(lubridate)    # for date and time
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)     # text preprocessing
library(textdata)     # text preprocessing
library(reshape2)
library(knitr)
library(grid)
library(igraph)
library(ggraph)
library(ggsci)
library(devtools)
library(circlize)
library(radarchart)
library(stringr)
#library(sjmisc)
#library(magick)
library(htmlwidgets)
library(VIM)          # missing values visual
library(colorspace)   # maybe for wordcloud
library(RWeka)
library(textmineR)

data <- read_csv("/Users/rookie/Desktop/tweet.csv",
                 col_types = cols(default = col_character(),
                             X1 = col_double(),
                            tweet_id = col_double(),
                            retweet_count = col_double(),
                            user_id = col_double(),
                            user_followers_count = col_double(),
                            user_friends_count = col_double(),
                            user_favourites_count = col_double(),
                            user_verified = col_double(),
                            user_statuses_count = col_double(),
                            failed = col_double(),
                            Unnamed = col_double()))
data %>% head(5)

worldcities <- read_csv("/Users/rookie/Desktop/worldcitiespop.csv",
                        col_types = cols(Country = col_character(),
                                         City = col_character(),
                                         AccentCity = col_character(),
                                         Region = col_character(),
                                         Population = col_double(),
                                         Latitude = col_double(),
                                         Longitude = col_double()))
aggr(data)
cleanCorpus <- function(text){
  # punctuation, whitespace, lowercase, numbers
  text.tmp <- tm_map(text, removePunctuation)
  text.tmp <- tm_map(text.tmp, stripWhitespace)
  text.tmp <- tm_map(text.tmp, content_transformer(tolower))
  text.tmp <- tm_map(text.tmp, removeNumbers)
  
  # removes stopwords
  stopwords_remove <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  text.tmp <- tm_map(text.tmp, removeWords, stopwords_remove)
  
  return(text.tmp)
}


##introduce the sentiment packages 
afinn <- read_csv("/Users/rookie/Desktop/Afinn.csv",
                  col_types = cols(word = col_character(), value = col_double()))
bing <- read_csv("/Users/rookie/Desktop/Bing.csv",
                 col_types = cols(word = col_character(), sentiment = col_character()))
nrc <- read_csv("/Users/rookie/Desktop/NRC.csv",
                col_types = cols(word = col_character(), sentiment = col_character()))


tweets_location <- data %>%
  # convert to lower case
  mutate(user_location = tolower(user_location)) %>%
  group_by(user_location) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n))

# Create a new column and fill it with NA
tweets_location$country <- NA

# US Cities
us_cities <- worldcities %>%
  filter(Country == "us") %>%
  mutate(Country = "US") %>%
  select(Country, City, AccentCity)

us_cities

# Cross locations with cities to extract the country
tweets_location$flag_us <- purrr::map_df(tweets_location, ~ .x %in% us_cities$City)$user_location
head(tweets_location)

data <- data %>%
  left_join(tweets_location, by = "user_location") %>%
  select(-c(n, flag_us))


data$created_atWeekday = gsub( " .*$", "", data$created_at )
created_atA = sub("^\\S+\\s+", '', data$created_at)
data$created_atMonth = gsub( " .*$", "", created_atA )
library(stringi)
data$created_atYear = stri_extract_last_regex(created_atA, "\\S+")

##add seasonality   
data$season[data$created_atMonth %in% c('Dec', 'Jan', 'Feb')]<- "Winter"
data$season[data$created_atMonth %in% c('Mar', 'Apr', 'May')]<- "Spring"
data$season[data$created_atMonth %in% c('Oct', 'Nov', 'Sep')]<- "fall"
data$season[data$created_atMonth %in% c('Jun', 'Jul', 'Aug')]<- "Summer"

##
data %>% count(created_atYear,sort=TRUE)
data %>% count(season,sort=TRUE)

##drop unnecessary columns
drop <- c("failed","user_verified",'unnamed:0','user_background_color')

data = data[,!(names(data) %in% drop)]
head(data)

unnest_tweets <- data %>% 
  mutate(text = as.character(data$text)) %>% 
  unnest_tokens(word, text)

# Create a dataframe with stopwords
stopwords_script <- tibble(word = c(stopwords("en"), c("thats","weve","hes","theres","ive","im", "will","can","cant","dont","youve","us", "youre","youll","theyre","whats","didnt", "just")))

 ##set up color palette
 # Custom Color Palette
 my_colors <- c("#05A4C0", "#85CEDA", "#D2A7D8", "#A67BC5", "#BB1C8B", "#8D266E")
 show_col(my_colors, labels = F, borders = NA)
 
 
 # Custom Theme Variable
 my_theme <- theme(#plot.background = element_rect(fill = "grey98", color = "grey20"),
                   #panel.background = element_rect(fill = "grey98"),
                   #panel.grid.major = element_line(colour = "grey87"),
                   #text = element_text(color = "grey20"),
                   plot.title = element_text(size = 22),
                   plot.subtitle = element_text(size = 17),
                   axis.title = element_text(size = 15),
                   axis.text = element_text(size = 15),
                   #legend.box.background = element_rect(color = "grey20", fill = "grey98", size = 0.1),
                   #legend.box.margin = margin(t = 3, r = 3, b = 3, l = 3),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 15),
                   strip.text = element_text(size=17))
 
 ##sentiment analysis
 options(repr.plot.width=15, repr.plot.height=15)
 
 unnest_tweets %>% 
    inner_join(bing, by="word") %>%
    count(word, sentiment, sort=T) %>% 
    acast(word ~ sentiment, value.var = "n", fill=0) %>% 
    
# wordcloud# positive red vs negative blue
comparison.cloud(colors=my_colors[c(5, 1)], max.words = 400, title.size = 2,scale = c(3,.5))
 
 
##investigate twitter accounts
 options(repr.plot.width=15, repr.plot.height=9)
 labels <- c("user_favourites" = "No. Favourites", "user_followers" = "No. Followers", 
             "user_friends" = "No. Friends")
 
 data %>%
   select(user_followers_count, user_favourites_count, user_friends_count) %>%
   gather(key = group_var, value = "Cases", na.rm = TRUE) %>%
   
   ggplot(aes(x = Cases)) +
   geom_boxplot(aes(fill = group_var), outlier.fill = "grey35", outlier.shape = 18, 
                outlier.alpha = 0.1, outlier.size = 2) +
   facet_grid(~ group_var, scales = "free", labeller = as_labeller(labels)) +
   scale_x_continuous(labels = comma) +
   scale_fill_manual(values = my_colors, guide = "none") +
   labs(title = "User Profile", subtitle = "Profile Size") +
   my_theme + theme(axis.text.y = element_blank(),
                    axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
                    axis.title = element_blank())
 
 options(repr.plot.width=15, repr.plot.height=7)

 
 # The plot:## overall mood in the tweets
 unnest_tweets %>% 
   inner_join(nrc, "word") %>%
   filter(!sentiment %in% c("positive", "negative")) %>% 
   count(sentiment, sort=T) %>% 
   
   ggplot(aes(x=reorder(sentiment, n), y=n)) +
   geom_bar(stat="identity", aes(fill=n), show.legend=F) +
   geom_label(aes(label=format(n, big.mark = ",")), size=5, fill="white") +
   labs(x="Sentiment", y="Frequency") +coord_flip()+
   scale_fill_gradient(low = my_colors[3], high = my_colors[1], guide="none") +
  theme(axis.text.x = element_blank())+my_theme+theme(panel.border = element_blank(),
                                                       panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(),
                                                      panel.background = element_blank())
 
 options(repr.plot.width=15, repr.plot.height=9)
 
 unnest_tweets %>% 
   inner_join(nrc, "word") %>% 
   count(sentiment, word, sort=T) %>%
   group_by(sentiment) %>% 
   arrange(desc(n)) %>% 
   slice(1:7) %>% 
   
# Plot:##split major emotions 
   ggplot(aes(x=reorder(word, n), y=n)) +
   geom_col(aes(fill=sentiment), show.legend = F) +
   facet_wrap(~sentiment, scales = "free_y", nrow = 2, ncol = 5) +
   coord_flip() +
   my_theme + theme(axis.text.x = element_blank()) + 
   labs(x="Word", y="Frequency", title="Sentiment split by most frequent words") +
   scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7",
                                "#40BDC8", "#80D3DB", "#BFE9ED"))
 
 options(repr.plot.width=15, repr.plot.height=9)
 
 unnest_tweets %>% 
   # Count how many word per value
   inner_join(afinn, "word") %>% 
   group_by(value) %>% 
   count(value, sort=T)  %>% 
   
# Plot## distribution of negative to positive intensity
   ggplot(aes(x=value, y=n)) +
   geom_bar(stat="identity", show.legend = F, width = 0.5, fill = my_colors[1]) +
   geom_label(aes(label=format(n, big.mark = ",")), size=5) +
   scale_x_continuous(breaks=seq(-5, 5, 1)) +
   labs(x="Score", y="Frequency", title="Word count distribution over intensity of sentiment: Neg - Pos") +
    my_theme+theme(axis.text.y = element_blank())+theme(panel.border = element_blank(),
                                                        panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(),
                                                        panel.background = element_blank())
 
 ##
 # Filter only main 3 countries with most tweets
 user_location=gsub("USA",'United States',data$user_location)
 user_location=gsub("United States of America",'United States',data$user_location)
 user_location=gsub("USA",'United States',data$user_location)
 user_location=gsub("Washington, DC",'United States',data$user_location)
 user_location=gsub("New York,NY",'United States',data$user_location)
 user_location=gsub("London",'United Kingdom',data$user_location)
 user_location=gsub("UK",'United Kingdom',data$user_location)
 user_location=gsub("London, England",'United Kingdom',data$user_location)
 
 data <- unnest_tweets %>%
   filter(country %in% c("United States","United Kingdom"))
 
 # Create totals dataframe for the 3 countries
 total_bing <- data %>% 
   inner_join(bing, by="word") %>%
   count(user_location) %>% 
   group_by(user_location) %>% 
   summarise(total_tweets = sum(n), .groups = "drop_last")
 
 # The table
 # total_bing
 options(repr.plot.width=15, repr.plot.height=9)
 
 to_plot <- data %>% 
   # get 'bing' and filter the data
   inner_join(bing, by="word") %>%
   
   # sum number of words per sentiment and country
   count(sentiment, user_location) %>% 
   group_by(user_location, sentiment) %>% 
   summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
   inner_join(total_bing, by="user_location") %>% 
   mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
   select(user_location, sentiment, sentiment_perc)
 
 data <- unnest_tweets %>%
    filter(user_location %in% c("United States","United Kingdom"))
 
 char_sentiment <- data %>% 
    inner_join(nrc, "word") %>% 
    filter(!sentiment %in% c("positive", "negative")) %>% 
    group_by(user_location, sentiment) %>% 
    count(user_location, sentiment) %>% 
    select(user_location, sentiment, char_sentiment_count=n)
 
 total_char <- data %>% 
    inner_join(nrc, "word") %>% 
    filter(!sentiment %in% c("positive", "negative")) %>% 
    count(user_location) %>% 
    select(user_location, total=n)
 
 plt <- char_sentiment %>% 
    inner_join(total_char, by="user_location") %>% 
    mutate(percent = char_sentiment_count / total * 100 ) %>% 
    select(-char_sentiment_count, -total) %>% 
    spread(user_location, percent)  %>% 
    chartJSRadar(showToolTipLabel = T, main="Countries Tweets and Emotion", maxScale=25, responsive=T,
                 addDots = T, 
                 colMatrix = grDevices::col2rgb(my_colors[c(1, 5, 4, 6)]),
                 lineAlpha = 0.7, polyAlpha = 0.2)
plt

# Filter only main 3 states in the united states with most tweets

data <- unnest_tweets %>%
   filter(country %in% c("Washington, DC","New York, NY",'Los Angeles, CA'))

# Create totals dataframe for the 3 countries
total_bing <- data %>% 
   inner_join(bing, by="word") %>%
   count(user_location) %>% 
   group_by(user_location) %>% 
   summarise(total_tweets = sum(n), .groups = "drop_last")

# The table
# total_bing
options(repr.plot.width=15, repr.plot.height=9)

to_plot <- data %>% 
   # get 'bing' and filter the data
   inner_join(bing, by="word") %>%
   
   # sum number of words per sentiment and country
   count(sentiment, user_location) %>% 
   group_by(user_location, sentiment) %>% 
   summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
   inner_join(total_bing, by="user_location") %>% 
   mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
   select(user_location, sentiment, sentiment_perc)

data <- unnest_tweets %>%
   filter(user_location %in% c("Washington, DC","New York, NY",'Los Angeles, CA'))

char_sentiment <- data %>% 
   inner_join(nrc, "word") %>% 
   filter(!sentiment %in% c("positive", "negative")) %>% 
   group_by(user_location, sentiment) %>% 
   count(user_location, sentiment) %>% 
   select(user_location, sentiment, char_sentiment_count=n)

total_char <- data %>% 
   inner_join(nrc, "word") %>% 
   filter(!sentiment %in% c("positive", "negative")) %>% 
   count(user_location) %>% 
   select(user_location, total=n)

plt <- char_sentiment %>% 
   inner_join(total_char, by="user_location") %>% 
   mutate(percent = char_sentiment_count / total * 100 ) %>% 
   select(-char_sentiment_count, -total) %>% 
   spread(user_location, percent)  %>% 
   chartJSRadar(showToolTipLabel = T, main="Countries Tweets and Emotion", maxScale=25, responsive=T,
                addDots = T, 
                colMatrix = grDevices::col2rgb(my_colors[c(3, 5, 4, 2)]),
                lineAlpha = 0.7, polyAlpha = 0.2)
plt




