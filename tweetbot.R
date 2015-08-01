library(pacman)
p_load(twitteR, devtools, base64enc, dplyr, rvest, stringr, tidyr)
set.seed(3)

# Read data from all .csv files, containing results of running scraper functions
csv_filenames <- list.files()
csvs <- csv_filenames[grepl("csv", csv_filenames)]


coltypes_for_results <- c(rep("character",5), "logical", rep("character", 7), "character")

all_scraped <- bind_rows(
  lapply(csvs, function(x) {
    read.csv(x, header = TRUE, colClasses = coltypes_for_results, stringsAsFactors = FALSE)
  }
  )
)


bucketMostRecentTweetTime <- function(time_last_tweet) {
  last_time_clean <- ifelse(time_last_tweet == "", "Never",
                            ifelse(grepl("ago", time_last_tweet), "Very recently",
                                   ifelse(grepl("20[0-9][0-9]", time_last_tweet), str_extract(time_last_tweet, "20[0-9][0-9]"),
                                          str_extract(time_last_tweet, "[A-Za-z]+") # this condition = month name, e.g. "Jul"
                                   )))
  last_time_clean
}

# Map last tweet bucket to numeric value for prioritizing tweets - see bucketMostRecentTweetTime()
assignNumberForTweetRecency <- function(time_bucket) {
  current_month <- as.numeric(str_extract(str_extract(Sys.Date(), "-[0-9][0-9]-"), "[0-9][0-9]"))
  month_crosswalk <- current_month - 1:12
  month_crosswalk <- ifelse(month_crosswalk < 0, month_crosswalk + 12, month_crosswalk)
  plyr::mapvalues(time_bucket, from = c("Very recently", month.abb, 2006:2020, "Never"), to = c(-1, month_crosswalk, 2006:2020, 10000))
}


# Filter for only those with Twitter names in A2, order with most active at top of data.frame
targets <- all_scraped %>%
  select(-15) %>% # drop unnamed variable causing dplyr problems
  filter(isInA2) %>%
  distinct(username) %>%
  mutate(last_tweet_time_bucket = bucketMostRecentTweetTime(time_last_tweet),
         last_tweet_numeric_priority = as.numeric(assignNumberForTweetRecency(last_tweet_time_bucket)),
         num_tweets = gsub(".[0-9]K", "000", num_tweets)) %>%
  arrange(last_tweet_numeric_priority, -extract_numeric(num_tweets)) %>%
  left_join(., all_no_dupe_names %>% select(FIRSTNAME, LASTNAME, WARD, PRECINCT), by = c("voter_firstname" = "FIRSTNAME", "voter_lastname" = "LASTNAME"))


treatment <- targets %>%
  sample_frac(size = 0.5) %>%
  arrange(last_tweet_numeric_priority, -extract_numeric(num_tweets))

control <- anti_join(targets, treatment) %>%
  arrange(last_tweet_numeric_priority, -extract_numeric(num_tweets))

# Insert your own Twitter credentials here
setup_twitter_oauth( ... )

# Initialize data.frame for storing sent tweets
tweetsSent <- data.frame(username = as.character(rep(NA, nrow(treatment))), message = as.character(rep(NA, nrow(treatment))), timestamp = .POSIXct(character(nrow(treatment))), stringsAsFactors =FALSE)

tweetAt <- function(username) {
  tweet_stems <- c(" registered to vote in Ann Arbor? Don't forget to vote in this Tuesday's hotly-contested city council election!", " are you an Ann Arbor voter? Remember to vote in the upcoming city council election on Tuesday August 4th!", " registered to vote in Ann Arbor? Here's a reminder to vote in the city council primary on Tuesday, August 4th!", " registered to vote in Ann Arbor? Strengthen our democracy by voting in the city council primary on Tuesday, August 4th!")
  this_stem <- sample(tweet_stems, 1)
  tweet_text <- paste0("@", username, this_stem)
  print(tweet_text)
  if(nchar(tweet_text) > 140){
    print("tweet > 140 characters, not sent")
  }else{
    tweet(tweet_text)
    }
  #return
  this_stem
}

firstEmptyRow <- function(df) {
  row_to_write <- min((1:nrow(df))[is.na(df[,1])])
  row_to_write
}

# Send the tweets in a loop
batch_size <- 54
mins_to_sleep <- 11

for(i in 1:batch_size) {
  untweeted_at <- treatment %>% filter(! username %in% tweetsSent$username)
  tweet_target <-  untweeted_at$username[i]
  tweet_used <- tweetAt(tweet_target)
  row_to_write <- firstEmptyRow(tweetsSent)
  tweetsSent[row_to_write, "username"] <- tweet_target
  tweetsSent[row_to_write, "message"] <- tweet_used
  tweetsSent[row_to_write, "timestamp"] <- Sys.time()
  time_to_sleep <- 60*(mins_to_sleep + rnorm(1, 0, mins_to_sleep/10))
  write.csv(tweetsSent, "tweetsSent.csv", na = "", row.names = FALSE)
  write.csv(untweeted_at, "untweeted_at.csv", na = "", row.names = FALSE)
  print(paste("Tweet number", i, "of", batch_size, "sent to", tweet_target, "at", Sys.time()))
  if(i != batch_size){
    print(paste("Waiting until", (Sys.time() + round(time_to_sleep)), "to send next tweet..."))
    print(paste("Projected end time: ", (Sys.time() + (batch_size-i)*mins_to_sleep*60 + time_to_sleep)))
    Sys.sleep(time_to_sleep)
    }
  else{print("DONE")}
}
