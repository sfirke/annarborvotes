source("profile_scrape_functions.R")

library(pacman)
p_load(rvest, dplyr, stringr)
set.seed(1)

# Analytics
pages_scraped_session <- 0
trace(read_html, tracer = function() pages_scraped_session <<- pages_scraped_session + 1, print = FALSE)
# untrace(read_html)

# Read voter data - matches Ann Arbor clerk's data format, YMMV
coltypes <- c("character", rep("NULL", 4), "character", rep("NULL", 2), "character", "character", "character", "character", "NULL", "character", "NULL", "character", "character", rep("NULL", 31), "character", "character", "character", rep("NULL", 5))
  

filenames <- list.files("November 2015//")
all_raw <- bind_rows(
  lapply(paste0("November 2015//", filenames), function(x) read.csv(x, header = TRUE, colClasses = coltypes))
)

# Clean voter data
all_clean <- all_raw %>%
  mutate(VOTERID = as.numeric(VOTERID), WARD = substr(WARDPRECINCT, 0, 2), PRECINCT = str_sub(WARDPRECINCT, start = -2), YOB = as.numeric(YOB), AGE = (2015 - YOB), WARD = as.numeric(WARD), PRECINCT = as.numeric(PRECINCT)) %>%
  mutate(WARD = substr(WARD, 0, 1)) %>%
  group_by(LASTNAME, FIRSTNAME) %>%
  filter(n() == 1) %>% # if there are duplicated names we can't tell which voter owns the Twitter acct
  ungroup() %>%
  mutate(FIRSTNAME = gsub(" ", "-", FIRSTNAME), LASTNAME = gsub(" ", "-", LASTNAME)) %>% # dashes cause Twitter queries to fail
  filter(!is.na(LASTNAME)) # can't query the 5 records w/o last name
         
# Scraping
testdat <- all_clean[12001:14500, ] # sample this if you want to chunk the scraping into blocks
df_length <- nrow(testdat)

# Initialize data frame for results:
username_res <- data.frame(voter_id = numeric(df_length), voter_firstname = character(df_length), voter_lastname = character(df_length), twitter_firstname = character(df_length), twitter_lastname = character(df_length), username = character(df_length), isInA2 = logical(df_length), location = character(df_length), time_last_tweet = character(df_length), num_tweets = character(df_length), num_following = character(df_length), num_followers = character(df_length), num_favorites = character(df_length), matchType = character(df_length), fetching_error = logical(df_length), stringsAsFactors = FALSE)
username_res <- as.list(seq_len(df_length))
time_per_iteration <- numeric(df_length)

# Scrape - run this and the subsequent reporting code together as one block
pages_scraped_pre_loop <- pages_scraped_session
for(i in 1:nrow(testdat)){
  starttime <- proc.time()
  print(paste0("Searching record number ", i, " of ", nrow(testdat)))
  
  username_res[[i]] <- getUserName(testdat$VOTERID[i], testdat$FIRSTNAME[i], testdat$LASTNAME[i], noisy = FALSE)
  
   time_per_iteration[i] <- (proc.time() - starttime)[3]
}

scrape_results <- bind_rows(username_res)

ans5 <- data.frame(matrix(unlist(username_res), nrow=4017, byrow=T),stringsAsFactors=FALSE)

lists_to_df <- function(x) function(i) unlist(lapply(x, `[[`, i), use.names=FALSE)
ans3 <- as.data.frame(Map(lists_to_df(username_res), names(username_res[[1]])))

# Reporting results of scrape
print(paste0("Found ", sum(scrape_results$isInA2, na.rm = TRUE), " hits; routine ended at ", Sys.time()))
summary(time_per_iteration)
print(paste0(sum(username_res$fetching_error), " fetching errors"))
print(paste0(pages_scraped_session - pages_scraped_pre_loop, " pages scraped in this loop"))

# Write results
output_file_name <- make.names(paste0("ward2_", df_length, "_rows_at_", gsub(" ", "_", Sys.time()), ".csv"))
write.csv(scrape_results, paste0("November 2015//", output_file_name), row.names = FALSE, na = "")


