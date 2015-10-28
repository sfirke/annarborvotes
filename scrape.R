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
  
filenames <- list.files("voter data//")
all_raw <- bind_rows(
  lapply(paste0("voter data//", filenames), function(x) read.csv(x, header = TRUE, colClasses = coltypes))
)

# Process voter data

all_clean <- all_raw %>%
  mutate(VOTERID = as.numeric(VOTERID), WARD = substr(WARDPRECINCT, 0, 2), PRECINCT = str_sub(WARDPRECINCT, start = -2), YOB = as.numeric(YOB), AGE = (2015 - YOB), WARD = as.numeric(WARD), PRECINCT = as.numeric(PRECINCT)) %>%
  mutate(WARD = substr(WARD, 0, 1)) # due to ward being 3, 4, 5, 10, 20 at this point

# Filter out permanent absentee voters - don't want to target those who may have mailed in ballots
all_no_permaAVs <- all_clean %>%
  filter(PERMAVIND == "N")

# filter out duplicated names since it would be unclear which voter owned the Twitter account
dupe_names <- all_no_permaAVs %>%
  group_by(LASTNAME, FIRSTNAME) %>%
  summarise(samename = n()) %>%
  ungroup() %>%
  filter(samename > 1)

all_no_dupe_names <- anti_join(all_no_permaAVs, dupe_names, by = c("LASTNAME", "FIRSTNAME")) %>%
  mutate(FIRSTNAME = gsub(" ", "-", FIRSTNAME), LASTNAME = gsub(" ", "-", LASTNAME)) # remove the spaces that cause Twitter queries to fail

# Filter into wards with primaries
ward1 <- all_no_dupe_names %>%
  filter(WARD == 1)

ward3 <- all_no_dupe_names %>%
  filter(WARD == 3)

ward4 <- all_no_dupe_names %>%
  filter(WARD == 4)

ward5 <- all_no_dupe_names %>%
  filter(WARD == 5)


# Voter records to scrape - my machine runs out of RAM after about 3,000 records so that's my block size, YMMV
testdat <- ward5[7501:10000, ] # use this to cycle through blocks of voters
df_length <- nrow(testdat)

# Initialize data frame for results:
username_res <- data.frame(voter_firstname = character(df_length), voter_lastname = character(df_length), twitter_firstname = character(df_length), twitter_lastname = character(df_length), username = character(df_length), isInA2 = logical(df_length), location = character(df_length), time_last_tweet = character(df_length), num_tweets = character(df_length), num_following = character(df_length), num_followers = character(df_length), num_favorites = character(df_length), matchType = character(df_length), fetching_error = logical(df_length), stringsAsFactors = FALSE)

username_res <- as.list(seq_len(df_length))

time_per_iteration <- numeric(df_length)

# Scrape - run this and the subsequent reporting code together as one block

for(i in 1:nrow(testdat)){
  starttime <- proc.time()
  print(paste0("Searching record number ", i, " of ", nrow(testdat)))
  
  username_res[[i]] <- getUserName(testdat$FIRSTNAME[i], testdat$LASTNAME[i], noisy = TRUE)
  
   time_per_iteration[i] <- (proc.time() - starttime)[3]
}

scrape_results <- bind_rows(username_res)

# Reporting results of scrape
print(paste0("Found ", sum(scrape_results$isInA2, na.rm = TRUE), " hits; routine ended at ", Sys.time()))
summary(time_per_iteration)
print(paste0(sum(username_res$fetching_error), " fetching errors"))
output_file_name <- make.names(paste0("ward3_", df_length, "_rows_at_", gsub(" ", "_", Sys.time()), ".csv"))
write.csv(username_res, output_file_name, row.names = FALSE, na = "")


