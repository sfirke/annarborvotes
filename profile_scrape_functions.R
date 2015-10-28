library(pacman)
p_load(rvest, dplyr, stringr)
set.seed(1)

# To Do - Scraping:
## Log method of identification, (a) for analysis of algorithm efficacy and (b) for post-hoc data treatments
### E.g., how often does grinding through 18 names yield an A2 match?  How about the last-name-only search that yields relatives?  Could clean those specially
## If there's a single hit for first and last but no geographical match, cache that but also keep looking in case a different search does yield a geo match?


getUserName <- function(firstname, lastname, noisy = TRUE) {
  
  firstname <- as.character(firstname) # in case a factor is passed as the argument 
  lastname <- as.character(lastname) # in case a factor is passed as the argument
  errored <- TRUE
  success <- FALSE
  username <- as.character(NA)
  isInA2 <- as.logical(NA)
  user_loc_string <- as.character(NA)
  user_first <- as.character(NA)
  user_last <- as.character(NA)
  time_last_tweet <- as.character(NA)
  num_tweets <- as.character(NA)
  num_following <- as.character(NA)
  num_followers <- as.character(NA)
  num_favorites <- as.character(NA)
  matchType <- "No Match"
  
  tryCatch(
    { # try
      
      username_2_names <- search_usernames(lastname, firstname)
      username <- username_2_names
  
      if(length(username) > 0) { matchType <- "FirstLastNoGeo"}
      if(noisy){print(paste0("Initial search for ", firstname, " ", lastname, " returned ", length(username), " account hits"))}
      
      # if 18 usernames (max result from first page), try adding geography
      if(length(username) == 18){
        username_local <- search_usernames(lastname, firstname, proximity = TRUE)
        username <- username_local
        
        if(noisy){print(paste0("Geographical search for exact name returned ", length(username), " account hits"))}
        if(length(username) > 0 && length(username) < 18) {matchType <- "FirstLastNearYou"}
        
        # if that narrowed it to zero, abandon the narrowing and reboot to original search
        if(length(username_local) == 0){
          username <- username_2_names
          if(noisy){print("Reverting to initial search results")}
          if(length(username) > 0 && length(username) < 18) {matchType <- "FirstLastNoGeo"}
          
        }
      }
      
    
      # Geographical search for last-name only as a last resort. Handles all of the people named "SAMUEL" on the voter rolls and "Sam" on Twitter - but will need to be manually cleaned as it mostly gets the kids of older voters, which without cleaning will yield mis-targeted tweets and noise in the experiment results.
      if(length(username) == 0 | length(username) == 18){
        if(length(username) == 18 & noisy){print("Too many results on full search, trying last name only locally")}
        username_surname_local <- search_usernames(lastname, firstname = NA, proximity = TRUE)
        username <- username_surname_local
        if(noisy){print(paste0("Geographical search for last name only returned ", length(username), " account hits"))}
        if(length(username) > 0 && length(username) < 18) {matchType <- "LastNameOnlyLocal"}
        
      }
      
      
      # if multiple hits for full name, query each and look for location = Ann Arbor
      # may need to bail on queries with huge # of results to check
      
      if(length(username) > 1 & length(username) < 18){ # NOTE THIS NOW BAILS ON 18, AS THAT INDICATES MANY MORE THAN 18
        
        if(noisy){print("Trying to resolve multiple matches by searching profiles for \"Ann Arbor\"")}
        
        foundHit = FALSE
        for(i in seq_along(username)) {
          tempUserInfo <- getUserInfo(username[i])
          if(!foundHit && locationMatches(tempUserInfo$user_loc, "Ann Arbor")){
            username <- username[i]
            if(noisy){print("Match found")}
            foundHit <- TRUE
            isInA2 <- TRUE
            user_loc_string <- tempUserInfo$user_loc
            user_first <- tempUserInfo$user_first
            user_last <- tempUserInfo$user_last
            time_last_tweet <- tempUserInfo$time_last_tweet
            break
          }
        }  
        success <- foundHit
      }
      
      if(length(username) == 1){ success <- TRUE}
      
    
      ifelse(success, {
        #    if(noisy){print(paste0("username = ", username))}
        tempUserInfo <- getUserInfo(username)
        user_loc_string <- tempUserInfo$user_loc
        user_first <- tempUserInfo$user_first
        user_last <- tempUserInfo$user_last
        isInA2 <- locationMatches(user_loc_string, "Ann Arbor")
        time_last_tweet <- tempUserInfo$time_last_tweet
        num_tweets <- tempUserInfo$num_tweets
        num_following <- tempUserInfo$num_following
        num_followers <- tempUserInfo$num_followers
        num_favorites <- tempUserInfo$num_favorites
        #    if(noisy){print(paste0("confirmed Ann Arbor location = ", isInA2))}
      }, {
        if(noisy){print("no username found")}
        username <- NA
        matchType <- "No Match"
        
      }
      )
      errored <- FALSE
      
    }, # end try
    error=function(cond) {
      message(paste("Error encountered when querying", firstname, lastname))
      message("Here's the original error message:")
      message(cond)
    }
  ) # end tryCatch
  
  ret <- data.frame(voter_firstname = firstname, voter_lastname = lastname, twitter_firstname = user_first, twitter_lastname = user_last, username, isInA2, location = user_loc_string, time_last_tweet, num_tweets, num_following, num_followers, num_favorites, matchType, fetching_error = errored, stringsAsFactors = FALSE)
  
  ret
  
}


# Fetches search results for a username query to Twitter, returns list of usernames
# Could have this take a parameter for location instead of hard-coding Ann Arbor - but low priority for me
search_usernames <- function(lastname, firstname = NA, proximity = FALSE) {
  name_string <- ifelse(is.na(firstname), lastname, paste0(firstname, "%20", lastname))
  if(proximity){
    urlToGet <- capture.output(cat("https://twitter.com/search?f=users&vertical=default&q=\"", name_string, "\"%20near%3A%22Ann%20Arbor%2C%20MI%22%20within%3A15mi&src=typd", sep = ""))
    }else{
      urlToGet <- capture.output(cat("https://twitter.com/search?f=users&vertical=default&q=\"", name_string, "\"&src=typd", sep = ""))
    }
  res <- read_html(urlToGet)
  usernames <- res %>%
    html_nodes(".u-linkComplex-target") %>%
    html_text
  usernames
}

# Takes Twitter username, returns their location string, firstname, lastname
getUserInfo <- function(username) {
  
  urlToGet <- paste0("https://twitter.com/", username) 
  res <- read_html(urlToGet)
  
  user_location_raw <- res %>%
    html_nodes(".ProfileHeaderCard-locationText") %>%
    html_text
  user_loc <- str_trim(user_location_raw)
  
  
  user_name_clean <- res %>%
    html_nodes(".ProfileNameTruncated-link") %>%
    html_text %>%
    .[1] %>%
    str_trim
  
  user_names <- unlist(str_split(user_name_clean, " "))
  user_first <- user_names[1]
  user_last <- user_names[length(user_names)]
  
  time_last_tweet <- res %>%
    html_nodes(".tweet-timestamp") %>%
    html_text %>%
    .[1]
  
  activity <- res %>%
    html_nodes(".ProfileNav-value") %>%
    html_text
  
  num_tweets <- activity[1]
  num_following <- activity[2]
  num_followers <- activity[3]
  num_favorites <- activity[4]
  
  user_info <- data.frame(user_first, user_last, user_loc, time_last_tweet, num_tweets, num_following, num_followers, num_favorites, stringsAsFactors = FALSE)
  #return
  return(user_info)
}

# Takes a location string to search and a name to search for, returns whether location string contains target substring (case-insensitive)
locationMatches <- function(stringtosearch, placename) {
  return(str_detect(tolower(stringtosearch), tolower(placename)))
}

