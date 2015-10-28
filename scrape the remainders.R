# Re-scrapes any first/last name combos that errored in the first run through scraping user data from Twitter

library(pacman)
p_load(dplyr, rvest)
csv_filenames <- list.files("scraped//")
csvs <- csv_filenames[grepl("csv", csv_filenames)]


coltypes_for_results <- c(rep("character",5), "logical", rep("character", 7), "character")

all_scraped <- bind_rows(
  lapply(csvs, function(x) {
    read.csv(paste0("scraped//", x), header = TRUE, colClasses = coltypes_for_results, stringsAsFactors = FALSE)
  }
  )
  )

all_scraped <- all_scraped %>%
  select(-15) %>% # deal with blank named variable that threw dplyr error
  mutate(fetching_error = as.logical(fetching_error))

write.csv(all_scraped, "all_scraped.csv", row.names = FALSE, na = "")
errored <- all_scraped %>%
  filter(fetching_error)

testdat <- data.frame(FIRSTNAME = errored$voter_firstname, LASTNAME = errored$voter_lastname, stringsAsFactors = FALSE) %>%
  distinct()  %>%
  mutate(FIRSTNAME = gsub(" ", "-", FIRSTNAME), LASTNAME = gsub(" ", "-", LASTNAME))