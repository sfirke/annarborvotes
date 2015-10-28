
# if no hits for full name, try just last name, in Ann Arbor
#
#   if(length(username) == 0){
#  urlToGet2 <- paste0("https://twitter.com/search?q=", lastname, "&near=me&src=typd&vertical=default&f=users") 
#  res <- html(urlToGet2)
#  
#  username <- res %>%
#    html_nodes(".u-linkComplex-target") %>%
#    html_text
#  
#  print(paste0("Geographical search near you using only last name returned ", length(username), " account hits"))
#  
#  if(length(username) > 0 && length(username) < 18) {matchType <- "LastOnlyNearYou"}
#  }
