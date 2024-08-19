process_tweet <- function(tweet) {
  # Convert data to a tibble
  if (is.character(tweet)) {
    tweets_df <- tidyr::tibble(tweet = tweet)
  } else {
    # Handle data frame input (unchanged)
    tweets_df <- tweet
  }

  # Convert tweet text to lowercase
  tweets_df[[1]]<- tolower(tweets_df[[1]])

  # Remove punctuation
  tweets_df[[1]] <- gsub("[^[:alnum:]\\s]", " ", tweets_df[[1]])

  # Remove extra spaces
  tweets_df[[1]] <- gsub("\\s+", " ", tweets_df[[1]])

  # Remove empty tweets
  tweets_df <- tweets_df %>%
    filter(nchar(tweet) > 0)

  # Tokenize tweets (apply str_split to the tweet column)
  tweets_df[[1]] <- str_split(tweets_df[[1]], " ")

  # Remove stop words
  tweets_df[[1]] <- lapply(tweets_df[[1]], function(x) x[!x %in% tidytext::stop_words])
 
  # Remove short tweets
  tweets_df[[2]] <- lapply(tweets_df[[1]], function(x) x[nchar(x) > 2])

  ## Concatenate all tweets in the first column of 'tweets_df' into a single string with spaces as separators.
  tweets_df[[1]]  <- lapply(tweets_df[[2]], function(x)paste(x,sep=' ',collapse=' '))

  # Now return the entire preprocessed data
  mylist <- list(tweets = unlist(tweets_df[[1]]), tokens = unlist(tweets_df[[2]]))
  return(mylist)
}
