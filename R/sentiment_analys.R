sentiment_analys <-
function(tweet) {
  
  # Preprocess the tweet text using the `process_tweet` function 
  tweets <- process_tweet(tweet)$tweet

  # Convert the preprocessed tweet text to a data.frame with a 'text' column
  tweets <- data.frame(text = tweets)

  # Add a 'sentiment' column to the data.frame using the `sentiment_by` function 
  sentiments <- tweets %>%
    dplyr::mutate(sentiment = sentiment_by(text, sentiment = "nrc"))

  # Calculate the sentiment scores for each tweet using the 'get_nrc_sentiment' function 
  s <- syuzhet::get_nrc_sentiment(tweets$text)

  # Create a bar plot of the sentiment scores
  barplot(colSums(s),
         las = 2, # Rotate x-axis labels for better readability
         col = 'darkblue', # Set bar color
         ylab = 'Count', # Set y-axis label
         main = 'Sentiment Scores of Tweets') # Set plot title

  # Create a data.table with two columns: 'text' and 'ave_sentiment'
  # 'text' contains the original tweet text
  # 'ave_sentiment' contains the average sentiment score for each tweet

  my_sentiment <- data.table(text = sentiments$text, ave_sentiment = sentiments$sentiment$ave_sentiment)

  # Return the data.table containing the tweet text and average sentiment scores
  return(my_sentiment)
}
