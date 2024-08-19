sentiment_polarity <- function(tweet){
  # Preprocess the tweet text using the `process_tweet` function 
  tweets <- process_tweet(tweet)$tweet

  # Convert the preprocessed tweet text to a data.frame with a 'text' column
  tweets <- data.frame(text = tweets)

  # Add a 'sentiment' column to the data.frame using the `sentiment_by` function 
  sentiments <- tweets %>%
    dplyr::mutate(sentiment = sentiment_by(text))

  # Extract the average sentiment score for each tweet
  s2 <- sentiments$sentiment$ave_sentiment

  senti = group = term <- c()

  # Create a data.table with term, sentiment score, and sentiment group (positive/negative)
  dt3 <- data.table(
    term = 1:length(s2),  # Term for each sentiment score (corresponds to tweet index)
    senti = s2,           # Sentiment score for each tweet
    group = c(0, 1)        # Initial group (will be assigned based on sentiment)
  )

  # Assign sentiment group based on score: 0 for negative, 1 for positive
  dt3[senti < 0, group := 0]  # Assign 0 to negative sentiment scores
  dt3[senti > 0, group := 1]  # Assign 1 to positive sentiment scores

  # Convert the group variable to a factor for plotting
  dt3[, group := as.factor(group)]

  # Create a ggplot object to visualize sentiment polarity
  ggplot(dt3, aes(x = term, y = senti, fill = group)) +
    geom_bar(stat = "identity") +  # Use identity stat to represent raw sentiment scores
    scale_fill_manual(values = c("0" = "green", "1" = "red"), name = "Sentiment Score") +
    ggtitle("Sentiment Polarity:") +
    xlab("Tweet") +  # Label x-axis as "Tweet" (assuming tweets are analyzed individually)
    ylab("Sentiment Score")
}