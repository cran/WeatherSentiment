word_cloud_tweet <- function(tweet) {
  
  # Preprocess the tweet text and convert it to a data.frame
  words <- data.frame(text = process_tweet(tweet)$tokens)
  
  # Remove words less than 3 characters
  texts_clean <- lapply(words, function(x) x[nchar(x) > 2]) # Keep words longer than 2 characters
  
  # Calculate word frequencies
  freq_data <- data.frame(word = names(table(texts_clean)), freq = as.numeric(table(texts_clean)))
  
  
  # Check if frequency data is not empty
  if(nrow(freq_data) > 0 && all(freq_data$freq > 0)) {
    # Generate the word cloud
    wordcloud(words = freq_data$word, freq = freq_data$freq, min.freq = 1,
              scale = c(5, 0.3), colors = RColorBrewer::brewer.pal(9, 'Blues')[5:9], 
              random.order = FALSE)
  } else {
    message("No valid frequency data found to generate the word cloud.")
  }
}
