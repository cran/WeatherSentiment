corr_analys <-
function(t, w, com_var = "Date", var1 = "T1", var2 = "T2") {

  # Merge the tweet and weather data based on the common variable
  merged_data <- merge(t, w, by = com_var)

  # Extract sentiment scores from the tweet text
  merged_data$sentiment_score <- sentiment(get(var1, merged_data))

  # Calculate the Pearson correlation coefficient between sentiment scores and the weather variable
  pearson_correlation <- cor(merged_data$sentiment_score$sentiment, get(var2, merged_data))

  # Return the correlation coefficient
  return(pearson_correlation)
}
