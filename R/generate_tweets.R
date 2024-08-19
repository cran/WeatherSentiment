generate_tweets <- function(n) {
  # List of weather conditions
  weather_conditions <- c("sunny", "rainy", "cloudy", "stormy", "snowy", "windy")

  # List of positive phrases
  positive_phrases <- c("I love", "It's a beautiful", "Such a great", "Enjoying the", "Feeling happy with this", "Perfect weather for")

  # List of negative phrases
  negative_phrases <- c("I hate", "It's a terrible", "Such a bad", "Hating the", "Feeling down with this", "Awful weather for")

  tweets <- data.frame(
    Date = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n),
    T1 = character(n)
  )

  for (i in 1:n) {
    condition <- sample(weather_conditions, 1)
    if (runif(1) > 0.5) {
      phrase <- sample(positive_phrases, 1)
      tweet <- str_c(phrase, condition, "day!")
    } else {
      phrase <- sample(negative_phrases, 1)
      tweet <- str_c(phrase, condition, "day.")
    }
    tweets$T1[i] <- tweet
  }

  return(tweets)
}
