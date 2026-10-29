library(airlineText)
x <- data.frame(tweet_id = c("1", "1", "2", "3"),
                airline = c("A", "A", "B", "B"),
                text = c("@air Great crew! https://example.com", "duplicate",
                         "Great flight", "the and"))
r <- analyze_tweets(x)
stopifnot(nrow(r$tweets) == 3L, r$tweets$clean_text[1] == "great crew",
          r$word_freq$n[r$word_freq$word == "great"] == 2L,
          sum(r$bow_matrix) == sum(r$bow_counts$n),
          "great crew" %in% r$bigram_freq$bigram)
e <- analyze_tweets(x[0, ])
stopifnot(nrow(e$tweets) == 0L, all(dim(e$bow_matrix) == 0L))
z <- analyze_tweets(x[4, ])
stopifnot(nrow(z$words) == 0L, nrow(z$tweet_tfidf) == 0L)
f <- tempfile(fileext = ".csv")
write.csv(x, f, row.names = FALSE)
stopifnot(identical(analyze_tweets(f)$word_freq, r$word_freq))
unlink(f)
