
read_tweets <- function(file) {
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()),
                 show_col_types = FALSE, progress = FALSE)
}
clean_tweets <- function(data) {
  required <- c("tweet_id", "airline", "text")
  if (!is.data.frame(data) || !all(required %in% names(data)))
    stop("data must contain tweet_id, airline, and text columns.", call. = FALSE)
  tweets <- data |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::distinct(.data$tweet_id, .keep_all = TRUE) |>
    dplyr::filter(!is.na(.data$text), !is.na(.data$airline),
                  stringr::str_trim(.data$text) != "") |>
    dplyr::mutate(doc_id = dplyr::row_number())
  tweets |>
    dplyr::mutate(
      clean_text = stringr::str_to_lower(.data$text, locale = "en"),
      clean_text = stringr::str_replace_all(.data$clean_text, "https?://\\S+|www\\.\\S+", ""),
      clean_text = stringr::str_replace_all(.data$clean_text, "@\\w+", " "),
      clean_text = stringr::str_replace_all(.data$clean_text, "&amp;", " and "),
      clean_text = stringr::str_replace_all(.data$clean_text, "&[a-z]+;", " "),
      clean_text = stringr::str_replace_all(.data$clean_text, "\u2019", "'"),
      clean_text = stringr::str_replace_all(.data$clean_text, "[^a-z\\s']", " "),
      clean_text = stringr::str_squish(.data$clean_text))
}
analyze_tweets <- function(data, stop_words = tidytext::stop_words$word) {
  if (is.character(data) && length(data) == 1L) data <- read_tweets(data)
  if (!is.character(stop_words) || anyNA(stop_words))
    stop("stop_words must be a character vector without NA.", call. = FALSE)
  tweets <- clean_tweets(data)
  tokens <- tweets |>
    dplyr::select(dplyr::all_of(c("doc_id", "airline", "clean_text"))) |>
    tidytext::unnest_tokens(word, clean_text)
  words <- tokens |>
    dplyr::filter(!.data$word %in% stop_words,
                  stringr::str_detect(.data$word, "[a-z]"), .data$word != "rt")
  word_freq <- dplyr::count(words, .data$word, sort = TRUE)
  bigrams <- tweets |>
    dplyr::select(dplyr::all_of(c("doc_id", "airline", "clean_text"))) |>
    tidytext::unnest_tokens(bigram, clean_text, token = "ngrams", n = 2) |>
    tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |>
    dplyr::filter(!is.na(.data$word1), !is.na(.data$word2),
                  !.data$word1 %in% stop_words, !.data$word2 %in% stop_words,
                  .data$word1 != "rt", .data$word2 != "rt") |>
    tidyr::unite(bigram, word1, word2, sep = " ")
  bigram_freq <- dplyr::count(bigrams, .data$bigram, sort = TRUE)
  bow_counts <- dplyr::count(words, .data$doc_id, .data$word, name = "n")
  if (nrow(bow_counts)) {
    bow_matrix <- tidytext::cast_sparse(bow_counts, doc_id, word, n)
    tweet_tfidf <- bow_counts |>
      tidytext::bind_tf_idf(word, doc_id, n) |>
      dplyr::arrange(dplyr::desc(.data$tf_idf))
  } else {
    bow_matrix <- Matrix::sparseMatrix(i = integer(), j = integer(), dims = c(0L, 0L))
    tweet_tfidf <- dplyr::mutate(bow_counts, tf = numeric(), idf = numeric(), tf_idf = numeric())
  }
  list(tweets = tweets, airline_counts = dplyr::count(tweets, .data$airline, sort = TRUE),
       tokens = tokens, words = words, word_freq = word_freq,
       bigrams = bigrams, bigram_freq = bigram_freq, bow_counts = bow_counts,
       bow_matrix = bow_matrix, tweet_tfidf = tweet_tfidf)
}
utils::globalVariables(c("word", "clean_text", "bigram", "word1", "word2", "doc_id", "n"))
