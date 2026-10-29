\name{analyze_tweets}
\alias{analyze_tweets}
\alias{read_tweets}
\alias{clean_tweets}
\title{Analyze Airline Tweet Text}
\description{Clean English tweet text and calculate word counts, bigrams and TF-IDF.}
\usage{
read_tweets(file)
clean_tweets(data)
analyze_tweets(data, stop_words = tidytext::stop_words$word)
}
\arguments{
\item{file}{Local CSV path or URL.}
\item{data}{Data frame with tweet_id, airline and text columns. analyze_tweets also accepts a CSV path or URL.}
\item{stop_words}{Character vector of excluded words.}
}
\value{read_tweets and clean_tweets return tibbles. analyze_tweets returns a list containing tweets, airline_counts, tokens, words, word_freq, bigrams, bigram_freq, bow_counts, bow_matrix and tweet_tfidf.}
\details{First duplicate ID wins before filtering. Cleaning retains English ASCII letters and apostrophes. Documents without retained words are omitted from the matrix and TF-IDF. Matrix row names map to doc_id. Empty vocabularies return a zero by zero matrix. No sentiment scoring is performed. Local CSV analysis needs no internet.}
\examples{
x <- data.frame(tweet_id = c("1", "2"), airline = c("A", "B"),
                text = c("Great friendly crew", "Delayed flight"))
analyze_tweets(x)$word_freq
}
