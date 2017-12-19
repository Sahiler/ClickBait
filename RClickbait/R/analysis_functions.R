#'Counts the occurences of keywords from the keyword vector for each item.
#'
#'@param titles A list of character vectors.
#'@return A double list with the keyword count for each item.
#'@export
keyword_count = function(titles){
  counts = c()
  for(i in 1:length(titles)){
    count = 0
    title = titles[[i]]
    for(j in 1:length(keywords)){
      count = count + length(grep(keywords[[j]], title))
    }
    counts[i] <- count
  }
  return(counts)
}
#'Calculates the sentiment of each string.
#'
#'@param strings A list of strings.
#'@return An integer list of the sentiment scores of each string.
#'@export
calculate_sentiment = function(strings){
  sentiment_scores = calculate_score(strings)
  for( i in 1:length(sentiment_scores)){
    if(sentiment_scores[[i]] > 10){
      sentiment_scores[[i]] <- -1;
    }
  }
  return(sentiment_scores)
}
#'Tokenizes articles.
#'
#'@param articles A list of articles.
#'@return A list of VCorpus objects based on the articles.
tokenize = function(articles){
  articles <- lapply(articles,tolower)
  corpusss <- VCorpus(VectorSource(articles))
  return(corpusss)
}
#'Removes stop words from VCorpus objects.
#'
#'@param articles A list of vcorpus objects.
#'@return VCorpus objects with the stop words removed from them.
remove_stop_words = function(articles){
  articles <- tm_map(articles, removeWords, stopwords("english"))
  return(articles)
}
#'Removes punctuation from words in a VCorpus object.
#'
#'@param strings A list of VCorpus objects.
#'@return A list of character vectors stripped of punctuation.
parse_punctuation = function(strings){
  tokenized = list()
  for(i in 1:length(strings)){
    tokened <- as.character(inspect(strings[[i]]))
    newstring <- strsplit(gsub("[^[:alnum:] ]", "", tokened), " +")
    tokenized[[i]] <- newstring[[1]]
  }
  tokenized[length(tokenized)>0]
}
#'Gets the significant words from a string.
#'
#'@param titles A list of strings.
#'@return A list of signifcant words correpsonding to each string.
get_sig_words = function(titles){
  tokenized = tokenize(titles);
  tokenized <- remove_stop_words(tokenized);
  key_words = parse_punctuation(tokenized);
  return(key_words)
}
#'Counts the number of significant words that appear in an article.
#'
#'@param sig_words A determined list of signicant words for each article.
#'@param articles A list of articles.
#'@return A list of the number of signicant words in each article.
count_sig_words = function(sig_words, articles){
  word_count = c()
  articles = lapply(articles, tolower)
  for(i in 1:length(articles)){
    words = 0
    article = articles[[i]]
    for(j in 1:length(sig_words[[i]])){
      key_word=sig_words[[i]][[j]]
      if(nchar(key_word)<2){
        next()
      }
      tryCatch({
        words = words + length(strsplit(article,key_word)[[1]])+1
      },error=function(e){
        ##do nothing
      })
    }
    word_count[[i]] <- words
  }
  word_count
}
#'Calculates the score of articles from a csv of articles.
#'
#'@param csv A data frame of articles with the title attribute and text attribute.
#'@param titles A list of titles.
#'@param texts A list of article texts.
#'@return A list of the scores of each article.
#'@usage
#'determine_score(csv)
#'determine_score(titles,texts)
#'@export
determine_score = function(csv){
  keywords = keyword_count(csv$title)
  sentiments = calculate_sentiment(csv$title)
  sig_words = get_sig_words(csv$title)
  article_lengths = nchar(as.character(csv$text))
  word_count = count_sig_words(sig_words, csv$text)
  return(word_count / log2(article_lengths) + abs(sentiments) * keywords * 5)
}
determine_score = function(titles,texts){
  keywords = keyword_count(titles)
  sentiments = calculate_sentiment(titles)
  sig_words = get_sig_words(titles)
  article_lengths = nchar(as.character(texts))
  word_count = count_sig_words(sig_words, texts)
  return(word_count / log2(article_lengths) + abs(sentiments) * keywords * 5)
}
