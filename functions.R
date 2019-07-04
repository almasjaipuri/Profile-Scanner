#Function to extract the details of a candidate using regular expressions
extractInfo <- function(text){
  details <- list()
  
  #name <- regmatches(text$text,regexpr(pattern = ".[0-9]+{10,13}",text = text$text))
  contact <- regmatches(text$text,regexpr(pattern = ".[0-9]+{10,13}",text = text$text))
  contact <- ifelse(length(contact)>0,contact,NA)
  email <- regmatches(text$text,regexpr(pattern = "[a-zA-Z0-9]+@gmail.com",text = text$text))
  email <- ifelse(length(email)>0,email,NA)
  experience <- regmatches(text$text,regexpr(pattern = "[0-9]+. years",text = text$text))
  experience <- ifelse(length(experience)>0,experience,NA)
  details <- cbind(contact,email,experience)
  return(details)
}

#Text cleaning function called from an another text formatting block
clean_corpus <- function(text){
  text <- replace_non_ascii(text,replacement = "")
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "@")
  corpus <- tm_map(corpus, toSpace, "\\|")
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus,removeNumbers)
  
 
  
  return(corpus)
}

# function to clean text
Clean_Text_Block <- function(text){
  # Get rid of blank lines
  indexes <- which(text == "")
  if (length(indexes) > 0) {
    text <- text[-indexes]
  }
  # See if we are left with any valid text:
  if (length(text) == 0) {
    cat("There was no text in this document! \n")
    to_return <- list(num_tokens = 0,
                      unique_tokens = 0,
                      text = "")
  } else {
    # If there is valid text, process it.
    # Loop through the lines in the text and combine them:
    clean_text <- NULL
    for (i in 1:length(text)) {
      # add them to a vector
      clean_text <- c(clean_text, clean_corpus(text[i]))
    }
    
    # Calculate the number of tokens and unique tokens and return them in a
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok,
                      unique_tokens = num_uniq,
                      text = clean_text)
  }
  
  return(to_return)
}

#Function creating document matrix
documentMatrix <- function(text1,text2){
  #' Create a list containing a vector of tokens in each document for each
  #' document. These can be extracted from the cleaned text objects as follows.
  doc_list <- list(text1$text, text2$text)
  
  #' Create a vector of document lengths (in tokens)
  doc_lengths <- c(text1$num_tokens,text2$num_tokens)
  
  #' Generate a vector containing the unique tokens across all documents.
  unique_words <- unique(c(text1$text,text2$text))
  
  #' The number of unique tokens across all documents
  n_unique_words <- length(unique_words)
  
  #' The number of documents we are dealing with.
  ndoc <- 2
  
  #' Now feed all of this information to the function as follows:
  Doc_Term_Matrix <- Generate_Document_Word_Matrix(
    number_of_docs = as.vector(unlist(ndoc)),
    number_of_unique_words = n_unique_words,
    unique_words = as.vector(unlist(unique_words)),
    Document_Words = doc_list,
    Document_Lengths = doc_lengths
  )
  
  #' Make sure to add column names to  Doc_Term_Matrix, then take a look:
  colnames(Doc_Term_Matrix) <- unique_words
  
  
}

#Function to create frequency plots and store them in a pdf file
frequency <- function(x, pdf_plot){
  
  pdf(pdf_plot)
  stats <- txt_freq(x$upos)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  plot1 <- barchart(key ~ freq, data = stats, col = "yellow", 
           main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
           xlab = "Freq")
  
  ## NOUNS
  stats <- subset(x, upos %in% c("NOUN")) 
  stats <- txt_freq(stats$token)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  plot2 <- barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
           main = "Most occurring nouns", xlab = "Freq")
  
  ## ADJECTIVES
  stats <- subset(x, upos %in% c("ADJ")) 
  stats <- txt_freq(stats$token)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  plot3 <- barchart(key ~ freq, data = head(stats, 20), col = "purple", 
           main = "Most occurring adjectives", xlab = "Freq")
  
  ## VERBS
  stats <- subset(x, upos %in% c("VERB")) 
  stats <- txt_freq(stats$token)
  stats$key <- factor(stats$key, levels = rev(stats$key))
  plot4 <- barchart(key ~ freq, data = head(stats, 20), col = "gold", 
           main = "Most occurring Verbs", xlab = "Freq")
  
  ## Using RAKE
  stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                         relevant = x$upos %in% c("NOUN", "ADJ"))
  stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
  plot5 <- barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
           main = "Keywords identified by RAKE", 
           xlab = "Rake")
  
  ## Using a sequence of POS tags (noun phrases / verb phrases)
  x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
  stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token),
                            pattern = "(A|N)*N(P+D*(A|N)*N)*",
                            is_regex = TRUE, detailed = FALSE)
  stats <- subset(stats, ngram > 1 & freq > 3)
  stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
  plot6 <- barchart(key ~ freq, data = head(stats, 20), col = "magenta",
           main = "Keywords - simple noun phrases", xlab = "Frequency")
  
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  
  dev.off()
}

#Function to form a wordcloud
word_cloud <- function(text){
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "@")
  corpus <- tm_map(corpus, toSpace, "\\|")
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  dataset = as.data.frame(as.matrix(dtm))
  library(wordcloud)
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  dataset = as.matrix(dtm)
  v = sort(colSums(dataset),decreasing=TRUE)
  myNames = names(v)
  d = data.frame(word=myNames,freq=v)
  wordcloud(d$word, colors=c(3,4),random.color=FALSE, d$freq, min.freq=80)

}

