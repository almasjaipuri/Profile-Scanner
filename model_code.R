#Importing the libraries
{
  library(dplyr)
  library(textreadr)
  library(tm)
  library(googleLanguageR)
  library(knitr)
  library(ggplot2)
  library(quanteda)
  library(stringr)
  library(udpipe)
  library(lattice)
  library(scales)
  library(readtext)
  library(qdap)
  library(tm)
  library(wordcloud)
  library(plotrix)
  library(dendextend)
  library(ggplot2)
  library(ggthemes)
  library(RWeka)
  library(quanteda)
  library(tidytext)
  library(tidyr)
  library(igraph)
  library(ggraph)
  library(widyr)
  library(sqldf)
  library(stringr)
  library(text2vec)
  library(lsa)
  library(tcR)
  library(vegan)
  library(proxy)
  library(rlist)
  
}


#Information Retrieval and plotting part
{
  
  
  profile_df_details <- data.frame()
  
  
  dir.create(paste0(dir_loc,"Input Files"))
  FILES <- list.files(dir_loc, pattern = ".doc")
 
 #converting doc and pdf files to text file format 
  for (i in 1:length(FILES)) {
    if(substr(FILES[i],nchar(FILES[[i]])-3,nchar(FILES[[i]]))=="docx"){
      FILE=read_docx(file=paste0(dir_loc,FILES[i]))
      write.table(FILE,file=paste0(dir_loc,"Input Files/",sub(".docx","",FILES[i]),".txt"))
    }
    else if(substr(FILES[i],nchar(FILES[[i]])-3,nchar(FILES[[i]]))==".doc"){
      FILE=read_doc(file=paste0(dir_loc,FILES[i]))
      write.table(FILE,file=paste0(dir_loc,"Input Files/",sub(".doc","",FILES[i]),".txt"))
    }
    
    
  }
  FILES_1 <- list.files(dir_loc, pattern = ".pdf")
  for (i in 1:length(FILES_1)) {
   if(length(FILES_1)>0){
     
     FILE=read_pdf(file=paste0(dir_loc,FILES_1[i]))
     write.table(FILE,file=paste0(dir_loc,"Input Files/",sub(".pdf","",FILES[i]),".txt"))
     
   }
    
  }
  #separating the skills file from the profiles
  str = 0
  input_files <- list.files(paste0(dir_loc,"Input Files/"))
  for(x in 1:length(input_files)){
    if(regexpr(pattern = "Skills",text = input_files[[x]])>0)
      str = x
  }
  skill <- input_files[str]
  input_files <- input_files[-str]
  skills <- readtext(paste0(dir_loc,"Input Files/",skill))
  #profile <- readtext(paste0(dir_loc,"Input Files/","AmolIT.txt"))
  skill_nlp_result <- gl_nlp(skills$text)
  t <- unique(skill_nlp_result$tokens[[1]]$content[skill_nlp_result$tokens[[1]]$tag=='NOUN'&skill_nlp_result$tokens[[1]]$label=='NN'])
  a <-unique(skill_nlp_result$tokens[[1]]$content[skill_nlp_result$tokens[[1]]$tag=='NOUN'&skill_nlp_result$tokens[[1]]$label=='ROOT'])
  b <- list()
  for(i in 1:length(a)){
    if(nchar(a[[i]])>3)
      b <- append(b,a[[i]])
  }
  c <- str_replace(combine_words(list(t[[1]],b[[1]]))," and","")
  
  location <- list()
  #loc_candidate <- data.frame()
 
  for (i in 1:length(input_files)){
    
    profile <- readtext(paste0(dir_loc,"Input Files/",input_files[[i]]))
    #profile <- readtext(paste0(dir_loc,"Input Files/","AmolIT.txt"))
    nlp_result <- gl_nlp(profile$text)
    location <- list.append(location,unique(nlp_result$entities[[1]]$locality[!is.na(nlp_result$entities[[1]]$locality)]))
    #loc_candidate[i,1] <- unique(nlp_result$entities[[1]]$locality[!is.na(nlp_result$entities[[1]]$locality)])
    proper <- nlp_result$tokens[[1]]$content[nlp_result$tokens[[1]]$proper=='PROPER']
    
    if(proper[[1]]=="Curriculum")
      name <- proper[c(3:4)]
    else if(proper[1]=="Resume")
      name <- proper[c(2:3)]
    else
      name <- proper[c(1:2)]
    name <- str_replace(combine_words(name)," and","")
    name <- str_replace_all(name, "[^[:alnum:]]", " ")
    profile_df <- tibble(text = profile$text)
    details <- extractInfo(profile_df)
    temp <- data.frame(0)
    temp$name <- name
    
    temp$contact <-details[[1]]
    temp$email <- details[[2]]
    temp$exp <- ifelse(!is.na(details[[3]]),details[[3]],NA)
    temp <- temp[,-1]
    colnames(temp) <- c("Candidate Name","Contact","E-Mail","Experience")
    profile_df_details <-rbind(profile_df_details,temp)
    
    
  }
  
 
  colnames(profile_df_details) <- c("Candidate Name","Contact","E-Mail","Experience")
 
plot_freq <- function(){
  plot1 <- wordcloud(unique(nlp_result$entities[[1]]$name[nlp_result$entities[[1]]$type=='ORGANIZATION']))
  return(plot1)
}
  
 
  
}

#Converting all the documents into corpus

pf1.corpus <- Corpus(DirSource(paste0(dir_loc,"/Input Files/")))

#Formatting the corpus of documents
pf1.corpus <- tm_map(pf1.corpus, removePunctuation)
pf1.corpus <- tm_map(pf1.corpus, stripWhitespace)
pf1.corpus <- tm_map(pf1.corpus, removeWords, stopwords("english"))

#stemming the documents
pf1.corpus <- tm_map(pf1.corpus, stemDocument)
my.tdm <- TermDocumentMatrix(pf1.corpus)
inspect(my.tdm)
my.dtm <- DocumentTermMatrix(pf1.corpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(my.dtm)



#creating termdocumentmatrix and calculating cosine and jaccard similarity index
tdm <- as.matrix(my.tdm)
cosine_dist_mat <- as.matrix(dist(t(tdm), method = "cosine"))
jaccard_dist_mat <- as.matrix(dist(t(tdm), method = "jaccard"))


jaccard_index <- as.data.frame(jaccard_dist_mat[4,-4])
jaccard_index[,2] <- profile_df_details$`Candidate Name`
colnames(jaccard_index) <- c("Score","Candidate")
jaccard_index <- jaccard_index[order(-jaccard_index$Score),]
jaccard_index$Candidate <- factor(jaccard_index$Candidate, levels = jaccard_index$Candidate)
jaccard_index$Rank <- rank(-jaccard_index$Score) 
theme_set(theme_bw())

#Function to plot the jaccard score as per ranking for the candidates
rank_plotting <- function(details){
  new_jaccard <- data.frame()
  a <- list()
  for(i in 1:length(jaccard_index[,1])){
    for(j in 1:length(details[,1])){
      if(as.character(jaccard_index[i,]$Candidate)==details[j,]$`Candidate Name`){
        a <- list.append(a,i)
      }
    }
    
  }
  new_jaccard <- jaccard_index[unlist(a),]
  new_jaccard$Rank <- rank(-new_jaccard$Score)
  plot3 <- ggplot(new_jaccard,aes(x = new_jaccard$Candidate,y = paste0(round(new_jaccard$Score * 100,2),"%")))+geom_bar(stat = 'identity',width = 0.5, fill = "tomato3")+
    geom_label(aes(label = paste0("Rank:",new_jaccard$Rank))) +xlab("Candidates") +ylab("Similarity Score")
  return(plot3)
}

rank(jaccard_index$Score)

