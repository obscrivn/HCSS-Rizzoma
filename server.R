#options(shiny.reactlog=TRUE)
#command+F3
library(shiny)
#source("myImagePlot.R")
#source("multiplot.R")
#source("MyPlot.R")
source("extractContent.R")
source("extractMetadata.R")
source("abstract.R")
source("words.R")
source("preprocess.R")
#source("kwics.R")
source("parseJSON.R")
source("parseMODS.R")
source("zotero.R")
source("zoteroTxt.R")
library(ca)
library(conf.design)
library(curl)
library(dplyr)
library(fields)
library(ggthemes)
library(graphics)
library(jsonlite)
#library(RcppArmadillo)
library(ggplot2)
library(ggplot2movies)
library(lda)
library(plyr)
library(qdap)
library(qdapRegex)
library(quanteda)
library(reshape2)
library(RTextTools)
library(RColorBrewer)
library(RCurl)
library(slam)
library(SnowballC)
library(stm)
library(stringi)
#library(devtools)
#devtools::install_github("hrbrmstr/streamgraph")
#showReactlog
library(streamgraph)
library(stringr)
library(tidyr)
library(tm)
library(topicmodels)
library(wordcloud)
#library(rjson) Conflicts with jsonlite
library(XML)

library(viridis)

shinyServer(function(input, output) {
  
  output$print_name_article <- renderPrint({
    if (is.null(input$file.article)) { return() }
    paste(input$file.article$name, sep="\n")
  }) 
  output$print_name_article_txt <- renderPrint({
    if (is.null(input$file.article.txt)) { return() }
    paste(input$file.article.txt$name, sep="\n")
  }) 
output$print_length_pdf <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
output$print_length_txt <- renderUI({
  if  (is.null(input$file.article.txt)) { return() }
  HTML(paste("Corpus Size Total: ", ListTerms()$len, sep=" ", collapse="<br/>"))
})
output$place_for_structured_data_browser <- renderUI ({
  switch (input$structured_data_file_source,
          "XML" = fileInput('structured_data_file_xml', 'Choose XML File', multiple=FALSE, accept=c('application/xml','text/xml','.xml')),
          "JSON"= fileInput('structured_data_file_json', 'Choose JSON File', multiple=FALSE, accept=c('application/json',',JSON')),
          "GoogleAPI" = list( textInput('Google_keywords', "Enter your search terms for Google Books, separated by spaces", placeholder = "e.g., data mining"), actionButton("Google_submit", "Submit") )
  )
})

##### Reading Data #######
structured_data <- reactive({ # loading data
  if ((!is.null(input$file.article)) || (!is.null(input$file.article.txt)) || (!is.null(input$file.tag))||(!is.null(input$file.rdf))) { return() }
  parsed_data = NULL
  if( !is.null(input$structured_data_file_json) ) {
    my_data <- fromJSON(input$structured_data_file_json$datapath)
    parsed_json <- parseJSON(my_data)
    parsed_data <- data.frame(parsed_json)
  }
  else if( !is.null(input$structured_data_file_xml) ) {
    #my_data <- xmlToDataFrame(input$structured_data_file_xml$datapath)
    my_data <- xmlTreeParse(input$structured_data_file_xml$datapath, useInternalNodes=TRUE)
    parsed_MODS <- parseMODS(my_data)
    parsed_data <- data.frame(parsed_MODS)
  }
  else if (!is.null( Google_keywords_submitted() ) ) {
    query <- gsub("\\s+", "+", Google_keywords_submitted())
    #    u <- URLencode( paste("https://www.googleapis.com/books/v1/volumes?q=", query, "&maxResults=40", sep = "") )
    #    my_data <- fromJSON( getURL(u) )
    
    
    #uncomment to load the entire result from Google API - takes a really long time
    con <- curl( URLencode( paste("https://www.googleapis.com/books/v1/volumes?q=", query, "&maxResults=40", sep = "") ) )
    text <- readLines(con)
    close(con)
    my_data <- fromJSON(text)
    
    parsed_json <- parseJSON(my_data)
    parsed_data <- data.frame(parsed_json)
    
    #Trying to overcome the 40 results limit and fix the encoding problem
    #my_data <- fromJSON(stream_in(curl(u)))
    #my_data <- do.call(rbind, lapply(paste(readLines(curl(u), warn=FALSE), collapse=""), jsonlite::fromJSON))
    
    #initial_JSON <- readLines(curl(u))
    #collapsedJSON <- paste(initial_JSON, collapse="")
    
    
  }
  return(parsed_data)
})

Google_keywords_submitted <- eventReactive(input$Google_submit, {
  input$Google_keywords
})


####### Display Structured Data
output$place_for_structured_data <- renderDataTable({
  my_data = structured_data()
  if ( is.null(my_data) )  { return() }
  my_data
}) 

ExtractRawContentPDF <- reactive ({
  if (is.null(input$file.article)) { return() }
  extractContentPdf(input$file.article)
})

output$choose_term <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf)))   { return() } 
  # word <- ListTerms()$d[[2]]#[1:x]
  word <- RemoveWordsStepOne()$d[[2]]
  selectizeInput("choose_term", label = "Type term", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
})   
  ###### print_name_rdf ######

output$print_name_rdf <- renderPrint({
  if (is.null(input$file.rdf)) { return()}
  # paste(input$file.rdf$name, sep="\n")
  paste(input$file.rdf$name, sep="\n")
}) 
######### zoteroData function #######
zoteroData <- reactive ({
  if (is.null(input$file.rdf))  { return() }
 # if ((is.null(input$directory)) || (is.na(input$directory)))  { return() }
  uris.name <-  input$file.rdf$datapath

  # dir<-input$directory
  #zot.pdf <-  zotero(uris.name)
  zot.pdf <-  zotero_rdf(uris.name) #xml parsing - list of articles and sublist of 5: title, abstract,date,surname, firstname,link,type,path
  # dir.list <- list()
  dir.list <- vector()
  title.list <- vector()
  k=1
  for (i in 1:length(zot.pdf)) {
   # if (zot.pdf[i]!="") {
    if (!is.na(zot.pdf[[i]][1])) {
      article <- zot.pdf[[i]]
     # text <- zot.pdf[i]
      #path <- paste0(dir,text)
      #path <- paste0("/Users/olgascrivner/Documents/ITMS/Summer2017/TestingFiles/",text)
      path <- article[8] # file path
      dir.list[k] <- path
      title <- article[1]
      title.list[k] <- title
      k <- k+1
    }
  }
 # extractPdfZotero
#  texts <- extractPdfZotero(dir.list)

 # texts <- extractPdfZotero(dir.list,zot.pdf)
  texts <- extractPdfZotero(zot.pdf) # list of extracted articles with sublist of titles, abstracts, authors, datetimes, text.extracts
 # zot.data <-  extractZoteroTxt(uris.name)
 # info <- list(zot.pdf=zot.pdf,dir.list=dir.list)
  info <- list(zot.pdf=zot.pdf,texts=texts, title.list=title.list)
  return(info)
 # return(zot.data)
})

output$zotero_term <-renderUI ({
  list(textInput('zotero_term', "Enter query 1 AND/OR query 2", placeholder = "european AND human"),
  actionButton("zotero_submit", "Submit") )
})

output$zotero_term2 <-renderUI ({
  list(textInput('zotero_term2', "Enter your 2nd search term", placeholder = "human"),
       actionButton("zotero_submit2", "Submit") )
})

output$zotero_condition <-renderUI ({
  list(textInput('zotero_condition', "Condition: Enter OR or AND", placeholder = "AND"),
       actionButton("zotero_submit3", "Submit") )
})

zotero_keywords_submitted <- eventReactive(input$zotero_submit, {
  input$zotero_term
})
zotero_keywords2_submitted <- eventReactive(input$zotero_submit2, {
  input$zotero_term2
})
zotero_keywords3_submitted <- eventReactive(input$zotero_submit3, {
  input$zotero_condition
})




output$choose_kwic_num <- renderUI({
  selectizeInput("choose_kwic_num", label = "Select or Type number of words between two terms if your condition is AND", 
                 choices = c(2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$zotero_slider <- renderUI({
  sliderInput("zotero_slider", "Window Length (left and right context):",
              min = 0, max = 100, value = 5)
})
#######extractZoteroTerm function ####
extractZoteroTerm <- reactive ({
  if (is.null(input$file.rdf))  { return() }
 # if (is.null(zotero_keywords1_submitted()) &is.null(zotero_keywords2_submitted())) { return() }
  #z <- zoteroData()$texts
  zot_data <- zoteroData()$texts#[[5]]#[[4]]
 # links_pdf <- zoteroData()$dir.list#zot.pdf
  dir.list <- zoteroData()$dir.list
 # z <- zoteroData()$texts[[5]]#zoteroData()$zot.pdf#dir.list
 # author <-zoteroData()$texts[[2]]
 # title <- zoteroData()$texts[[1]]
 # date <- zoteroData()$texts[[3]]
 # query1 <- gsub("\\s+", "",zotero_keywords1_submitted())
 # query2 <- gsub("\\s+", "",zotero_keywords2_submitted())
 # condition <- "and"
  if (!is.null(zotero_keywords_submitted())) {
    query <- gsub("\\s+", " ", tolower(zotero_keywords_submitted()))
    query1  <- unlist(strsplit(query," "))[1]
    query2 <- unlist(strsplit(query," "))[3]
    condition <- unlist(strsplit(query," "))[2]
  }
 # if (!is.null( zotero_keywords2_submitted()) ) {
   # query1 <- gsub("\\s+", " ", zotero_keywords1_submitted())

  #  query2 <- gsub("\\s+", "", tolower(zotero_keywords2_submitted()))
 # }
 # if (!is.null( zotero_keywords3_submitted()) ) {
   # condition <- gsub("\\s+", "", tolower(zotero_keywords3_submitted()))
#  }
   # query  <- unlist(strsplit(query1," "))[1]
    #query2 <- unlist(strsplit(query1," "))[2]
 # }
  len <- as.integer(input$zotero_slider)
  between <- as.integer(input$choose_kwic_num)
 # path <- input$file.rdf$datapath
 # extract <-  extractZotero(z,query,query2,len,words,author,title, date)#p1,p2)
  #extract <- zoteroData()$texts  #
 # extract <- extractZotero(links_pdf,query,query2,len,path)#, words)#p1,p2)
 # extract <- extractZoteroTxt(zot_data,query,query2,len, dir.list)
  extract <- extractZoteroTxt(zot_data,query1,query2,condition,len,between)
  withProgress(message = 'Preprocessing Zotero',
               detail = 'Almost done...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  return(extract) # list of 5 text.extract, titles, datetimes,authors, abstracts
})

########print_content_rdf ######
output$print_content_rdf <- renderUI({
  if (is.null(input$file.rdf))  { return() }
  txt.lines <- extractZoteroTerm()[[1]]#$titles#zoteroData()$dir.list #directory()
 # txt.lines <- zoteroData()$dir.list#extractZoteroTerm()$titles#zoteroData()$dir.list #directory()
 # txt.lines <- zoteroData()$title.list#texts[[1]]#[[2]]#extractZoteroTerm()$text.extract
  HTML(paste("<br/>", txt.lines, sep="<br/>"))
}) 
##########zotero_content function ########
zotero_content <- reactive({
  if (is.null(input$file.rdf))  { return() }
  txt.lines <- extractZoteroTerm()$text.extract#extractZoteroTerm()[[1]]
 # txt.lines <-zoteroData()$texts
   # extractZoteroTerm()$text.extract# zoteroData()$texts[[5]]#extractZoteroTerm()
  return(txt.lines)
})

output$print_zotero <- renderUI({
  if (is.null(input$file.rdf))  { return() }
  txt.lines <- zotero_content()
  HTML(paste("<br/>", "Document: ",txt.lines, sep="<br/>"))
})

output$choose_length <- renderUI({
  selectizeInput("len",
                 "Context Length:",
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
})

####### ListTerms function #####
ListTerms <- reactive({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) &&(is.null(input$file.rdf)) && (is.null(input$file.tag)))   { return() } 
  
  if(!is.null(input$file.article)) {
    corpus.lda <- ExtractRawContentPDF()#Selection()#$text.extract
  }
  else if(!is.null(input$file.article.txt))  {
    corpus.lda <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
  }
  else if (!is.null(input$file.rdf))  {
    corpus.lda <- zotero_content()#extractZoteroTerm()#$text.extract
  }
  else if (!is.null(input$file.tag))  {
    corpus.lda <- selectPos()
  }
  else if (!is.null(structured_data()))  {
    corpus.lda <- structured_data()$corpus
  }
  listTerms(corpus.lda)
})  

output$term_print <- renderUI ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.rdf)) && (is.null(input$file.tag))) { return() }
  len <- input$len
  term <- input$choose_term
  HTML(paste("Your Term: ", term, "Your Context Length: ", len,  sep=" ", collapse="\n"))
})
######Abstract function ###########
Abstract <- reactive ({
  if (is.null(input$file.article))  { return() } 
  if (input$article_content=="Abstract") {
    extractAbstract(x=ExtractRawContentPDF(), y=input$file.article)
  }
})



 ########  metadataPdF function######
metadataPdf <- reactive ({
  if (is.null(input$file.article)) { return() }
  x=input$file.article
  extractMetadata(x)
})

## Reading Metadata File from csv

output$place_for_file_browser <- renderUI({
  #  if( input$metadata_source == "None") {return()}
  if( input$metadata_source == "PDF") {
    metadata_file_name = input$file.article.name#NULL;
  }
  switch (input$metadata_source,
          "CSV" = column(6,
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         fileInput('csv_file_name', 'Choose CSV File', multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
          ),
          "JSON"= fileInput('json_file_name', 'Choose JSON File', multiple=FALSE, accept=c('application/json',',JSON')),
          "XML" = fileInput('xml_file_name', 'Choose XML File', multiple=FALSE, accept=c('application/xml','text/xml','.xml'))
  )
})
## Reading Metadata File from csv
######### fileData metafunction #########
fileData <- reactive({ # loading data
  my_data = NULL
  if( !is.null(input$csv_file_name) ) {
    my_data <- read.csv(input$csv_file_name$datapath, header=input$header, sep=input$sep, quote=input$quote)
  }
  else if( !is.null(input$json_file_name) ) {
    my_data <- fromJSON(input$json_file_name$datapath, flatten = TRUE)
  }
  else if( !is.null(input$xml_file_name) ) {
    my_data <- xmlToDataFrame(input$xml_file_name$datapath)
  }
  else if( !is.null(input$file.article)) {
    a <- metadataPdf()$authors
    t <- metadataPdf()$titles
    dt <- metadataPdf()$datetimes
    my_data <- data.frame(date=dt, title=t, author = a)
    # my_data <- data.frame(metadataPdf()$metapdf)
  }
  else if (!is.null(input$file.rdf)) {
   # a <- 4#$authors
   # t <- 2#$titles
   # dt <- 3
   # a <- extractZoteroTerm()[[4]]#$authors
   # t <- extractZoteroTerm()[[2]]#$titles
  #  dt <- extractZoteroTerm()[[3]]
    a <- extractZoteroTerm()$authors#zoteroData()$texts[[2]]
    t <- extractZoteroTerm()$titles#zoteroData()$texts[[1]]
    dt <- gsub("[a-zA-Z]* ","", extractZoteroTerm()$datetimes)#zoteroData()$texts[[3]]
    my_data <- data.frame(date=dt, title=t, author = a)
  }
  #else if( !is.null(input$structured_data_file_json) ) {
  #  parsed_json <- parseJSON(structured_data())
  #  my_data <- data.frame(date=parsed_json$dates, title=parsed_json$titles, author = parsed_json$authors)
  #}
  return(my_data)
})
#########zotero_metadata_table #######
output$zotero_metadata_table <- renderDataTable({
 # if (is.null(input$file.rdf)){ return() }
  if (is.null(input$file.rdf)){ return() }
   a <- extractZoteroTerm()[[4]]#$authors
   t <- extractZoteroTerm()[[2]]#$titles
   dt <- extractZoteroTerm()[[3]]#$datetimes
  # my_data <- data.frame(date=dt, title=t, author = a)
 # a <- zoteroData()$authors
 # t <- zoteroData()$titles
 # dt <- zoteroData()$datetimes
  my_data <- data.frame(date=dt, title=t, author = a)
  return(my_data)
})
### Display Metadata from CSV
output$place_for_metadata_table <- renderDataTable({
  data_from_metadata_file = fileData()
  if ( is.null(data_from_metadata_file) )  { return() }
  if (input$metadata_source=="None")  { return() }
  data_from_metadata_file
}) 

output$print_content_txt <- renderUI({
  if (is.null(input$file.article.txt))  { return() }
  txt.lines <-  ExtractRawContentTXT()
  withProgress(message = 'Loading txt file',
               detail = '....', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  HTML(paste("<br/>", "Document: ",txt.lines, sep="<br/>"))
  
  
}) 

output$print_content_pdf <- renderUI({
  if (is.null(input$file.article))  { return() }
  pdf.lines <-  ExtractRawContentPDF()
  withProgress(message = 'Loading pdf file',
               detail = '....', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 

output$print_abstract <- renderUI({
  if (is.null(input$file.article))   { return() }
  if (input$article_content=="Full Text") {
    pdf.abstract="Abstract is not selected"
  }
  else {
    pdf.abstract <- Abstract()
  }
  HTML(paste("<br/>", "Document: ",pdf.abstract, sep="<br/>"))
}) 

output$print_preprocessed <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&& (is.null(input$file.rdf))) { return() }
  if (input$preprocessing=="No Changes") {
    pdf.lines <- "No pre-processing steps are applied"
  }
  else if (input$preprocessing=="Apply Steps") {
    withProgress(message = 'Preprocessing',
                 detail = 'Almost done...', value = 0, {
                   for (i in 1:60) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    pdf.lines <- PreprocessingSteps()[6]#$lda.format#window.one()$lda.format
    pdf.lines <- unlist(pdf.lines)
  }
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 
  

### Pre-Processing
######### PreprocessingSteps function ######

PreprocessingSteps <- reactive ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&& (is.null(input$file.rdf))) { return() }
 if (is.null(input$file.rdf) && (is.null(structured_data()))) {
   if(!is.null(input$file.article)) {
    if(input$article_content=="Full Text"){
      x <- ExtractRawContentPDF()
    }
    else if(input$article_content=="Abstract"){
      x <- Abstract()
    } 
    y <- input$file.article
  }
  else if(!is.null(input$file.article.txt)) {
    x <- ExtractRawContentTXT()
    y <- input$file.article.txt
  }
  # else if(!is.null(structured_data())) {
  #   # parsed_json = parseJSON(structured_data())
  #   x <- structured_data()$corpus
  #   y <- list(name = structured_data()$titles, size = 0, type = "", datapath = "")
  # }
  else if (!is.null(input$file.tag)) {
  x <- selectPos()
  y <- input$file.tag
  }
    remove_urls <-input$remove_urls
    remove_references<-input$remove_references
    remove_html<-input$remove_html
    lower_case<-input$lower_case
    remove_numbers<-input$remove_numbers
    exceptions<-input$exceptions
    remove_punctuation <- input$remove_punctuation
    mylist<-tokenize(x,y,remove_urls,remove_references,remove_punctuation,
                     remove_html,lower_case,remove_numbers,exceptions)
 }
  else  {
    if(!is.null(structured_data())) {
      x <- structured_data()$corpus
      y <- structured_data()$titles
    }
    else if (!is.null(input$file.rdf)) {
      x <- zotero_content() #extractZoteroTerm()$text.extract#zotero_content()#extractZoteroTerm()$text.extract
      y <- extractZoteroTerm()$titles
       # zoteroData()$texts[[1]]#extractZoteroTerm()$titles#extractZoteroTerm()$titles
    }
    remove_urls <-input$remove_urls
    remove_references<-input$remove_references
    remove_html<-input$remove_html
    lower_case<-input$lower_case
    remove_numbers<-input$remove_numbers
    exceptions<-input$exceptions
    remove_punctuation <- input$remove_punctuation
    mylist<-tokenizeRdf(x,y,remove_urls,remove_references,remove_punctuation,
                     remove_html,lower_case,remove_numbers,exceptions)
  }
  return(mylist)
})


##### STOP WORDS 
#############stopWordsTxt function ######
stopWordsTxt <- reactive ({
  stop_words<-vector()
  if (input$stops=="Default") {
    stop_words <- stopwords("SMART")
  }
  if (input$stops=="Upload") {
    uris.name <- input$stopwords.txt$datapath
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)
    x=enc2utf8(text.scan)
    text.punct <- str_c(x)#text.punct)
    text.punct<- str_trim(text.punct)
    text.punct <- gsub("\\s\\s+", " ", text.punct)   
    stops<-strsplit(text.punct, " ")
    stop_words<-unlist(stops)
  }
  return(stop_words)
})

output$print_stopwords <- renderPrint({
  if (input$stops=="None") {"Stopwords are not selected"}
  else{ 
    stopWordsTxt()}
})
#########RemoveWordsStepOne #########
#Allows for interactive instant word removals from user
RemoveWordsStepOne <-reactive({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag)) &&(is.null(input$file.rdf))) { return() }
  #cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  mycorpus <- PreprocessingSteps()[6]
  if  (input$stops=="None") {    
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
    corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    lda.corpus <- corpus
    # corpus <- mycorpus
    corpus <- unlist(corpus)
    corpus.paste <-paste(mycorpus, sep=" ")
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    terms.sorted <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms.sorted)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    #d <- d[order(d$frequency, decreasing = T), ]
    d <- d[order(d$word), ]
    # words.list <- as.list(d$word)
    
  }
  else if  ((input$stops=="Default")||(input$stops=="Upload")) {
    remove_word <- stopWordsTxt()
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
    corpus.tm <- tm_map(corpus.tm,removeWords,stopWordsTxt())
    corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    lda.corpus <- corpus
    corpus <-unlist(corpus)
    # corpus.paste <-paste(mycorpus, sep=" ")
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    remove_word <- stopWordsTxt()
    del <- names(terms) %in% remove_word #| terms < cutoff.lower
    terms <- terms[!del]
    terms.sorted <- sort(terms, decreasing = TRUE)
    terms.matrix<-as.matrix(terms.sorted)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
   # d <- d[order(d$frequency, decreasing = T), ]
    d <- d[order(d$word), ]
    # words.list <- as.list(d$word)
  }
  info <- list(corpus=corpus,d=d,lda.corpus=lda.corpus)
  return(info)
})
#######RemoveWordsStepTwo########
RemoveWordsStepTwo <-reactive({
 # if ((is.null(input$file.article)) & (is.null(input$file.article.txt)) & (is.null(structured_data()))& (is.null(input$file.tag))&(is.null(input$file.rdf))) { return() }
  if (is.null(input$remove_words)) {
    corpus <-RemoveWordsStepOne()$corpus
    lda.corpus<-RemoveWordsStepOne()$lda.corpus
    d <-RemoveWordsStepOne()$d
    # words.list <-RemoveWordsStepOne()$words.list
  }
  else {
    mycorpus <-RemoveWordsStepOne()$corpus
    doc.vect <- VectorSource(mycorpus)
    corpus.tm <-Corpus(doc.vect)
    # corpus.tm <- removeWords(corpus.tm, c(input$remove_words))
    corpus.tm <- tm_map(corpus.tm,removeWords,c(input$remove_words))
    corpus.tm <- tm_map(corpus.tm, stripWhitespace)
    corpus <- list()
    for (i in 1:length(corpus.tm)) {
      doc <-corpus.tm[[i]]$content
      corpus[[i]] <- doc
    }
    # corpus <-unlist(corpus)
    corpus.paste <-paste(corpus, sep=" ")
    corpus.paste <- str_c(corpus.paste)
    corpus.paste<- str_trim(corpus.paste)
    corpus.list <- strsplit(corpus.paste, "\\s+")
    terms <- table(unlist(corpus.list))
    terms.sorted <- sort(terms, decreasing = TRUE)
    # remove_word <- stopWordsTxt()
    # del <- names(terms) %in% remove_word #| terms < cutoff.lower
    # terms <- terms[!del]
    terms.matrix<-as.matrix(terms)
    d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
    d$word <- row.names(d)
    agg_freq <- aggregate(frequency ~ word, data = d, sum)
    d <- d[order(d$frequency, decreasing = T), ] 
    #words.list <- as.list(d$word)
  }
  info <- list(corpus=corpus,d=d)#,words.list=words.list)
  return(info)
})
######RemoveWordsStepThree #########
  RemoveWordsStepThree <-reactive({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  corpus <- stemming()
  doc.vect <- VectorSource(corpus)
  docs <-Corpus(doc.vect)
  tdm <- TermDocumentMatrix(docs)
  dtm <- DocumentTermMatrix(docs)
  term.matrix <- as.matrix(tdm)
  if(!is.null(input$file.article)) {
    file.names <-input$file.article$name
  }
  if(!is.null(input$file.article.txt))  {
    file.names <-input$file.article.txt$name
  }
  if(!is.null(structured_data()))  {
    file.names <-  structured_data()$titles
  }
  if(!is.null(input$file.tag))  {
    file.names <-  input$file.tag$name
  }
  if(!is.null(input$file.rdf)) {
    file.names <- extractZoteroTerm()$titles#zoteroData()$texts[[1]]#extractZoteroTerm()$titles
  }
  colnames(term.matrix) <- file.names
  corpus.paste <-paste(corpus, sep=" ")
  corpus.paste <- str_c(corpus.paste)
  corpus.paste<- str_trim(corpus.paste)
  corpus.list <- strsplit(corpus.paste, "\\s+")
  terms <- table(unlist(corpus.list))
  terms.sorted <- sort(terms, decreasing = TRUE)
  terms.matrix<-as.matrix(terms)
  d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
  d$word <- row.names(d)
  agg_freq <- aggregate(frequency ~ word, data = d, sum)
  d <- d[order(d$frequency, decreasing = T), ]
  info <- list(d=d,corpus=corpus,tdm=tdm,term.matrix=term.matrix,dtm=dtm)
  return(info)
})

output$print_apply_stops <- renderUI({
  if (input$stopwords=="None") {"No changes are made. You need to select stopwords first"}
  if (input$stopwords=="Apply Stopwords") {
    HTML(paste0(RemoveWordsStepThree()$corpus))
  }
})
######### stemming#######
stemming <- reactive ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if(input$stopwords=="None") {
    corpus <-RemoveWordsStepOne()$corpus
    # d <-RemoveWordsStepOne()$d
  }
  else if(input$stopwords=="Apply Stopwords") {
    corpus <-RemoveWordsStepTwo()$corpus
    #   d <-RemoveWordsStepTwo()$d
  }
  
  if (input$language=="none") {
    corpus <-RemoveWordsStepTwo()$corpus
    # text.punct="Select language"
  }
  else {
    doc.vect <- VectorSource(corpus)
    docs <-Corpus(doc.vect)
    corpus <- list()
    for (i in 1:length(docs)) {
      doc <-docs[[i]]$content
      text.split <- unlist(strsplit(doc, " "))
      text.stem <- paste(wordStem(text.split, language = input$language),collapse = " ")
      text.stem <- str_c(text.stem)
      text.stem<- str_trim(text.stem)
      corpus[[i]] <- text.stem
    }
    corpus <-unlist(corpus)
  }
  return(corpus)
})

output$print_stemmer <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if (is.null(input$remove_manual)) { return() }
  HTML(paste0(stemming()))
})
#########rawfrequency###########
rawFrequency <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if(!is.null(input$file.article)) {
    x <-ExtractRawContentPDF()
  }
  else if(!is.null(input$file.article.txt)) {
    x<- ExtractRawContentTXT()
  }
  else if(!is.null(input$file.tag)) {
    x <-selectPos()
  }
  else if(!is.null(input$file.rdf)) {
    x<-zotero_content()#extractZoteroTerm()$text.extract
  }
  else if(!is.null(is.null(structured_data()))) {
    x <-structured_data()$corpus
  }
  d <-frequencyTable(x)
  return(d)
})

output$freq <- renderDataTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  d<-rawFrequency()$dataf#ListTerms()$d#rawFrequency()$dataf
  return(d)#,options=list(lengthMenu = c(5, 10, 15), pageLength = 5))
})

output$zipf <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  tdm <- ListTerms()$tdm
  dtm <- ListTerms()$dtm
  p<- Zipf_plot(dtm, type="l")
  return(p)
})

#heaps not working
#output$heaps <- renderPlot ({
# if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
#if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
# if (input$show_freq=="Frequency") {
# dtm <-ListTerms()$dtm  #RemoveWords()$tdm
#})


output$choose_text <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if (!is.null(input$file.article)) {
    names <- input$file.article$name
  }
  if (!is.null(input$file.article.txt)) {
    names <- input$file.article.txt$name
  }
  if (!is.null(input$file.tag)) {
    names <-input$file.tag$name
  }
  if (!is.null(structured_data())) {
  names <- structured_data()$titles
  }
  if (!is.null(input$file.rdf)) {
    names <- extractZoteroTerm()$titles
  }
  selectizeInput("show_text", label = "Select which document to analyze", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
  
})



#########ExtractRawContentTXT#######
ExtractRawContentTXT <- reactive ({
  if (is.null(input$file.article.txt)) { return() }
  extractContentTxt(input$file.article.txt)
})

output$choose_top <- renderUI({
  selectizeInput("top", label = "Select a number for top frequent words (ex. 10 top frequent words)", 
                 choices = c(10,20,30,40,50,60,70,80,90,100),
                 options = list(create = TRUE),
                 selected=10,
                 multiple = FALSE) 
}) 

output$choose_remove <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  word <- RemoveWordsStepOne()$d[[2]]
  selectizeInput("remove_words", label = "Select words to be removed", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
  # }
})

output$printWords <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if (is.null(input$remove_manual)) { return() }
  HTML(paste0(RemoveWordsStepOne()$d[[2]]))
  # HTML(paste0(RemoveWordsStepTwo()$words.list))#PreprocessingSteps()$lda.format))# RemoveWordsNew()$corpus.lda))
})

output$word_count <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  x <- input$top
  #  word <- RemoveWordsNew()$d[[2]][1:x]
  #  frequency <- RemoveWordsNew()$d[[1]][1:x]
  word <-  RemoveWordsStepThree()$d[[2]][1:x]
  # word <- RemoveWordsFinal()$d[[2]][1:x]
  frequency <- RemoveWordsStepThree()$d[[1]][1:x]
  plot <-barplot(frequency, names.arg=word, las=2,cex.names=0.8)
  return(plot)
})

output$choose_min_frequency <- renderUI({
  selectizeInput("min",
                 "Minimum Frequency:",
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=1,
                 multiple = FALSE) 
})

output$choose_max_words <- renderUI({
  selectizeInput("max",
                 "Maximum Words per Plot:",
                 choices = c(100,150,200),
                 options = list(create = TRUE),
                 selected=150,
                 multiple = FALSE) 
})
#########print_cloud########
output$print_cloud <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  wordcloud_rep <- repeatable(wordcloud)
  font <- input$font
  
  if (input$pal=="black"){
    pal="black"
  } else if (input$pal=="green"){
    pal <- brewer.pal(9, "BuGn")
    pal <- pal[-(1:2)]
  } else if (input$pal=="multi") {
    
    pal <- brewer.pal(8,"Dark2")
  }
  if (input$multicloud=="Word Cloud") {
    d <- RemoveWordsStepThree()$d
    wordcloud_rep(d$word, d$freq, scale=c(8,0.2),ordered.colors=F,#ordered.colors=T,
                  rot.per=.15,#c(8,0.3),
                  min.freq = input$min,# 
                  #vfont=c("sans serif","plain"),
                  #vfont=c("script","plain"),
                  vfont=c(font,"plain"),
                  random.order=FALSE,
                  max.words=input$max,#100,#input$freq, max.words=input$max,
                  #colors=brewer.pal(6, "Dark2"))
                  colors=pal )#"black")
  }
  else if (input$multicloud=="Commonality Cloud") {
    d <- RemoveWordsStepThree()$term.matrix
    commonality.cloud(d, max.words=40,random.order=FALSE,ordered.colors=F,#ordered.colors=T,
                      rot.per=.15,#c(8,0.3),
                      #  min.freq = input$min,# 
                      #vfont=c("sans serif","plain"),
                      #vfont=c("script","plain"),
                      vfont=c(font,"plain"),
                      #  random.order=FALSE,
                      #  max.words=input$max,#100,#input$freq, max.words=input$max,
                      #colors=brewer.pal(6, "Dark2"))
                      colors=pal )#"black")
  }
  else if (input$multicloud=="Comparison Cloud") {
    d <- RemoveWordsStepThree()$term.matrix
    comparison.cloud(d,max.words=40,random.order=FALSE)
  }
  # wordcloud(d$word, d$freq)
  #  max.words =100,min.freq=3,scale=c(4,.5), 
  # random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
})

# kwicAnalysis <- reactive({
#   if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#   len <- input$len
#   term <- input$choose_term
#   if(!is.null(input$file.article)) {
#     text <- ExtractRawContentPDF()#Selection()#$text.extract
#     num <-length(input$file.article$name)
#   }
#   if(!is.null(input$file.article.txt))  {
#     text <- ExtractRawContentTXT()#extractContent(input$file.article.txt) 
#     num <-length(input$file.article.txt$name)
#   }
#   if(!is.null(input$file.tag))  {
#     text <- selectPos()#extractContent(input$file.article.txt) 
#     num <-length(input$file.tag$name)
#   }
#   if(!is.null(input$file.rdf))  {
#     text <- zotero_content()#extractZoteroTerm()$text.extract#extractContent(input$file.article.txt) 
#     num <-length(extractZoteroTerm()$titles)
#   }
#   if(!is.null(structured_data()))  {
#     text <- structured_data()$corpus#extractContent(input$file.article.txt) 
#     num <-length(structured_data()$corpus)
#   }
#   kwics(len,term,text,num)
# })
# 
# output$print_kwic <- renderUI({
#   if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) &&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#   if (is.null(input$choose_term)) { return() }
#   lines <- kwicAnalysis()
#   
#   HTML(paste("<br/>", lines, sep="<br/>"))
# }) 

##### LDA Analysis
output$choose_topic_num <- renderUI({
  selectizeInput("num", label = "Select or Type Number of Topics", 
                 choices = c(2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$choose_word_num <- renderUI({
  selectizeInput("word", label = "Select or Type Number of Words per Topic", 
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$iter <- renderUI({
  names <- c(500,1000)
  selectizeInput("iter", label = "Select or Type a number for iterations", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=500,
                 multiple = FALSE) 
}) 
output$alpha <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("alpha", label = "Select or Type a number for apha", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 
output$eta <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("eta", label = "Select or Type a number for eta", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 

#######chronology########
chronology <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if( input$metadata_source == "None") {return()}
 # if((is.null(input$chronology)) ||(input$chronology=="None")) { return() }
  remove.words.file <- stopWordsTxt()
  corpus.lda <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
  if(!is.null(input$file.article)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus #PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
  }
  if(!is.null(input$file.tag)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
  }
  if(!is.null(input$file.rdf)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
  }
  
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  corpus.lda <- gsub("\\s+"," ",corpus.lda)
  corpus.lda <- str_c(corpus.lda)
  corpus.lda<- str_trim(corpus.lda)
  set.seed(2013)
  my.corpus <- Corpus(VectorSource(corpus.lda))
  # my.corpus <- tm_map(my.corpus,removeWords,stopwords("english"))
  language=input$language
  dtm <- DocumentTermMatrix(my.corpus)
  # if (!is.null(input$metadata_pdf)) {
  dates<-  fileData()$date
  # dates <- metadataPdf()$datetimes
  # }
  #if (!is.null(input$metadata_csv)) {
  #  dates <- newData()[[2]]
  # }
  for (i in 1:length(my.corpus)){
    meta(my.corpus[[i]], tag = "datetimestamp") <- dates[i]
  }
  dtm <- DocumentTermMatrix(my.corpus)
  n.topics <- as.numeric(input$num)
  lda.model <- LDA(dtm, n.topics,method="Gibbs")
  n.words <- as.numeric(input$word)
  term <- terms(lda.model,n.words)  
  df <- data.frame(id=names(topics(lda.model)), 
                   date=unlist(meta(my.corpus, type="local",tag="datetimestamp"),as.character)#, "%Y-%m-%d %H:%M:%S"))))#2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"   
  )

 # dfw <- cbind(df,posterior(lda.model)$topics)
  dft <- cbind(df,posterior(lda.model)$topics)
  # dft2 < cbind(df,term)
  M <-melt(dft,id.vars=c("id","date")) 
  l<-length(my.corpus)
  info <- list(dft=dft, term=term,lda.model=lda.model,M=M,l=l)#,datetimestamp=datetimestamp)
  return(info)
})

output$chronology_top <- renderPrint ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  chronology()$term
})
output$chronology_plot <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#  if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
  #  terms <- chronology()$term
  g <- ggplot(dft,aes(x= dft[,2],y=dft[,4],color=dft[,3]))+xlab("Time Period") + ylab("Posterior") + geom_point(aes(size = dft[,4]))+ geom_density2d(alpha=.2) #+ geom_text(aes(label=terms))
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line() xlab("Time Period") + ylab("Posterior")  +
  return(g)
})
output$chronology_table <- renderTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
  # g <- ggplot(dft,aes(x= dft[,2],y=dft[,3]),color=dft[,1])+geom_point()  + geom_density2d(alpha=.2)
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line()
  return(dft)
})
#######BestK#######
BestK <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  # cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    #  novel.vector <- ExtractContentPDF()$lda.format
    # num.documents <- length(ExtractContentPDF()$lda.format)
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- input$file.article$name  
  }
  if(!is.null(input$file.article.txt)) {
    novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    # novel.vector <- ExtractContentTXT()$lda.format #novel.list
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- input$file.article.txt$name  
  }
  if(!is.null(input$file.tag)) {
    novel.vector <- PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    # novel.vector <- ExtractContentTXT()$lda.format #novel.list
    num.documents <- length(PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- input$file.tag$name  
  }
  novel.vector <- removeWords(novel.vector, remove.words.file)
  novel.vector <- removeWords(novel.vector, c(input$remove_words))
  pdf.corpus <- lexicalize(novel.vector, lower=TRUE)
  if (input$language=="None") {
    pdf.corpus$vocab <- pdf.corpus$vocab
  }
  else {
    pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = input$language)
  }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
  pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  if (!is.null(input$metadata_pdf)) {
    file.names <- metadataPdf()$names
  }
  if (!is.null(input$metadata_csv)) {
    file.names <- metadataPdf()$names
  }
  matrix <- create_matrix(cbind(as.vector(file.names),as.vector(novel.vector)), 
                          language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
  best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(matrix, d)})
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  
  p <- ggplot(best.model.logLik.df, aes(x=topics, y=LL)) +   
    xlab("Number of topics") + ylab("Log likelihood of the model") +   
    geom_line() +   theme_bw()
  
  k <- best.model.logLik.df[which.max(best.model.logLik.df$LL),]
  info <- list(p=p,k=k,best.model.logLik=best.model.logLik,best.model=best.model)
  return(info)
})
output$best_k <- renderTable ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  best.model <- BestK()$best.model
  best.model.logLik<-BestK()$best.model.logLik
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  best.model.logLik.df[which.max(best.model.logLik.df$LL),]
})
output$best_k_plot <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  BestK()$p
})
#########LdaAnalysis########
LdaAnalysis <- reactive({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  set.seed(2013)
  remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
  if(!is.null(input$file.article)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus #PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
    #   num.documents <- length(RemoveWordsStepOne()$lda.corpus)#PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article$name))  
  }
  if(!is.null(input$file.article.txt)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
    # num.documents <- length(RemoveWordsStepOne()$lda.corpus)
    #corpus.lda <-  window.one()$lda.format
    # num.documents <- length(window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.article.txt$name))
  }
  if(!is.null(input$file.tag)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
    # num.documents <- length(RemoveWordsStepOne()$lda.corpus)
    #corpus.lda <-  window.one()$lda.format
    # num.documents <- length(window.one()$lda.format)
    n.docs <- as.numeric(length(input$file.tag$name))
  }
  if(!is.null(input$file.rdf)) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
    n.docs <- as.numeric(length(zoteroData()$texts[[1]]))#extractZoteroTerm()$titles))
  }
  if(!is.null(structured_data())) {
    corpus.lda <-  RemoveWordsStepOne()$corpus
    n.docs <- as.numeric(length(structured_data()$titles))
  }
  corpus.lda <- removeWords(corpus.lda, remove.words.file)
  corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
  corpus.lda <- gsub("\\s+"," ",corpus.lda)
  corpus.lda <- str_c(corpus.lda)
  corpus.lda<- str_trim(corpus.lda)
  # empty.string <- lapply(corpus.lda, function(x) gsub(" +", " ", x))
  # pdf.corpus <- lexicalize(empty.string, lower=TRUE)
  
  # corpus <- Corpus(VectorSource(corpus.lda))
  # corpus <- tm_map(corpus,removeWords,remove.words.file)
  # newtext <-tm_map(corpus,removeWords,input$remove_words)
  
  # pdf.corpus <- lexicalize(newtext, lower=TRUE)
  pdf.corpus <- lexicalize(corpus.lda, lower=TRUE)
  
  if (input$language=="none") {
    pdf.corpus$vocab <- pdf.corpus$vocab
  }
  else {
    language=input$language
    pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language)# = "english")
  }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
  pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  K <- as.numeric(input$num)
  alphaK <-as.numeric(input$alpha)
  etaK<-as.numeric(input$eta)
  num.words <- as.numeric(input$word)
  iterK <-as.numeric(input$iter)
  pdf.lda <-
    lda.collapsed.gibbs.sampler(pdf.corpus$documents,K,pdf.corpus$vocab,iterK, alpha=alphaK, eta=etaK, compute.log.likelihood=TRUE)
  topics <- top.topic.words(pdf.lda$topics, num.words, by.score = T)
  docs <- top.topic.documents(pdf.lda$document_sums,n.docs)# num.documents)
  p_topic <- as.vector(pdf.lda$topic_sums / sum(pdf.lda$topic_sums))
  lda.coordinates <- mat.or.vec(n.docs,K)
  # for (i in 1:n.docs){
  #   for (j in 1:K){
  #     lda.coordinates[i,j] <-
  #       sum(pdf.lda$assignments[[i]]==(j-1))/length(pdf.lda$assignments[[i]])
  #   }
  # }
  info<-list(p_topic=p_topic,topics=topics, docs=docs,lda.coordinates=lda.coordinates)
  return(info)
})
########topics########
output$topics <- renderTable({ #renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$topics
})
#######docs#########
output$docs <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$docs
})

output$docsNames <- renderUI({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) ||  (input$lda=="None")) { return() }
  if (!is.null(input$file.article.txt)) {
    k <- length(input$file.article.txt$name)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", input$file.article.txt$name, sep=" ", collapse="<br/>"))
  }
  else if (!is.null(input$file.article)) {
    k <- length(input$file.article$name)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", input$file.article$name,  metadataPdf()$titles, sep=" ", collapse="<br/>"))
  }
  else if (!is.null(input$tag)) {
    k <- length(input$file.tag$name)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", input$file.tag$name,  sep=" ", collapse="<br/>"))
  }
  else if (!is.null(input$file.rdf)) {
    k <- length(extractZoteroTerm()$titles)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", extractZoteroTerm()$titles,  sep=" ", collapse="<br/>"))
  }
  else if (!is.null(structured_data())) {
    k <- length(structured_data()$titles)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", structured_data()$titles,  sep=" ", collapse="<br/>"))
  }
})
########printCoordinates########
output$printCoordinates <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  
  distance <- LdaAnalysis()$lda.coordinates
  d<-dist(distance)
  # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  # fit # view results
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  p <- plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
            main="Metric  MDS",	type="n")
  text(x, y,  cex=.9)  
  return(p)
})

output$best_topic_num <-renderUI({
  names <- c("NULL","Calculate")
  selectizeInput("best_num", label = "Select Calculate to find the best topic number (Log Likelihood) - It may take a long time", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
}) 
########stmAnalys############
stmAnalysis <- reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  novel.vector <- as.list(RemoveWordsStepTwo()$d$word)
  corpus <- Corpus(VectorSource(novel.vector))
  tdm <-DocumentTermMatrix(corpus)   
  out <- readCorpus(tdm, type="dtm")
  documents <- out$documents
  vocab <- out$vocab
  n.topics <- as.numeric(input$num)
  stmmodel <- stm(documents, vocab, n.topics, verbose=FALSE)
  return(stmmodel)
})
### Print Topics

#### Print Documents for each topic
#Example of documents associated with topics
output$association <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  novel.vector <-RemoveWordsStepThree()$corpus#PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  K=input$num
  #par(mfrow = c(3, 3),mar = c(.5, .5, 1, .5))
  for (i in 1:K){
    thoughts <- findThoughts(stmmodel, text=novel.vector,topic=i,n = 1)
    #thoughts <- paste(strsplit(as.character(thoughts3$docs[1]), " ")," ")
    #thoughts5 <- findThoughts(stmmodel, text=novel.vector,topic=5,n = 1)
    plotQuote(thoughts, width = 30, main = paste("Topic",i, " "))
  }
})
### Summary
# Expected Topic Proportion
#########proportion#########
output$proportion <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  #par(mfrow=c(1,1),mar=c(5,5,5,5))
  plot.STM(stmmodel, type = "summary", xlim = c(0, .9))
})

########## perspective########
output$perspectives <- renderPlot ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  par(mfrow=c(1,1),mar=c(1,1,1,1))
  plot.STM(stmmodel, type = "labels")#, xlim = c(0, .9))#,xlim = c(1, 5))
})
##########cloud_stm########
output$cloud_stm <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  cloud(stmmodel, topic = K, scale = c(5,.25))
})

### graphical display of topic correlation
#########topic corelation#########
output$corelation <-renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  modoutcorr <- topicCorr(stmmodel)
  #modoutcorr$cor
  plot.topicCorr(modoutcorr,vertex.label.cex = 1.0)
})

output$topics_stm <- renderPrint({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  labelTopics(stmmodel)
  # stmmodel$documents
})





######## CLUSTER ANALYSIS######

cluster <-reactive ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  method=input$method
  distance = input$distance
  dtm <-ListTerms()$dtm
  m <- as.matrix(dtm)
  d<-dist(m,distance)
  fit <-hclust(d,method)
  
  return(fit)
})

output$cluster_plot <- renderPlot({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  par(cex=1.2,mar=c(5, 4, 4, 2))
  color=input$color
  k=as.numeric(input$cuttree)
  fit <- cluster()
  # fit$labels <- input$file.article.txt$name
  # if (!is.null(input$metadata_source)) {
  #   file.names <- fileData()$title
  # }
  if (!is.null(input$file.article.txt)) {
    file.names <- input$file.article.txt$name
    file.names <- abbreviate(file.names, 10)
  }
  else if (!is.null(input$file.article)){
    file.names <- input$file.article$name
    file.names <- abbreviate(file.names, 10)
  }
  else if (!is.null(input$file.tag)){
    file.names <- input$file.tag$name
    file.names <- abbreviate(file.names, 10)
  }
  else if (!is.null(input$file.rdf)){
    file.names <- zoteroData()$texts[[1]]#extractZoteroTerm()$titles
    file.names <- abbreviate(file.names, 10)
  }
  else if (!is.null(structured_data())) {
    file.names <- structured_data()$titles
    file.names <- abbreviate(file.names, 10)
  }
  # fit$labels <- file.names
  p<-  plot(fit)
  if (input$cuttree==0)
    #  p<-  plot(fit)
    p<- plot(fit,horiz=T,labels=file.names)
  else {
    #  p<-  plot(fit)
    p<-  plot(fit,horiz=T,labels=file.names)
    rect.hclust(fit,k, border=color)
  }
  # fit <- cluster()
  #return(fit)
})


output$cuttree <-renderUI({
  punct <- c(0,2,3,4,5,6,7,8,9)
  selectizeInput("cuttree", label = "Select number of groups", 
                 choices = punct,
                 selected = 0,
                 multiple = FALSE) 
})

######### streamgraph ###########
output$sg1 <- renderStreamgraph({
  
  #name <- c("populism","security", "populism", "security","populism", "security",
    #        "europe","defense","europe","defense","europe","defense")
  #year <- c(2001,2001,2002,2002, 2003, 2003, 2001,2001, 2002, 2002, 2003, 2003)
  #count <- c(100,50,150, 70, 50, 200, 80,40, 70,70, 100,150)
  
  d<-chronology()$M
  name <- as.character(d[,3])
  year <- as.Date(d[,2], format="%Y")
  count <-as.numeric(d[,4])
  
  myset <- data.frame(name,year,count)
 # head(myset)
 # colnames(myset)
  #tidyr::gather(name, count, -year) %>%
  # group_by(year, name) %>%
  myset %>% 
    #tally(wt=myset$count) %>%
    streamgraph(key="name", value="count", date="year") %>%
    # sg_axis_x(20) %>%
   # streamgraph(key="id", value=myset[,4], date="date") %>%
    sg_axis_x(1, "year", "%Y") %>%
    sg_fill_brewer("PuOr") %>%
    sg_legend(show=TRUE, label="Topics: ")
  })

output$movies <- renderStreamgraph({
 # data(movies)
  ggplot2movies::movies %>%
    select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
    tidyr::gather(genre, value, -year) %>%
    group_by(year, genre) %>%
    tally(wt=value) %>%
    streamgraph("genre", "n", "year") %>%
    sg_axis_x(20) %>%
    sg_fill_brewer("PuOr") %>%
    sg_legend(show=TRUE, label="Genres: ")
 # output$sg1 <- renderStreamgraph(sg)

})

output$stocks <- renderStreamgraph({
  stocks_url <- "http://infographics.economist.com/2015/tech_stocks/data/stocks.csv"
  stocks <- read.csv(stocks_url, stringsAsFactors=FALSE)
  
  stock_colors <- viridis_pal()(100)
  
  stocks %>% 
    mutate(date=as.Date(quarter, format="%m/%d/%y")) %>% 
    streamgraph(key="ticker", value="nominal", offset="expand") %>% 
    sg_fill_manual(stock_colors) %>% 
    sg_axis_x(tick_interval=10, tick_units="year") %>% 
    sg_legend(TRUE, "Ticker: ")
  # output$sg1 <- renderStreamgraph(sg)
  
})


output$print_data = renderTable({
  
  d<-chronology()$M
  name <- as.character(d[,3])
  year <- d[,2]
  count <-d[,4]
  
  myset <- data.frame(name,year,count)
  
})
output$chronology_top2 <- renderPrint ({
  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  chronology()$term
})
})