extractZoteroTxt <- function(zot_data,query1,query2,condition, len, between){
   # num <- length(x$name)
  require(tm)
  require(qdapRegex)
  #zot_data <- unlist(zot_data)
 # zot_data <- 
  #zot_data <- text.extract
  num <- length(zot_data[[5]])
  query1 = query1
  query2 = query2
  condition = condition
  len <- len
  between <- between
 # context <- 5
  lines <- list()
  text.extract <- list()
  #kwic.extract <- list()
#
  w = 1
  titles <- vector()
  authors <- vector()
  datetimes <- vector()
  abstracts <- vector()
  contents <- list()

   # texts <- vector()
   # titles <-vector()
   # authors <- vector()
  #  datetimes <- vector()
    for (i in 1:num) {
     # lines.merge=NA
      #content <- zot_data[[i]]
      content <- zot_data[[5]][i]
      corpus.collapse<-paste(content,collapse=" ")
      text.punct<-  gsub('[[:digit:]]+', '', corpus.collapse)
      text.punct <- tolower(text.punct)
      text.punct <-rm_citation(text.punct)
      text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
      text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
      text.punct <-rm_round(text.punct)
      text.punct <-rm_curly(text.punct)
      text.punct <-rm_square(text.punct)
      text.split<-unlist(strsplit(text.punct, "References|references|REFERENCES"))
      #text.split <- unlist(text.p)
      text.punct<-text.split[1]
      text.punct <- gsub("[^[:alnum:] ]", "", text.punct)
      text.punct <- gsub("\\s\\s+"," ",text.punct)
      # lda.list <- unlist(strsplit(corpus.collapse[[i]], "\\s+"))
      lda.list <- unlist(strsplit(text.punct, "\\s+"))
      # remove punctuation
     # if (condition %in% "and") {
        loc1 <- grep(query1, lda.list,perl=TRUE)
        loc2 <- grep(query2, lda.list,perl=TRUE)
        ### choose the smallest
       # list.loc <- list(loc1,loc2)
      #  if (length(loc1)<length(loc2)){
           z=0
          # ### Add between window
           for (k in 1:length(loc1)) {
            # if ((loc1[k]-between)>0) {
             strings <- lda.list[(loc1[k]):(loc1[k]+between)]
          #   }
          #   else {
          #     strings <- lda.list[(loc1[k]):(loc1[k]+between)]
          #   }
             if (query2 %in% strings) {
               z=z+1
               ### add left and right context
               if ((loc1[k]-between-len)<1 ){
                 match.string <- lda.list[(loc1[k]):(loc1[k]+between+len)]
               }
              else {
               match.string <- lda.list[(loc1[k]-between-len):(loc1[k]+between+len)]
              }
               line <- paste(match.string, collapse=" ")
               line <- gsub("\\s\\s+"," ",line)
               lines[[z]] <- line
              # z=z+1
             }
           }
          # if (z>0) {
          lines.merge <- paste(unlist(lines), collapse=" ")
        #  text.extract[[w]] <- lines.merge
          title <- zot_data[[1]][i]
          datetime <- zot_data[[4]][i]
          abstract <- zot_data[[2]][i]
          name <- zot_data[[3]][i]
          titles[w] <- title
          authors[w] <- name
          datetimes[w] <- datetime
          abstracts[w] <- abstract
          w=w+1
       #    }
      #  }
        # else {
        #     z=0
        #   ### Add between window
        #   for (k in 1:length(loc2)) {
        #    # if ((loc2[k]-between)>0) {
        #     strings <- lda.list[(loc2[k]):(loc2[k]+between)]
        #     if (query1 %in% strings) {
        #       z=z+1
        #       ### add left and right context
        #       match.string <- lda.list[(loc2[k]-between-len):(loc2[k]+between+len)]
        #       line <- paste(match.string, collapse=" ")
        #       line <- gsub("\\s\\s+"," ",line)
        #      # lines[[k]] <- line
        #       lines[[z]] <- line
        #       #z=z+1
        #     }
        #   }
        #     lines.merge <- paste(unlist(lines), collapse=" ")
        #   #  if (z>0) {
        #  # lines.merge <- paste(unlist(lines), collapse=" ")
        #  # text.extract[[w]] <- lines.merge
        #   title <- zot_data[[1]][i]
        #   datetime <- zot_data[[4]][i]
        #   abstract <- zot_data[[2]][i]
        #   name <- zot_data[[3]][i]
        #   titles[w] <- title
        #   authors[w] <- name
        #   datetimes[w] <- datetime
        #   abstracts[w] <- abstract
        #   w=w+1
        #   text.extract[[w]] <- lines.merge
        # }
        #contents[[i]] <- loc1
         #text.extract[[i]] <- content
        
     #   }
       # lines.merge <- paste(unlist(lines), collapse=" ")
       # if (!is.na(lines.merge)) {
      text.extract[[i]] <- lines.merge
          
   
    
       # }
       # text.extract[[i]] <- lda.list
    }
 # lines.merge <- paste(unlist(lines), collapse=" ")
  
  text.extract <- unlist(text.extract) 

    info <- list(contents=contents,text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors, abstracts=abstracts)
    return(info)
  }

  # #Get all the lines of interest in the file
 
#     text.hyphen <- gsub("-\\s+","",text.collapse)
#     text.space<- gsub("\\s\\s+"," ",text.hyphen)
#     # sent <- segment(text.space,what='sentences')
#     #tok<-tokenize(text.space)
#     # k <- kwic(text.space, query,len,valuetype="regex")
#     k <- kwic(text.space, query,wordnum,valuetype="regex")
#     kpaste <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#     if (grepl(query2,kpaste)) {
#       if (length(k)==5) {
#         
#         zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#         zot <- gsub("\\s\\s+"," ",zot)
#         text.extract[[z]] <- zot
#         text.full[z] <-text.space
#         titles[z] <- title
#         authors[z] <-author
#         datetimes[z] <- datetime
#         abstracts[z] <-abstract
#         z=z+1
#       }
#     }
#   }
#   
#   text.extract <- unlist(text.extract)
#   info <- list(text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors, text.full=text.full,abstracts=abstracts)
#   return(info)
# }