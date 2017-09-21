extractZoteroTxt <- function(zot_data,query,query2,len, dir.list){
   # num <- length(x$name)
  zot_data <- zot_data
  #zot_data <- text.extract
  num <- length(zot_data)
  query = query
  query2 = query2
  len <- len
  context <- 5
  text.extract <- list()
  kwic.extract <- list()
  z = 1
   # texts <- vector()
   # titles <-vector()
   # authors <- vector()
  #  datetimes <- vector()
    for (i in 1:num) {
     # uris.name <- x$datapath[i]
    #  text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)  
      data = zot_data[[i]]
      text.collapse <- paste(data, collapse = " ")
      text.hyphen <- gsub("-\\s+","",text.collapse)
      text.space <- gsub("\\s\\s+"," ",text.hyphen)
      text.split <- unlist(strsplit(text.space, " "))
     # text <- paste(data[i], collapse = " ")
      
     # text <- gsub("-\\s+", "", text) 
    #  texts[i] <- text
      #nrow(k)
      k <- kwic(text.space, query,context,valuetype = "regex")
      kpaste <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
      if (grepl(query2,kpaste)) {
        if (length(k) == 5) {
          for (l in 1:nrow(k)) {
         # text.split[6+1]
          if (((k$position[l]+1)-context-len)<1) {
            start <- (k$position[l]+1)-context
          } else if (((k$position[l]+1)-context-len)>0) {
            start <- (k$position[l]+1)-context-len
          }
          if (((k$position[l]+1)+context+len) > length(text.split)) {
            end <- k$position[l]+1+context
          }  else {
            end  <- k$position[l]+1+context+len
              
          }
            if (start<1) {
              start <- k$position[l]+1
            }
            if ((end <1) | (end > length(text.split))) {
              end <- k$position[l]+1
            }

           # text.split[((k$position[1]+1)-context-len):((k$position[1]+1)+context+len)]            
       zot <-  paste(text.split[start:end], collapse= " ")
         # zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
          zot <- gsub("\\s\\s+"," ",zot)
          kwic.extract[[l]] <- zot
          }
          kwic.paste <- paste(unlist(kwic.extract), collapse=" ")
          text.extract[[i]] <- kwic.paste
         # text.space[k$keyword]
         # text.extract[[z]] <- zot
          # k <- kwic(text.space, query,len)
          # if (!is.na(k)) {
          #  if (length(k) == 5) {
          #    zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
          #   zot <- gsub("\\s\\s+"," ",zot)
          # titles[z] <- read.meta
         # text.extract[[z]] <- zot
          #  text.full[z] <- text.space
        #  z = z+1
        }
     # name.parse <- strsplit(x$name[i],"-")
     # author.parse <- name.parse[1]
    #  year.parse <- name.parse[2]
    #  title.parse <- name.parse[3]
    #  titles[i] <- title.parse
     # authors[i] <- author.parse
    #  datetimes[i] <- year.parse
      }
    }
  text.extract <- unlist(text.extract)
  #  info <- list(texts=texts,titles=titles,datetimes=datetimes, authors=authors)
    return(text.extract)
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