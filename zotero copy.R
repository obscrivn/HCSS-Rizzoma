# parse rdf zotero files
 #x<- "/Users/olgascrivner/Documents/ITMS/TextMiningZotero/HCSS-Rizzoma/English - first spiral corpus.rdf"

 ## run zotero function 
#zotero.rdf <-  zotero(x)
#zotero.rdf[1]
#length(zotero.rdf)

## run new zotero function
#zotero_extract_rdf <- zotero_rdf(x)
# title, abstract,date,surname, firstname,link,type,path
#n = 1
#path <- list()
# for (i in 1:length(zotero_extract_rdf)) {
#   l <- unlist(zotero_extract_rdf[[i]])
#   p <- l[length(l)]
#   path[[n]] <- p
#   n=n+1
# }
#zotero.rdf <- zotero_rdf(x)
# query="europ"
 query1="cyberspace"
query2="operations"
# query2="influence"
 len = 10
 path_to_rdf = x
# k=1
 dir.list <- path
 zot_data <- extract
 query1 <- "cyberspace"
 query2 <- "operations"
 condition <- "AND"
 len = 10
# between = 3
# num <- length(extract[[5]])
 #i=1
# extract <- extract.txt
 #extract.txt <- extractZoteroTxt(zot_data,query1,query2,condition, len,between)
# for (i in 1:length(zot.pdf)) {
#   if (zot.pdf[i]!="") {
#     text <- zot.pdf[i]
#     #path <- paste0(dir,text)
#     path <- paste0("/Users/olgascrivner/Documents/ITMS/Summer2017/TestingFiles/",text)
#     dir.list[k] <- path
#     k <- k+1
#   }
# }
# 
# extract <- extractPdfZotero(zotero_extract_rdf)
# mydata <- extractZotero(dir.list,query,query2,len, path_to_rdf)
# mydata$titles

# mydata <- extractZotero(zot.pdf,query,query2,len, path_to_rdf)

zotero_rdf <- function(x) {
  require(XML)
  xml = xmlTreeParse(x,useInternalNodes=TRUE)
  k=1
  article <- vector()
  articles <- list()
  nodes = getNodeSet(xml, "//bib:Article")
  attachments <- getNodeSet(xml, "//z:Attachment") 
  for (i in 1:length(nodes)) {
    node <- nodes[[i]]
    doc <- xmlToList(node)
    title <-   doc$title
    abstract <- doc$abstract
    date <- doc$date
    surname <- doc$authors$Seq$li$Person$surname
    firstname <- doc$authors$Seq$li$Person$givenname
    link <- doc$link@.Data
    
   # attachments <- getNodeSet(xml, "//z:Attachment") 
  #  length(attachments) # 493
  #  length(nodes) # 159
   # zmatch <- "#item_2477"
   # link %in% attachments
   # class(attachments)
   # getNodeSet(attachments)
    
    links <- sapply(attachments, xmlAttrs)
   # class(attachment)
   # attachment[1]
   # length(attachment)
    num <- grep(link, links)
  #  link %in% attachment
   # df <- data.frame(attachments)
    ## find attachment with the link 
    ## z:Attachment rdf:about="#item_2477"
    attachment <- attachments[[num]]
    doc2 <- xmlToList(attachment)
    
    ## Test for file item # between article and  attachement
    if (link %in% doc2$.attrs[[1]]==TRUE){
      type <-  doc2$type # test pdf
      path <-   doc2$resource@.Data  # file path
     # if ((!is.null(type))) {
     # if ((type %in% "application/pdf"==TRUE) || (type %in% "text/html"==TRUE)) {
        article <- c(title, abstract,date,surname, firstname,link,type,path)
        articles[[k]] <- article
        k=k+1
     # }
      }
      else {
        
      }
      }
   # articles[[k]] <- article
   # k=k+1
   # }
  return(articles)
}
  zotero <- function(x) {
     uris.name <- x
# #Get all the lines of interest in the file
 zot.lines <- readLines(uris.name)#zot.file)
 zot.lines <- zot.lines[grep("<rdf:resource",zot.lines)]
 #zot.lines <- zot.lines[grepl(".pdf\"", zot.lines)]

zot.line.parser <- function(z){
  #This takes one of our rdfresource lines and returns the pdf title
  #This relies on the rdf:resource tag having exactly two double quotes
  #Seems safe enough, but just to be safe we take the first and last quote mark
  #This avoids issues with quotes in a title/file name.
  require(stringr)
  require(tools)
  first <- 1
  last <- dim(str_locate_all(z,'"')[[1]])[1]
  
  start <- str_locate_all(z,'"')[[1]][first] + 1
  stop <- str_locate_all(z[1],'"')[[1]][last] -1
  substr(z, start, stop)
}

zot.pdf <- unlist(lapply(zot.lines, zot.line.parser))

   }
   
zoteroMeta <- function(x) {
  uris.name <- x
  # #Get all the lines of interest in the file
  zot.lines <- readLines(uris.name)#zot.file)
  date.lines <- zot.lines[grep("<dc:date",zot.lines)]
  author.lines <- zot.lines[grep("<foaf:surname",zot.lines)]
  abstract.lines <- zot.lines[grep("<dcterms:abstract",zot.lines)]
  date.lines <- gsub("<dc:date>","",date.lines)
  date.lines <- gsub("</dc:date>","",date.lines)
  date.lines <- gsub("[a-zA-Z\\s]*","",date.lines)
  date.lines <- as.integer(date.lines)
  author.lines <- gsub("<foaf:surname>","",author.lines)
  author.lines <- gsub("</foaf:surname>","",author.lines)
  author.lines <-gsub("\\s+","",author.lines)
  abstract.lines <- gsub("<dcterms:abstract>","",abstract.lines)
  abstract.lines <- gsub("</dcterms:abstract>","",abstract.lines)
  abstract.lines <-gsub("\\s+"," ",abstract.lines)
  info <- list(date.lines,abstract.lines,author.lines,zot.pdf)
  return(info)
} 

#extractPdfZotero <- function(list.rdf,zot.pdf){
#### extract pdf files from a list of rdf articles
extractPdfZotero <- function(zot.rdf) {
 # list.rdf <- list.rdf
 # num <- length(list.rdf)
 # zot.rdf <- zot.pdf
  require(tm)
 num <- length(zot.rdf) # list of rdf articles with sublist of 5 title, abstract,date,surname, firstname,link,type,path
  titles <-vector()
authors <-vector()
  datetimes <- vector()
  abstracts <- vector()
  names <- vector()
  # cont <- rep("",num)
  text.extract <- list()
  for (i in 1:num) {
    lists <- zot.rdf[[i]]
   # uris.name <- as.character(paste0(list.rdf[i]))
    uris.name <- lists[length(lists)]#lists[8]
    # uris.name <- zotero.rdf[5]
   # uris.name <- "/Users/olgascrivner/Documents/ITMS/TextMiningZotero/HCSS-Rizzoma/files/3422/Adamsky_2018_From Moscow with coercion.pdf"
    # if (nchar(uris.name)<100) {
    tempPDF <- readPDF(control = list(info="-f",text = "-layout"))(elem = list(uri = uris.name),
                                                                   language="en",id="id1")
    read.file <- tempPDF$content
  #  id <- tempPDF$meta$id
  #  title <- tempPDF$meta$id
    texts <- enc2utf8(read.file)
    text.collapse <- paste(texts,collapse=" ")
    text.hyphen <- gsub("-\\s+","",text.collapse)
    text.space <- gsub("\\s\\s+"," ",text.hyphen)
  #  split_title <- unlist(strsplit(id," - "))
   # if (!is.null(tempPDF$meta$datetimestamp)) {
     # title <- tempPDF$meta$heading
     # author <- tempPDF$meta$author # Alan Ritter ; Colin Cherry ; Bill Dolan
      #id <-tempPDF$meta$id
    #  des <- tempPDF$meta$datetimestamp
   #   y <- strsplit(as.character(des), "-")
   #   datetime <- y[[1]][1]
     # name <- tempPDF$meta$id
  #  }
  #  if (!is.null(tempPDF$meta$author)) {
    #  title <- tempPDF$meta$id
     # name <- tempPDF$meta$id
  #    author <- tempPDF$meta$author
  #  }
     # des <- tempPDF$meta$datetimestamp
    #  y <- strsplit(as.character(des), "-")
    #  datetime <- des#y[[1]][1]
  #  }
  #   if (is.null(tempPDF$meta$datetimestamp)) {
  #     datetime <- split_title[2]#strsplit(id," - ")[[1]]
    #   zoteroData()$texts[[1]]
     #  title <- "NA"
   #  }
   #  if (is.null(tempPDF$meta$author)) {
   #    author <- split_title[1] #strsplit(id," - ")[[1]]
    #   author <- "NA"
   #  }
   #  if (length(datetime)<1) {
   #    datetime <- strsplit(id," - ")[[2]]
   #  }
    # if (length(name)<1) {
    #   name <-x$name[i]
    # }
    title <- lists[1]
    datetime <- lists[3]
    abstract <- lists[2]
    name <- paste0(lists[5]," ",lists[4])
    titles[i] <- title
    authors[i] <- name
    datetimes[i] <- datetime
    names[i] <- name
    abstracts[i] <- abstract
    text.extract[[i]] <- text.space
   # text.extract <- unlist(text.extract)
    
    
    #  }
  }
  info <- list(titles=titles, abstracts=abstracts, authors=authors,datetimes=datetimes, text.extract=text.extract) 
  return(info)
}


#extract <- extractZotero(uris.name,query,query2,len, x)
#extract
extractZotero <- function(links_pdf,query,query2,len, path_to_rdf){#, words){#p1,p2) {
  require(tm)
  require(quanteda)
  path = links_pdf
 # path <- dir.list
  query = query
  query2 = query2
  len <- len
  context <- 5
  num <- length(path)
  text.extract <- list()
  #text.full <- vector()
  titles <- vector()
  authors <- vector()
  datetimes <- vector()
  z = 1
 #  uris.name = zotero.rdf[3]
  for (i in 1:num) {
    uris.name <- paste0(path[i])
    #uris.name <- "/Users/olgascrivner/Documents/ITMS/TextMiningZotero/HCSS-Rizzoma/English - first spiral corpus.rdf"
    # uris.name <-"/Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles/files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"
    tempPDF <- readPDF(control = list(info="-f",text = "-layout"))(elem = list(uri = uris.name),language="en",id="id1")
    read.file <- tempPDF$content
    
    read.meta <-tempPDF$meta$id
   # year <- tempPDF$meta$datetimestamp
    author <- tempPDF$meta$author
    texts <- enc2utf8(read.file)
    #  pat1 <- grep(pattern1, texts, ignore.case = TRUE)
    # pat2 <- grepl(pattern2, texts, ignore.case = TRUE)
    text.collapse <- paste(texts,collapse=" ")
    text.hyphen <- gsub("-\\s+","",text.collapse)
    text.space <- gsub("\\s\\s+"," ",text.hyphen)
    # pat1 <- grepl(pattern1, text.space, ignore.case = TRUE)#,perl=TRUE)
    #if ((patern1 %in% texts) && (pattern2 %in% texts)) {
   # if (!is.null(tempPDF$meta$heading)) {
    #  title <- tempPDF$meta$heading
    #  author <- tempPDF$meta$author # Alan Ritter ; Colin Cherry ; Bill Dolan
      id <- tempPDF$meta$id
 #   name <- tempPDF$meta$id
   # }
   # if (is.null(tempPDF$meta$heading)) {
   #   title <- tempPDF$meta$id
    #  name <- tempPDF$meta$id
    #  author <- tempPDF$meta$author
    #  des <- tempPDF$meta$datetimestamp
    #  y <- strsplit(as.character(des), "-")
    #  datetime <- des#y[[1]][1]
      des <- tempPDF$meta$datetimestamp
     # y <- strsplit(as.character(des), "-")
      datetime <-des# y[[1]][1]
   # }
    #   if (length(title)<1) {
    #   zoteroData()$texts[[1]]
    #  title <- "NA"
    # }
    if (length(author)<1) {
      author <- strsplit(id," - ")[1]
      #   author <- "NA"
    }
      if (length(datetime) < 1) {
        datetime <- strsplit(id," - ")[2]
      }
     # else {
     #   des <- tempPDF$meta$datetimestamp
     #   y <- strsplit(as.character(des), "-")
     #   datetime <- y[[1]][1]
        
     # }
    # if (length(name)<1) {
    #   name <-x$name[i]
    # }

    #  if (sum(pat1)>0) {#&& (sum(pat2)>0)) {
    # text.full[z] <-text.space
    # text.split <-unlist(strsplit(text.space," "))
   #   k <- kwic(text.space, query,len,valuetype = "regex")
    #  z=1
      k <- kwic(text.space, query,context,valuetype = "regex")
              kpaste <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
              if (grepl(query2,kpaste)) {
                if (length(k) == 5) {
      #            
                zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
                zot <- gsub("\\s\\s+"," ",zot)
                text.extract[[z]] <- zot
   # k <- kwic(text.space, query,len)
    # if (!is.na(k)) {
  #  if (length(k) == 5) {
  #    zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
   #   zot <- gsub("\\s\\s+"," ",zot)
     # titles[z] <- read.meta
      text.extract[[z]] <- zot
    #  text.full[z] <- text.space
      z = z+1
      titles[i] <- read.meta
      authors[i] <- author
      datetimes[i] <- datetime
     # names[i] <- name
      # text.extract[[1]] <-text.space
    }
    
              }
    
  }
  
  text.extract <- unlist(text.extract)
  info <- list(text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors)
  
  return(info)
}

# extractZotero <- function(extracts,query,query2,len,words,author,title,date){#p1,p2) {
#   extract = extracts
#   # wordnum=as.numeric(words)  
#   author.extract <- author
#   title.extract <-title
#   date.extract <- date
#   query=query
#   query2=query2
#   # query<- unlist(strsplit(query1," "))[1]
#   len <-len
#   num <- length(extract)
#   text.extract <- list()
#   # text.extract[[1]] <-text.space
#   #  text.full <- vector()
#   titles <- vector()
#   authors <- vector()
#   datetimes <- vector()
#   # abstracts <- vector()
#   #  uris.name <- x
#   # #Get all the lines of interest in the file
#   #  zot.lines <- readLines(uris.name)#zot.file)
#   #   date.lines <- zot.lines[grep("<dc:date",zot.lines)]
#   #   author.lines <- zot.lines[grep("<foaf:surname",zot.lines)]
#   #  abstract.lines <- zot.lines[grep("<dcterms:abstract",zot.lines)]
#   #  title.lines <- zot.lines[grep("<dc:title>",zot.lines)]
#   #  date.lines <- gsub("<dc:date>","",date.lines)
#   #  date.lines <- gsub("</dc:date>","",date.lines)
#   #  date.lines <- gsub("[a-zA-Z\\s]*","",date.lines)
#   #  data.lines <- as.integer(date.lines)
#   # author.lines <- gsub("<foaf:surname>","",author.lines)
#   # author.lines <- gsub("</foaf:surname>","",author.lines)
#   #  author.lines <- gsub("\\s+","",author.lines)
#   #  abstract.lines <- gsub("<dcterms:abstract>","",abstract.lines)
#   #  abstract.lines <- gsub("</dcterms:abstract>","",abstract.lines)
#   #  abstract.lines <- gsub("\\s+"," ",abstract.lines)
#   #  title.lines <- gsub("<dc:title>","",title.lines)
#   #  title.lines <- gsub("</dc:title>","",title.lines)
#   #  title.lines <- gsub("\\s+"," ",title.lines)
#   
#   n <- 1
#   for (i in 1:num) {
#     data <- extract[i]
#     # data<- text.extract[1]
#     text <- paste(data, collapse = " ")
#     text.hyphen <- gsub("-\\s+","",text)
#     text.space<- gsub("\\s\\s+"," ",text.hyphen)
#     #kwiclist<-vector()
#     # if (paths[i]!="") {
#     # p <- unlist(strsplit(paths[i],"\\\\|/"))
#     #pjoin <- paste0("./",p[1],"/",p[2])
#     # if (length(list.files(path=pjoin))>1) {
#     #   uris <- gsub(".pdf","",paths[i])
#     #  uris.name <- paste0(uris,".txt")
#     
#     #  if (is.na(file.size(uris.name))==FALSE) {
#     #   if (file.size(uris.name)>0) {
#     # if (length(uris.name)>0) {
#     # if (file.exists(uris.name)) {
#     #uris.name <- paste0(paths[i])
#     #  uris.name <- paths[i]
#     # text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = TRUE)  
#     # if (length(text.scan)>0) {
#     # data=enc2utf8(text.scan)
#     # if (length(data)>0){
#     #  texts <- paste(data, collapse = " ")
#     # uris.name <-"/Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles/files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"
#     # tempPDF <- readPDF(control = list(info = "-f",text = "-layout"))(elem = list(uri = uris.name),language = "en",id = "id1")
#     # tempPDF <- readPDF(control = list(text = "-layout"))(elem = list(uri = uris.name),language = "en",id = "id1")
#     # read.file <- tempPDF$content
#     #   datetime <- data.lines[i]
#     # abstract <- abstract.lines[i]
#     #  author <- author.lines[i]
#     #  title <- title.lines[i]#tempPDF$meta$id
#     #  texts <- enc2utf8(read.file)
#     #  pat1 <- grep(pattern1, texts, ignore.case = TRUE)
#     # pat2 <- grepl(pattern2, texts, ignore.case = TRUE)
#     #  text.collapse <- paste(texts,collapse=" ")
#     #  text.collapse <- paste(data,collapse=" ")
#     #  text.collapse <- paste(texts,collapse=" ")
#     #    text.hyphen <- gsub("-\\s+","",text.collapse)
#     #    text.space<- gsub("\\s\\s+"," ",text.hyphen)
#     # sent <- segment(text.space,what='sentences')
#     #tok<-tokenize(text.space)
#     # k <- kwic(text.space, query,len,valuetype="regex")
#     # if (length(text.space)>0) {
#     # k <- kwic(text.space, query,wordnum,valuetype = "regex")
#     k <- kwic(text.space, query,len,valuetype = "regex")
#     # k[,3]
#     # kpaste <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#     #  kpaste <- paste(k[,3],k[,4],k[,5])
#     # kpaste[1]
#     # query2="Union"
#     if (length(k)==5) {
#       kpaste <- paste(k[,3],k[,4],k[,5])
#       kpaste <- gsub("\\s+"," ",kpaste)
#       #for (n in 1:length(kpaste)) {
#       # grepl(query2,kpaste[n]) 
#       pat <- grep(query2,kpaste)
#       if (length(pat>0)) {
#         zots <- kpaste[pat]
#         zot <-paste(zots, collapse=" ")
#         #q<-  grep("european", unlist(strsplit(kpaste[1]," ")),ignore.case = TRUE)
#         # zot <-kpaste[n] #paste(k[1,3],k[1,4],k[1,5])
#         # paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#         # zot <- gsub("\\s\\s+"," ",zot)
#         # kwiclist[n] <- zot
#         # }
#         # kwicpaste <- paste(kwiclist,collapse=" ")
#         author <- author.extract[i]
#         title <- title.extract[i]
#         datetime <- date.extract[i]
#         #   datetime <- data.lines[i]
#         #  abstract <- abstract.lines[i]
#         #  author <- author.lines[i]
#         #  title <- title.lines[i]#tempPDF$meta$id
#         text.extract[[n]] <- zot
#         #   text.full[i] <-text.space
#         titles[n] <- title
#         authors[n] <-author
#         datetimes[n] <- datetime
#         #  abstracts[i] <- abstract
#         text.extract <- unlist(text.extract)
#         n=n+1
#         # z=z+1
#         # }
#         #  }#else {z=z+1}
#         # text.extract <- unlist(text.extract)
#       }
#     }
#   }
#   #  text.extract <- unlist(text.extract)
#   # return(text.extract)
#   info <- list(text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors)#, text.full=text.full,abstracts=abstracts)
#   return(info)
# }
# extractZotero <- function(y,query,query2,len,x,words){#p1,p2) {
#   path = y
#   wordnum=as.numeric(words)
#   query=query
#  query2=query2
#   # query<- unlist(strsplit(query1," "))[1]
#   len <-len
#    num <-length(path)
#     text.extract <- list()
#    # text.extract[[1]] <-text.space
#     text.full <- vector()
#     titles <-vector()
#     authors <- vector()
#     datetimes <- vector()
#     abstracts <- vector()
#     uris.name <- x
#     # #Get all the lines of interest in the file
#     zot.lines <- readLines(uris.name)#zot.file)
#     date.lines <- zot.lines[grep("<dc:date",zot.lines)]
#     author.lines <- zot.lines[grep("<foaf:surname",zot.lines)]
#     abstract.lines <- zot.lines[grep("<dcterms:abstract",zot.lines)]
#     date.lines <- gsub("<dc:date>","",date.lines)
#     date.lines <- gsub("</dc:date>","",date.lines)
#     date.lines <- gsub("[a-zA-Z\\s]*","",date.lines)
#     data.lines <- as.integer(date.lines)
#     author.lines <- gsub("<foaf:surname>","",author.lines)
#     author.lines <- gsub("</foaf:surname>","",author.lines)
#     author.lines <-gsub("\\s+","",author.lines)
#     abstract.lines <- gsub("<dcterms:abstract>","",abstract.lines)
#     abstract.lines <- gsub("</dcterms:abstract>","",abstract.lines)
#     abstract.lines <-gsub("\\s+"," ",abstract.lines)
#     z=1
#     for (i in 1:num) {
#       uris.name <- paste0(path[i])
#      # uris.name <-"/Users/olgascrivner/Documents/MCS/TextMiningClass/Project/TestingFiles/files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"
#        tempPDF <- readPDF(control = list(info="-f",text = "-layout"))(elem = list(uri = uris.name),language="en",id="id1")
#        read.file <- tempPDF$content
#        datetime <- data.lines[i]
#        abstract <-abstract.lines[i]
#        author <- author.lines[i]
#        title <- tempPDF$meta$id
#        texts <- enc2utf8(read.file)
#       #  pat1 <- grep(pattern1, texts, ignore.case = TRUE)
#       # pat2 <- grepl(pattern2, texts, ignore.case = TRUE)
#        text.collapse<-paste(texts,collapse=" ")
#        text.hyphen <- gsub("-\\s+","",text.collapse)
#        text.space<- gsub("\\s\\s+"," ",text.hyphen)
#         # sent <- segment(text.space,what='sentences')
#          #tok<-tokenize(text.space)
#         # k <- kwic(text.space, query,len,valuetype="regex")
#        k <- kwic(text.space, query,wordnum,valuetype="regex")
#        kpaste <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#        if (grepl(query2,kpaste)) {
#          if (length(k)==5) {
#            
#          zot <- paste(k$contextPre,k$keyword,k$contextPost,collapse=" ")
#          zot <- gsub("\\s\\s+"," ",zot)
#          text.extract[[z]] <- zot
#          text.full[z] <-text.space
#          titles[z] <- title
#          authors[z] <-author
#          datetimes[z] <- datetime
#          abstracts[z] <-abstract
#          z=z+1
#          }
#        }
#     }
# 
#     text.extract <- unlist(text.extract)
#     info <- list(text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors, text.full=text.full,abstracts=abstracts)
#     return(info)
# }

