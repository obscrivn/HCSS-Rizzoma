library(tm)
uris.name <- "./files/137482/Giliker - 2015 - The Influence of Eu and European Human Rights Law .pdf"

uris.name2 <- "./files/137486/Otero-Iglesias - 2012 - The Influence of the Euro in Reshaping Global Mone.pdf"

uris.name3 <- "./files/137498/Tatham - 2014 - Judicialisation of Trade Policy and the Impact on .pdf"

tempPDF <- readPDF(control = list(info="-f",text = "-layout"))(elem = list(uri = uris.name3),language="en",id="id1")
read.file <- tempPDF$content
id <- tempPDF$meta$id
title <- tempPDF$meta$id
texts <- enc2utf8(read.file)


x <- "EuropeanInfluencetest4.rdf"
zoteroData <- zotero(x)
texts <- extractPdfZotero(zoteroData)
texts[[5]][1]
zot_data <- texts
q <- "human AND law"
query <- gsub("\\s+", " ", tolower(q))
query1  <- unlist(strsplit(query," "))[1]
query2 <- unlist(strsplit(query," "))[3]
condition <- unlist(strsplit(query," "))[2]
len <- 10
between <- 5
extract <- extractZoteroTxt(zot_data[[4]],query1,query2,condition, len, between)
library(streamgraph)
