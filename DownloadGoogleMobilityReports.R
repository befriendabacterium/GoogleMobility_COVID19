# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd("~/Documents/Google Mobility Reports")

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)

# DOWNLOAD PDFS -----------------------------------------------------------
download.folder = paste(getwd(),Sys.Date(), sep='/')
download.folder
dir.create(download.folder)
setwd(download.folder)

base.url<-"https://www.google.com/covid19/mobility/"
base.url2<-getURL(base.url)
parsed<-htmlParse(base.url2)
doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
doc.links
pdf.urls <- as.character(doc.links[grep('pdf', doc.links)])
pdf.urls

pdf.names_all<-gsub(".*mobility/","",pdf.urls)

for (i in 1:length(pdf.urls)){
  if(length(pdf.urls) > 0) {
    pdf.name <- pdf.names_all[i]
    download.file(pdf.urls[i], pdf.name, method = 'auto', quiet = FALSE, mode = "w",
                  cacheOK = TRUE, extra = getOption("download.file.extra"))
  }  
}

