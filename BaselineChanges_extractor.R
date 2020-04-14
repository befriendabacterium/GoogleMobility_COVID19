# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd("~/Documents/Google Mobility Reports/2020-04-03/")

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('pdftools')
library(pdftools)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('stringi')
library(stringi)
#install.packages('stringr')
library(stringr)

# LOAD FUNCTIONS ----------------------------------------------------------

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# EXTRACT DATA FROM PDFS ------------------------------------------------------------

placetypes<-c("Retail & recreation", "Grocery & pharmacy", "Parks", "Transit stations", "Workplaces","Residential")
baselinechange_df<-c()

for (i in 1:length(list.files())){
  
  print(paste(i,"/",length(list.files())))
  
  extracted_text<-pdf_text(list.files()[i])
  
  extractedstrings<-unlist(stri_extract_all_regex(extracted_text, '\n\\S+\n'))
  
  extracted_baselinechanges<-numextract(extractedstrings)
  extracted_baselinechanges<-extracted_baselinechanges[!is.na(extracted_baselinechanges)]
  extracted_baselinechanges
  
  baselinechange_tempcols<-cbind(rep(i,6),placetypes,extracted_baselinechanges)
  baselinechange_df<-rbind(baselinechange_df,baselinechange_tempcols)
  }

colnames(baselinechange_df)[1]<-'filename'
setwd("~/Documents/Google Mobility Reports")
write.csv(baselinechange_df, paste('baselinechanges','_',Sys.Date(),".csv", sep=''), row.names = F)
