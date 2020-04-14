# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
dev.off()
setwd("~/Documents/Google Mobility Reports/2020-04-03/")

#install.packages('pdftools')
library(pdftools)
#install.packages('png')
library(png)
#install.packages('imager')
library(imager)
#install.packages('stringi')
library(stringi)
#install.packages('stringr')
library(stringr)

# LOAD FUNCTIONS ----------------------------------------------------------

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# STEP 1: READ IN PDFS AND CONVERT TO PNG FOR PROCESSING ------------------------------------------------------------

file=c('2020-03-29_GB_Mobility_Report_en.pdf')

testpdf_p1<-pdf_render_page(file, page=1, dpi=600)
writePNG(testpdf_p1, 'testpdf_p1.png')
p1= load.image('testpdf_p1.png')

testpdf_p2<-pdf_render_page(file, page=2, dpi=600)
writePNG(testpdf_p2, 'testpdf_p2.png')
p2= load.image('testpdf_p2.png')

# CROP OUT THE FIVE MAIN GRAPHS FROM THE FIRST TWO PAGES ------------------

p1 #get dims so know roughly where to crop graphs
#plot 1 - retail
retail<-imsub(p1,  x>1550, x<1450, y>2800,y<900) %>% plot(main='Retail & recreation', xlim=c(0,1500))
#plot 2 - grocery - add 300 to second y
grocery<-imsub(p1,  x>1550, x<1450, y>3775, y<900) %>% plot(main='Grocery & pharmacy', xlim=c(0,1500))
#plot 3 - parks
parks<-imsub(p1,  x>1550, x<1450, y>4750, y<900) %>% plot(main='Parks')
points(156,777)
#page 2
p2
#plot 4 - transit stations
transit<-imsub(p2, x>1416, x<1326, y>240,y<900) %>% plot(main='Transit stations')
points(156,777)
#plot 5 - workplaces
workplaces<-imsub(p2, x>1416, x<1326, y>1215, y<900) %>% plot(main='Workplaces')
#plot 6 - residential
residential<-imsub(p2, x>1416, x<1326, y>2190, y<900) %>% plot(main='Residential')

graphs<-imlist(retail, grocery, parks, transit, workplaces, residential)
names(graphs)<-c('Retail & recreation','Grocery & pharmacy', 'Parks', 'Transit stations', 'Workplaces', 'Residential')

# DRAW AXES LINES OVER THE IMAGE AND MAKE VECTORS OF THE XY COORDINATES OF EACH AXIS TICK MARK---------------

#actual lines lie top border of axis (y), left border of axis (x)

#plot and make vectors of x coordinates and associated dates 
xmin<-156
xmax<-1109
abline(v=xmin)
abline(v=xmax)
xint<-(xmax-xmin)/7/6 #divided by 7 weeks divided by y days

x_coordinates<-seq(xmin,xmax,by=xint)
x_dates<-seq(as.Date("2020-02-16"),as.Date("2020-03-29"), by='day')
abline(v=x_coordinates)
x_coordinates<-x_coordinates[-43] # minus 1 because retrieving last point is not accurate as is always off the actual graph..use reported figures instead for last point

#plot and make vectors of y coordinates and associated baseline changes 
ymin<-206
ymax<-777
yint=(ymax-ymin)/160

abline(h=ymin)
abline(h=ymax)

y_coordinates<-seq(ymin,ymax,by=yint)
y_changes<-seq(80,-80,by=-1)
abline(h=y_coordinates)

#extend sequence to 100% (some of the lines are outside the -80% +80% range
ymin<-ymin-(yint*40)
ymax<-ymax+(yint*40)
yint=(ymax-ymin)/240

y_coordinates<-seq(ymin,ymax,by=yint)
y_changes<-seq(120,-120,by=-1)
abline(h=y_coordinates)

# DETECT LINE POSITION (Y) AT DATES (X) -----------------------------------

dev.off()
pdf.name<-gsub(".*mobility/","",file)

extracted_df<-c()

for (g in 1:length(graphs)){
  
  im<-graphs[[g]] 
  plot(im, main=names(graphs[g]))
  
  extracted_changes<-c()
  
  for (p in 1:length(x_coordinates)){
    
    colours<-color.at(im,seq(x_coordinates[p]-4,x_coordinates[p]+4,1))[,,1]
    
    #count pixels of interest per line of vision
    bluepixelcounts<-colSums(colours<0.4)
    #count no of rows where colour is below 0.4, and subset out all rows where all 5 pixels are below 0.4 (right colour blue)
    where_line<-median(which(bluepixelcounts==max(bluepixelcounts)))
    points(x_coordinates[p],where_line, cex=0.5)

    diffs<-abs(where_line-y_coordinates)
    mindiff<-which(diffs==min(diffs))
    
    extracted_changes<-c(extracted_changes,y_changes[mindiff])
  }
  current_row<-c(pdf.name,names(graphs[g]),extracted_changes)
  extracted_df<-rbind(extracted_df, current_row) 
}

# ADD DATA FOR FINAL TIMEPOINT USING REPORT DATA--------------------------

  extracted_text<-pdf_text(file)
  
  extractedstrings<-unlist(stri_extract_all_regex(extracted_text, '\n\\S+\n'))
  
  extracted_baselinechanges<-numextract(extractedstrings)
  extracted_baselinechanges<-extracted_baselinechanges[!is.na(extracted_baselinechanges)]
  extracted_baselinechanges

#add last day (sunday) manually based on data in report
extracted_df<-cbind(extracted_df,extracted_baselinechanges)

rownames(extracted_df)<-NULL
colnames(extracted_df)<-c('file','place_type',format(as.Date(x_dates, format = "%d.%m.%y"),
                                                     format = "%Y-%m-%d"))
extracted_df[,3:45]<-as.numeric(extracted_df[,3:45])

# PLOT ORIGINAL IMAGE VS EXTRACTED DATA -----------------------------------

point_cols<-c(rep('black',42),'red')

dev.off()
par(mfrow=c(1,2))

for (g in 1:length(graphs)){
  
  im<-graphs[[g]] 
  plot(im, main=names(graphs[g]))
  
  plot(extracted_df[g,3:ncol(extracted_df)]~x_dates, ylim=c(-100,100), cex=0.75,xlab='Date', ylab='Baseline change (%)', main=names(graphs[g]), pch=21, bg=point_cols)
  lines(extracted_df[g,3:ncol(extracted_df)]~x_dates)
}

#either need to adjust median/min or axes alignment
