# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
dev.off()

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

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# STEP 1: READ IN PDFS AND CONVERT TO PNG FOR PROCESSING ------------------------------------------------------------

pdf='2020-03-29_GB_Mobility_Report_en.pdf' #name of report PDF to be processed - should be a report over 3 pages long that includes local-level graphs (e.g. Great Britain report)
page_number=9 #page number to be processed - should be between 3 and the total number of pages in the report-1 (last page is explanatory)
start_date<-"2020-02-16" #start date of the time period covered by the report (YYYY-MM-DD)
end_date<-"2020-03-29" #end date of the time period covered by the report (YYYY-MM-DD)

# STEP 2: READ IN PDFS AND CONVERT TO PNG FOR PROCESSING ------------------------------------------------------------

writePNG(pdf_render_page(pdf, page=page_number, dpi=600), 'currentpage.png')
currentpage = load.image('currentpage.png')

# STEP 3: CROP OUT THE FIVE MAIN GRAPHS FROM THE FIRST TWO PAGES ------------------

currentpage #get dims so know roughly where to crop graphs
#plot 1 - retail
retail_1<-imsub(currentpage,  x>430, x<1130, y>925, y<700) %>% plot(main='Retail & recreation', xlim=c(0,1500))
#plot 2 - grocery - add 300 to second y
grocery_1<-imsub(currentpage,  x>1883, x<2583, y>925, y<700) %>% plot(main='Grocery', xlim=c(0,1500))
#plot 3 - parks
parks_1<-imsub(currentpage,  x>3336, x<4006, y>925, y<700) %>% plot(main='Parks', xlim=c(0,1500))
#plot 4 - transit stations
transit_1<-imsub(currentpage,  x>430, x<1127.5, y>2073, y<700) %>% plot(main='Transit stations', xlim=c(0,1500))
#plot 5 - workplaces
workplaces_1<-imsub(currentpage,  x>1883, x<1453, y>2073, y<700) %>% plot(main='Workplaces', xlim=c(0,1500))
#plot 6 - residential
residential_1<-imsub(currentpage,  x>3336, x<2876, y>2073, y<700) %>% plot(main='Residential', xlim=c(0,1500))

#plot 1 - retail
retail_2<-imsub(currentpage,  x>430, x<1130, y>3650, y<700) %>% plot(main='Retail & recreation', xlim=c(0,1500))
#plot 2 - grocery - add 300 to second y
grocery_2<-imsub(currentpage,  x>1883, x<2583, y>3650, y<700) %>% plot(main='Grocery', xlim=c(0,1500))
#plot 3 - parks
parks_2<-imsub(currentpage,  x>3336, x<4006, y>3650, y<700) %>% plot(main='Parks', xlim=c(0,1500))
#plot 4 - transit stations
transit_2<-imsub(currentpage,  x>430, x<1127.5, y>4798, y<700) %>% plot(main='Transit stations', xlim=c(0,1500))
#plot 5 - workplaces
workplaces_2<-imsub(currentpage,  x>1883, x<1453, y>4798, y<700) %>% plot(main='Workplaces', xlim=c(0,1500))
#plot 6 - residential
residential_2<-imsub(currentpage,  x>3336, x<2876, y>4798, y<700) %>% plot(main='Residential', xlim=c(0,1500))

graphs<-imlist(retail_1, grocery_1, parks_1, transit_1, workplaces_1, residential_1,
               retail_2, grocery_2, parks_2, transit_2, workplaces_2, residential_2)

names(graphs)<-c('Retail & recreation','Grocery & pharmacy', 'Parks', 'Transit stations', 'Workplaces', 'Residential',
                 'Retail & recreation','Grocery & pharmacy', 'Parks', 'Transit stations', 'Workplaces', 'Residential')

# DRAW AXES LINES OVER THE IMAGE AND MAKE VECTORS OF THE XY COORDINATES OF EACH AXIS TICK MARK---------------

#actual lines lie top border of axis (y), left border of axis (x)

#plot and make vectors of x coordinates and associated dates 
plot(workplaces_2)
xmin<-157 #before was 157
xmax<-1107#before was 1107
abline(v=xmin)
abline(v=xmax)
xint<-(xmax-xmin)/6/7 #divided by 6 weeks divided by 7 days

x_coordinates<-seq(xmin,xmax,by=xint)
x_coordinates[length(x_coordinates)]<-x_coordinates[length(x_coordinates)]-2
abline(v=x_coordinates)
x_dates<-seq(as.Date(start_date),as.Date(end_date), by='day')

#plot and make vectors of y coordinates and associated baseline changes 
plot(workplaces_2)
ymin<-184.5 #before was 185
ymax<-662.5 #before was 662
yint=(ymax-ymin)/160

abline(h=ymin)
abline(h=ymax)

y_coordinates<-seq(ymin,ymax,by=yint)
y_changes<-seq(80,-80,by=-1)
abline(h=y_coordinates)

#extend sequence to 100% (some of the lines are outside the -80% +80% range
ymin<-ymin-(yint*40)
ymax<-ymax+(yint*40)
yint=(ymax-ymin)/2400

y_coordinates<-seq(ymin,ymax,by=yint)
y_changes<-seq(120,-120,by=-0.1)
abline(h=y_coordinates)

# DETECT LINE POSITION (Y) AT DATES (X) -----------------------------------

dev.off()
pdf.name<-gsub(".*mobility/","",pdf)

extracted_df<-c()

for (g in 1:length(graphs)){
  
  im<-graphs[[g]] 
  plot(im, main=names(graphs[g]),xlim=c(0,1500))
  abline(h=range(y_coordinates)+c(yint*400,-yint*400))
  
  extracted_changes<-c()
  
  for (p in 1:length(x_coordinates)){
    
    colours<-t(color.at(im,x_coordinates[p]))
    #colours<-color.at(im,seq(x_coordinates[p]-1,x_coordinates[p]+1,1))[,,1]
    #change pixel counts for the text at top from black (0) to white (1) - workaround
    colours[,1:75]<-1
    #count pixels of interest per line of vision
    bluepixelcounts<-colSums(colours<0.4, na.rm=T)
    #if there is no pixel blue/under 0.4 because of missing data, then put in an NA
    if (sum(bluepixelcounts)==0) {
      extracted_changes<-c(extracted_changes,NA)
    }
    else{
    #count no of rows where colour is below 0.4, and subset out all rows where all 5 pixels are below 0.4 (right colour blue)
    where_line<-median(which(bluepixelcounts==max(bluepixelcounts)))
    points(x_coordinates[p],where_line, cex=0.5)
    diffs<-abs(where_line-y_coordinates)
    mindiff<-which(diffs==min(diffs))
    extracted_changes<-c(extracted_changes,y_changes[mindiff])
    }
    
  }
  extracted_df<-rbind(extracted_df, extracted_changes) 
}

extracted_df[,43]
points(x_coordinates[p],y_coordinates[which(y_changes==-48)], cex=0.5, col='red')
points(x_coordinates[p],y_coordinates[which(y_changes==-49)], cex=0.5, col='red')


 # ADD DATA FOR FINAL TIMEPOINT USING REPORT DATA--------------------------

extracted_text<-pdf_text(pdf)
extracted_text<-extracted_text[page_number]
extracted_text_lines<-unlist(strsplit(extracted_text,'\n'))

#extract county names
extracted_locality<-extracted_text_lines[which(grepl("Retail & recreation",extracted_text_lines)==T)-1]
extracted_locality

#extract 'not enough data info' i.e. check for asterix after each place type
patterns<-paste(names(graphs),".*?","\\*", sep='') #spit out first 150 characters after each place type 
enoughdata<-c()
for (t in 1:12){
  labelstring<-regmatches(extracted_text,regexec(patterns[t],extracted_text))[[1]]
  where_asterix<-sum(str_count(labelstring,unique(names(graphs))))==1
  print(where_asterix)
  enoughdata<-c(enoughdata,!where_asterix)
}

#extract baseline change data
extracted_baselinechanges<-unlist(stri_extract_all_regex(extracted_text, "\\S+% compared to baseline|Not enough data for this date?!Not enough data for this date: Currently, there is not enough data to provide a complete analysis of this place. Google\n  needs a significant volume of data to generate an aggregated and anonymous view of trends."))
extracted_baselinechanges<-numextract(extracted_baselinechanges)
extracted_baselinechanges
#'finish the lines' by replacing the final date values where there are actual values i.e a line not NAs, with the baseline change values
extracted_df[,ncol(extracted_df)][!is.na(extracted_df[,ncol(extracted_df)])]<-extracted_baselinechanges[!is.na(extracted_df[,ncol(extracted_df)])]

#append metdata and last day (sunday) based on data in report
extracted_df<-cbind(origin_file = pdf.name,
                    locality = rep(extracted_locality, each=6),
                    place_type = names(graphs),
                    enough_data = enoughdata,
                    extracted_df,
                    extracted_baselinechanges)

formatted_dates<-format(as.Date(x_dates, format = "%d.%m.%y"),format = "%Y-%m-%d")
#put an asterix by last date to make clear it's extracted from the baseline change figures, not the graph (not shown on graph)
formatted_dates[43]<-paste(formatted_dates[43],"*", sep = '')
#rename time series columns
colnames(extracted_df)[5:ncol(extracted_df)]<-formatted_dates

#remove row names
rownames(extracted_df)<-NULL

#coerce baseline change columns to numeric
extracted_df[,5:nrow(extracted_df)]<-as.numeric(extracted_df[,5:nrow(extracted_df)])

# PLOT ORIGINAL IMAGE VS EXTRACTED DATA -----------------------------------

point_cols<-c(rep('black',42),'red')

dev.off()
par(mfrow=c(1,2))

for (g in 1:length(graphs)){
  
  im<-graphs[[g]] 
  plot(im, main=names(graphs[g]))
  plot(extracted_df[g,5:ncol(extracted_df)]~x_dates, ylim=c(-100,100), cex=0.75,xlab='Date', ylab='Baseline change (%)', main=names(graphs[g]), pch=21, bg=point_cols)
  lines(extracted_df[g,5:ncol(extracted_df)]~x_dates)
  abline(h=c(-80,-40,0,+40,+80), col=add.alpha(1,0.25))
}

#either need to adjust median/min or axes alignment
