# Data Extractor: Google COVID-19 Community Mobility Reports [REDUNDANT SEE BELOW]

## PLEASE NOTE GOOGLE HAVE NOW PUBLISHED THE DATA FROM THE REPORTS (INCLUDING PAST REPORTS AND LOCAL-LEVEL DATA) IN FULL AS A CSV MAKING THIS CODE REDUNDANT. You can access the data by clicking 'Download Global CSV' at https://www.google.com/covid19/mobility/

Downloads and extracts graph and text data from Google's COVID19 Mobility reports (https://www.google.com/covid19/mobility/). These reports are based on pooled and anonymised "location data gathered via the Google Maps app or one of the firm's other mobile services" (https://www.bbc.co.uk/news/technology-52138076). The same as those little graphs that tell you how busy a shop, restaurant or other place is when you google it. They are released ~weekly and mainly intended to help public health officials make decisions during the pandemic, though are also being used by journalists. 

Currently, the main R Code (GraphData_extractor.R) is able to extract data from the first 6 national-level graphs (for place-types Retail & recreation, Grocery & pharmacy, Parks, Transit stations, Workplaces and Residential) of most of the PDF reports (e.g. 2020-03-29_GB_Mobility_Report_en.pdf). It also extracts the final timepoint in the 6-week series from the summary figure text (big font next to graphs in report).

BaselineChanges_extractor.R will become redundant as I integrate it gradually into GraphData_extractor.R, but I'm leaving it there for now in case anyone just wants to extract the summary data.

DownloadGoogleMobilityReports.R is a quick code to download all the PDF files from Google's site - needs to be run when each batch of reports is published as they are only temporarily available until the next batch is up!

