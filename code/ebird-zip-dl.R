#full checklists by GPS location
#clear the workspace
rm(list=ls())

#Load csv of ZIP codes
setwd("~/Documents/GWWA/ebird-downloader/scripts/")
zips <- read.csv("zips.csv", header=TRUE, stringsAsFactors = FALSE)
head(zips)

#Load csv of ZIP code to FIPS (Federal Information Processing Standard). 
#The included file was downloaded from https://www.huduser.gov/portal/datasets/usps_crosswalk.html.
zip.fips <- read.csv("zip-fips.csv", header=TRUE, stringsAsFactors = FALSE)
head(zip.fips)

#Since we're only interested in the ZIP to FIPS conversion, we can remove the other three columns.
#We also want to rearrange the columns so that ZIP comes first since we're going to merge this with the ZIP codes file.
library(dplyr)
zip.fips <- select(zip.fips, ZIP, FIPS=COUNTY)
head(zip.fips)

#Now we want to look up the FIPS code for each of the ZIP codes in our csv of ZIP codes
#NOTE: this only takes the first matching county for a ZIP code. Some ZIP codes span multiple counties. 
library(plyr)
data <- join(zips, zip.fips, match = "first")
head(data)

#If you want to manually double check your FIPS codes, run this code instead and then find duplicates and edit manually.
library(dplyr)
data.dplyr <- zips %>% left_join(zip.fips)

#Hopefully I'll work this issue out later; if you have GPS coordinates use that script instead

#Rural urban continuum codes were downloaded from https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html

library(readxl)
rural.urban <- read_excel("Rural.Urban.Continuum.Codes.1974.1983.1993.2003.2013.xls")

#Merge continuum codes with data file
data <- data %>% left_join(rural.urban)
head(data)
#Download the eBird subnational codes from http://help.ebird.org/customer/portal/articles/973915-uploading-data-to-ebird

ebird.codes <- read.csv("ebird_api_ref_location_eBird_list_subnational2.csv")
head(ebird.codes)

#Because our data file has the word County included, we need to add that to the eBird data to merge them by County Name
head(ebird.codes)
ebird.codes$SUBNATIONAL2_NAME <- paste(ebird.codes$SUBNATIONAL1_CODE,": ",ebird.codes$SUBNATIONAL2_NAME," County", sep="")
head(ebird.codes)
ebird.codes <- ebird.codes %>% select(Country=COUNTRY_CODE, CtState=SUBNATIONAL1_CODE, CtStCo=SUBNATIONAL2_CODE, County=SUBNATIONAL2_NAME)
head(ebird.codes)

library(reshape2)
data$County <- colsplit(data$County, " \\(" , c("County", "FIPS2"))
data$County <- paste("US-", data$County$County, sep="")
head(data)
data <- data %>% select(ZIP, FIPS, State, County, 
                        RU74 = `Rural-Urban Continuum Code 1974`, 
                        RU83=`Rural-Urban Continuum Code 1983`, 
                        RU93 = `Rural-Urban Continuum Code 1993`, 
                        RU03 = `Rural-Urban Continuum Code 2003`, 
                        RU13 = `Rural-Urban Continuum Code 2013`)

data <- data %>% left_join(ebird.codes)
head(data)
str(data)
data$CtState[1]

#set up the components of the URLs needed for downloading

download <- c("download_file")
ebird.org <- c("http://ebird.org/ebird/linegraph?bmo=")
SPECIES <- c("y00665,x00669,brewar,lawwar,gowwar") #change species here; you can only download 5 at a time
COUNTIES <- data$CtStCo
years <- c(1985:2016) #change this if you need different years
bMonth <- c(4) #change beginning month (January =1, February =2, etc...)
eMonth <- c(7) #change end month
STATE <- data$CtState
FILE <- c ()
for (i in 1:length(years)) {FILE <- append(FILE, paste(data$County,years[i],sep="-"))}
script <- c()

#this for loop is going to create a unique URL for each county for each year
for (i in 1:length(years)) {script <- 
  append(script, paste(download, "\ (", "\" ",ebird.org,
                       bMonth,
                       "&emo=",
                       eMonth,
                       "&byr=",
                       years[i],
                       "&eyr=",
                       years[i],
                       "&r=",
                       COUNTIES,
                       "&spp=",
                       SPECIES,
                       "&fmt=tsv",
                       "\"",
                       ",",
                       "\"", (paste(data$County, years[i], sep="-")), "\"","\ )" , sep=""))}

#write the script to download the data using python
write(script,file="dlscript.py",ncolumns=1)

#########IMPORTANT STEP TO BE DONE MANUALLY
#I was too lazy to concatenate all the code needed at the beginning of the python script, so you'll need to manually paste the text from login
#paste the login text at the beginning of the dlscript.py file
#make sure to put in your username and password for eBird

#change your working directory so that all the downloads go into a folder
setwd("~/Documents/GWWA/ebird-downloader/downloads/")

#call the script and run it in with python
#depending on how many years and how many counties you're downloading, this could take a while so be patient



##############################system('python ~/Documents/GWWA/ebird-downloader/scripts/dlscript.py')


#creates a value of the mean of the frequency for each county named for the county and range of dates
mean.freq <- matrix(ncol=2)
mean.freq
mean.freq <- as.data.frame(mean.freq)
colnames(mean.freq) <- c("V1", "mean")

for (i in 1:length(FILE)) {
x <- read.delim(FILE[i], header=TRUE)
assign(FILE[i], x)
freq <- colSums(x[2:6,3:18])
mean <- mean(freq)
newrow <- (cbind(FILE[i], mean))
mean.freq <- rbind(mean.freq, newrow)}

mean.freq <- mean.freq[-1,]
head(mean.freq)
str(mean.freq)
library(stringr)

#split by characters and create new variables from V1
split1 <- str_split_fixed(mean.freq$V1, "-",2)
country <- split1[,1]
split2 <- str_split_fixed(split1[,2], ":", 2)
state <- split2[,1]
split3 <- str_split_fixed(split2[,2], "-", 2)
year <- split3[,2]
head(split3)
split4 <- str_split_fixed(split3[,1], " ", 2)
county <- split4[,2]

#check that everything looks right
head(country)
head(state)
head(year)
head(county)

#transpose row vectors to column vectors
t(country)
t(state)
t(year)

#combine the split vectors with the mean frequency data frame
mean.freq <- cbind(mean.freq, country, state, year, county)
head(mean.freq)

#load in the coded articles
setwd("~/Documents/GWWA")
articles <- read.csv("articles.csv", header=TRUE)
articles <- articles %>% distinct()

#make an easy column to look up
mean.freq <- mutate(mean.freq, lookup=paste(state,year,county,sep=""))
articles <- mutate(articles, lookup=paste(State, Year, County, sep=""))
articles <- articles %>% semi_join(mean.freq, by="lookup")


articles <- articles %>% 
  select(id=No., coder=Coder.ID, pub=Publication, city=City, state=State, ZIP, country=Country, 
         county=County, RU=Rural.Urban.Code, pop=Total.county.population, cty.size=County.size, 
         dens=Density,freq=Propotion.of.eBird.checklists.with.GWWA, byline=Byline, date=Date, 
         day=Day, mnth=Month, yr=Year, headline=Headline, len=Length, TA=Threat.or.Action, 
         S1=First.source, S2=Second.source, S3=Third.source)