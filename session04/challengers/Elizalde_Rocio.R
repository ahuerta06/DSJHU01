# Challenge Session 04 - August 31th, 2016

# 1. Download the American Community Survey data about United States communities and load it into an R object.
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",temp)
myData <- data.table(read.csv(temp, header=TRUE, sep=","))
unlink(temp)

#2. Describe the data table (dimensions, column names, data types) 
str(myData)
#3. Use the `sqldf` package to get the different states where there are houses with 4 or more bedrooms.
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk
library(RMySQL)
library(sqldf)
query_results <- sqldf("select distinct st from 'myData' where BDS >= 4")
#4. With the same data obtain the households on greater than 10 acres who sold more than $10,000 worth of agriculture products.
sqldf("select * from 'myData' where ACR == 3 & AGS == 6")

#5. Load the Gross Domestic Product data for the 190 ranked countries in this data set:
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",temp)
GDP_Data <- read.csv(temp, header=TRUE, sep=",")
unlink(temp)

GDP_Data <- GDP_Data[5:194,c(-3,-(7:10))]
names(GDP_Data) <- c("CountryCode","Ranking","Economy","GDP","Comentarios")

#6. Load the educational data from this data set: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",temp)
EducData <- read.csv(temp, header=TRUE, sep=",")
unlink(temp)

#7. Match the data based on the country shortcode.
MergedData <- merge(x = EducData, y = GDP_Data, by = "CountryCode", all=FALSE)
MergedData$Ranking <- as.numeric(as.character(MergedData$Ranking))

#8. Describe the data table (dimensions, column names, data types) 
str(MergedData)

#9. What is the average GDP ranking for the "Lower middle income" and "Upper middle income" group?
by_income_grp <- MergedData %>% 
  group_by(Income.Group) %>%
  summarize(mean(Ranking)) %>%
  print

#10. Using `dyplr` package reorder the rows in the dataframe by rank
arrange_by_rank <- arrange(MergedData, Ranking)
#Bonus: how would you do it in a descending order?
arrange_by_rank_desc <- arrange(MergedData, desc(Ranking))
#11. Using `dplyr` get the average, max, min and median total economy by lending category
MergedData$GDP <- as.numeric(as.character(MergedData$GDP))
by_lending_cat <- MergedData %>% 
  group_by(Lending.category) %>%
  summarize(mean(GDP, na.rm =TRUE),max(GDP, na.rm =TRUE),min(GDP, na.rm =TRUE),median(GDP, na.rm =TRUE)) %>%
  print
#12. Add these values by government accounting concept.
by_acc_concept <- MergedData %>% 
  group_by(Government.Accounting.concept) %>%
  summarize(sum(GDP, na.rm =TRUE)) %>%
  print

#__Bonus tasks:__
#13. Read the HTML link http://biostat.jhsph.edu/~jleek/contact.html and load it into an R variable _(1 point)_
#14. Load the dataset from the following link into an R variable and describe it https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for _(1 point)_
#Q4
library(XML)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
html <- htmlTreeParse(url, useInternalNodes = T)
xpathSApply(html,"//title",xmlValue)

con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
htmlCode
nchar(htmlCode[c(10,20,30,100)])

#Q5
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
#lines <- readLines(fileURL, n=10)

w <- c(1, 9, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4)
colNames <- c("filler", "week", "filler", "sstNino12", "sstaNino12", "filler", "sstNino3", "sstaNino3", "filler", "sstNino34", "sstaNino34", "filler", "sstNino4", "sstaNino4")
d <- read.fwf(fileURL, w, header=FALSE, skip=4, col.names=colNames)
d <- d[, grep("^[^filler]", names(d))]
sum(d[, 4])


