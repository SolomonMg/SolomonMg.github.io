# Acquiring Text Data Workshop part 2
# 
# Author: Solomon Messing
###############################################################################

##################################################################
############# Regular Expressions to Clean Data ##################
##################################################################

library('RCurl')
library('RJSONIO')
library('XML')

## Data cleaning example:

## Pull in some semi-structured data from a wikipedia table:
theurl <- "http://en.wikipedia.org/wiki/List_of_countries_by_population"
tables <- readHTMLTable(theurl)

# find legit table:
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
which(n.rows>20)

# here's one:
tables[[2]]

## Whoa, that data is messy, use some regex and string processing to 
## clean it up.  

# save it to it's own data frame:
scraped <- tables[[2]]

# take a look at each var seperately:
names(scraped)

# that won't do:
names(scraped) <- c("rank", "country", "pop", "date", "pcntWorldPop", "src")
scraped$rank
scraped$country
scraped$pop
scraped$date
scraped$pcntWorldPop
scraped$src

# Clean with regex:
library('stringr')

scraped$pop <- str_replace_all(scraped$pop, ",", "")
scraped$pop <- as.numeric(scraped$pop)
scraped$pop
# NICE

### (a) Now you clean scraped$country


### (b) And scraped$pcntWorldPop

##################################################################
############ Features from in text with RegEx ####################
##################################################################

# The goal of this part is to write a regular expression (in R) for identifying an entity 
# and then the words around that entity.  An entity can be some political actor ("Barack Obama") 
# or an institution ("Congress", "World Bank").  The posted R code provides many of RegEx 
# functions in R that will be useful for accomplishing the following tasks.  

# Load some of the twitter scraping data:
load(file("http://dl.dropbox.com/u/25710348/CSSscraping/data/twdata.Rda"))

twdata$content <- as.character(twdata$content)

# example features we might want: 
twdata$containNum <- FALSE 
twdata$containNum[grep( "\\d", twdata$content)] <- TRUE        
twdata$containWords <- FALSE
twdata$containWords[grep( "\\w+", twdata$content)] <- 
twdata$mentionObama <- FALSE
twdata$mentionObama[agrep( "[Oo]bama", twdata$content, .2)] <- TRUE 

# Simple Regex's are pretty fast.  Why might it slooow things down 
# with more complicated Regex? 

# a)  Write a regular expression to capture all hashtags in these tweets.  

# b)  Write a regular expression to identify all occurrences of the entity in these tweets.  

# c) Write a regular expression to identify the word that occurs immediately before and 
# after any mention of your entity.
#
# Hint: \\b will return a word boundry

# d) Write a regular expression to return all sentences in which your entity occurs.

################################################
##### Simple scrape w/ regex and cURL:  #######$
################################################

# Suppose we want to know how many hits some series of items has in google news in
# a given period.  

# We navigate to a custom search in their news search site and eventually find
# that the url associated with the query "Obama" from to Jan 1, 2008ÐSep 11, 2008
# looks like this:
# http://www.google.com/search?tbm=nws&hl=en&gl=us&as_q=Obama&as_epq=&as_oq=&as_eq=&as_scoring=r&btnG=Search&as_qdr=a&as_drrb=b&as_mindate=1%2F1%2F08&as_maxdate=9%2F11%2F08&as_nsrc=&as_nloc=&as_author=&as_occt=any&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F08%2Ccd_max%3A9%2F11%2F08
# the as_q= is what comes before the query.  Excellent! 

# break out the url into pieces:
base <- "http://www.google.com/search"
querystring1 <- "?tbm=nws&hl=en&gl=us&as_q="
gnewsquery <- "Rick+perry"
querystring2 <- "&as_epq=&as_oq=&as_eq=&as_scoring=r&btnG=Search&as_qdr=a&as_drrb=b&as_mindate=1%2F1%2F08&as_maxdate=11%2F04%2F08&as_nsrc=&as_nloc=&as_author=&as_occt=any&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F08%2Ccd_max%3A11%2F04%2F08"

thisurl <- paste(base, querystring1, gnewsquery, querystring2, sep="")

# hit the server with readLines:
res <- readLines(thisurl)

# use RegEx to extract key info:
pattrn <- "About ((\\d,?))+ results"
( results <- na.omit(str_extract(res, pattrn))[1] )

# Extract number and clean it up so we can save it as numeric in the data: 
numresults <- str_extract(results, "((\\d,?))+ ")
numresults <- gsub(",", "", numresults)
numresults <- gsub(" ", "", numresults)
as.numeric(numresults)
# SWEET.


###############################################################################
################## If time: Geolocating IP Addresses ########################
###############################################################################


ips <- c("64.90.182.55", "96.47.67.105", "206.246.122.250", "129.6.15.28", "64.236.96.53", 
        "216.119.63.113", "64.250.177.145", "208.66.175.36", "173.14.55.9", "64.113.32.5", 
        "66.219.116.140", "24.56.178.140", "132.163.4.101", "132.163.4.102", "132.163.4.103",
        "192.43.244.18", "128.138.140.44", "128.138.188.172", "198.60.73.8", "64.250.229.100",
        "131.107.13.100", "207.200.81.113", "69.25.96.13", "216.171.124.36", "64.147.116.229")

# create variables to store output:
state <- character(length(ips))
lat <- character(length(ips))
lon <- character(length(ips))

# iterate over ip addresses:
for(i in 18:length(ips)){
    #i=5	
    print(paste("i = ", i, " ip: ", i))
    
    ipinfo <- getURL(paste("http://www.ip-db.com/", ips[i], sep=""))
    
    # use RegEx to extract key info:
    library(stringr)
    
    # Get state
    # We use the regex (\\w\\s?)+ where \\w = word character, \\s? = possible space character, 
    # () groups the pattern, and + means one or more.  
    # note that we must escape " characters by \" to include them in the pattern
    pattrn <- "Region:</b></font></td><td width=\"10\">&nbsp;</td><td><font face=\"arial\" size=\"2\">(\\w\\s?)+</td>"
    ( results <- str_extract(ipinfo, pattrn))
    
    # Extract the actual text of the state and clean it up so we can actually use it: 
    state[i] <- str_extract(results, ">(\\w\\s?)+<")
    state[i] <- gsub(">", "", state[i])
    state[i] <- gsub("<", "", state[i])
    
    print(state[i])
    
    # Now get lattitude and longitude:
    # We can limit our pattern to target="_blank" because it's a unique pattern in the html
    # An example of our substring of interest follows: "target=\"_blank\">40.7089 -74.0012</a></td>"
    # So we use this pattern \\-?\\d\\d(\\.\\d+)? \\-?\\d\\d(\\.\\d+)? where \\d means digit,
    # means optional, () groups the pattern, and \\- is an escaped dash
    
    pattrn <- "target=\"_blank\">\\-?\\d+(\\.\\d+)? \\-?\\d+(\\.\\d+)?</a></td>"
    ( results <- str_extract(ipinfo, pattrn))
    
    # Extract both lattitude an longitude and clean it up so we can save it as numeric in the data: 
    latlon <- str_extract_all(results, "\\-?\\d+(\\.\\d+)?")
    
    # the code above returns a list in case results has more than one set of characters in the character
    # vector, so we need to unlist it to make it into a normal vector:
    latlon <- unlist(latlon)
    
    # since there are two results, we return one for each for lat and lon:
    lat[i] <- latlon[1] 
    lon[i] <- latlon[2] 
    print(lat[i])
    print(lon[i])
    
    # Put a delay between entries so there's no danger of overwhelming the server
    Sys.sleep(sample(15:25,1)/10) 
}

###############################################################################
####################### draw maps based on ip addresses #######################
###############################################################################

# We'll draw points for each observation of lattitude and longitude, and
# we'll shade in each state according to how many cases we have in each state

# first count the number of cases in each state:
mtstates <- table(state)

# then match to the state.name object (which the maps object also uses)
mtfullstatenames <- tolower(state.name[match(names(mtstates), state.name)])

# extract the state names to a separate object:
names(mtstates) <- mtfullstatenames

# use "cut" to group the count data into several groups
mtstatesb <- cut(mtstates, breaks= c(-Inf, 0, 1, 2, 4, Inf), labels=c("0", "1", "2", "3-4", "4+")  )  

library(maps)
# generate a map object so that we can match names to the state level shape files
usmap <- map("state")


# Some of the state level shape files are broken down by sub-state region.  We
# just care about the political boundaries for the purposes of this project, 
# so we will not differentiate.  Thus, we need to make all names within a 
# state the same, which will mean some names are repeated.  
# Luckily the usmap$names are as follows: state:subregion
usmap$names

# so we can just split the strings based on the ":" character.  We'll use
# sapply( ..., "[[", 1) which extracts the first element when a list is returned:
mapstates <- sapply(strsplit(usmap$names, ":"), "[[", 1)
mapstates

# Now we need to generate a color for each item in mapstates so we can color in the map. 
# We first use "match()" to index our count of observations per state in mtstatesb by mapstates.  
# shapeFileN will contain the numeric range specified in mstatesb, but indexed by the 
# the names in mapstates, some of which will of course be repeated as discussed above. 

shapeFileN <- mtstatesb[match(mapstates,names(mtstates))]

# Now we've can generate a series of grey colors indexed by the count of observations per state,
# indexed so that the the observations match the shape files that come with the "state" object in
# the maps package:
cols <- rev(grey.colors(5))[shapeFileN]

# now let's draw the map:
png("demosamplemap.png", width=1000, height=700)

# draw the map object using the colors specified above
map("state", col= cols, fill=T)

# draw lattitude and longitude points for each observation:
points(lon, lat, cex = .3, col = rgb(0,0,0,.5))

# draw a legend
legend("bottomright", levels(mtstatesb), fill = c("white", rev(grey.colors(5))[2:5]), bty="n")

# put a title on it:
title("Geographic distribution of NIST Internet Time Servers")
dev.off()














