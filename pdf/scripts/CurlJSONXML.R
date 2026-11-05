# Acquiring Text Data Workshop part 1 
# 
# Author: Solomon Messing
###############################################################################

install.packages(c('RCurl', 'RJSONIO', 'XML', 'stringr', 'tm', 'RWeka', 
        "RTextTools", "mixtools", "topicmodels", "Snowball", "slam", "MASS", 
        "proxy", "cluster", "psych", "car", "foreign", "rJava") )

#####################################################
####### Simple example using Google's API:    #######
#####################################################

library('RCurl')
library('RJSONIO') 
library('XML') 

# put your name in quotes:
myQueries <- c("<yourname>", "<anothername>")

search.url <- "http://ajax.googleapis.com/ajax/services/search/web?v=1.0"

# applies URLencode to every element in myQueries
response <- list()
for( i in 1:length(myQueries)){
	request.url <- paste(search.url, "&q=", URLencode(myQueries[i]), sep="")
	response[[i]] <- fromJSON(getURL(request.url))
}

# That's a lot of crap we just returned:
response

# The best way to figure out what you want is to use 
# Eclipse + StatET, which has an excellent object viewer.
# It can be a pain to set up, however.

# Another great way to look at JSON with expandable and collapseable
# objects and arrays, is via Chrome's JSON viewer:
# https://chrome.google.com/webstore/detail/chklaanhfefbnpoihckbnefhakgolnmc

# You can visit the url paste(search.url, "&q=", URLencode(myQueries[i]), sep="")
# directly in your browser or save the output to a JSON file and browse it
# locally.  The name of each array in the original JSON corresponds to
# the same thing in R.

# The object reutned by fromJSON() is a list of lists.  You can access
# elements in the list hierarchy via element number, e.g.:
response[[1]][[1]][[1]][[1]][[1]]

# Or by calling each element by name, except if the elements are not
# named, as in our exmaple below:
response[[1]]$responseData$cursor$estimatedResultCount

# Take a look at the number of pages:
response[[1]]$responseData$cursor$estimatedResultCount
response[[1]]$responseData$results[[1]]["content"]

# you can use sapply and lapply to get stuff out of the list by applying 
# a function to each list element:
sapply(response, function(x) x$responseData$results[[1]]  )

# you can nest sapply:
sapply(response, function(x) sapply(x$responseData$results, function(x) x["url"])

# Or loop over everything:
for( i in 1:length(response)){
    this <- response[[i]]$responseData$results
    for( j in 1:length(this)){
        print( this[[j]]["url"] )
    } 
}

# (a) Write code to return the number of hits for each search query

# (b) Write code to return the content for each result (use sapply)

##########################################
##### Pull Twitter data from Otter  ######
##########################################

# Look at API documentation at http://code.google.com/p/otterapi/. 
# Register for an API at: http://manage.topsy.com/app
library('RCurl')
library('RJSONIO')
library('XML')

queries <- read.csv("http://www.stanford.edu/~messing/accounts.csv")[,2]
# Because each account has an @ in front, topsy knows we are search for
# tweets mentioning these accounts.

# Problem: JSON output from topsy is paginated by 100 tweets.  
# How can we deal with pagination?  Below, we first find the 
# number of pages, then iterate through each.

title <- list()
date <- list()
link <- list()
guid <- list()
creator <- list()
content <- list()
cand <- list()
twdat <- list()

# k will index each page in the entire data set.
k <- 1

# start by getting the number of tweets for each query (account):
for( i in 1:length(queries)){
	
	# Look at the JSON output for the first account by running:
	# i = 1
	thisurl <- paste("http://otter.topsy.com/searchcount.json?q=", queries[i], 
			sep="")
	print(thisurl)
	
	# then paste thisurl into your browser
	
	# total number of tweets for this account:
	total <- fromJSON(getURL(thisurl))$response[["a"]]
	
	# we want to divide by 100 and round up to get the number of 
	# passes we'll need to make for each representative
	itr <- ceiling(total/ 100)

	# keep track of things:
	print(paste("account: ",queries[i], "# ", i, "; "))

	# now iterate over each page for this account:
	for( j in 1:itr){
		thisurl <- paste("http://otter.topsy.com/search.json?",
				"q=", queries[i],
				paste("&page=", j, sep=""),
				"&perpage=100", 
				sep="")
		urlcontents <- getURL(thisurl)
		jsstuff <- fromJSON(urlcontents) 
		
		# use sapply to get the data we need:
		twdat$authnick[[k]] <- sapply(jsstuff$response$list, function(x) x$trackback_author_nick )
		twdat$totrts[[k]] <- sapply(jsstuff$response$list, function(x) x$hits )
		twdat$thisdate[[k]] <- sapply(jsstuff$response$list, function(x) x$trackback_date  )
		twdat$origdate[[k]] <- sapply(jsstuff$response$list, function(x) x$firstpost_date  )
		twdat$title[[k]] <- sapply(jsstuff$response$list, function(x) x$title  )
		twdat$content[[k]] <- sapply(jsstuff$response$list, function(x) x$content  )
		twdat$link[[k]] <- sapply(jsstuff$response$list, function(x) x$trackback_permalink  )
		twdat$origlink[[k]] <- sapply(jsstuff$response$list, function(x) x$url  )
		twdat$accountTo[[k]] <- rep( queries[i], length(twdat$link[[k]]) )
		k <- k+1
#		Sys.sleep(.5) 
		closeAllConnections()
	print(paste("page ", j, " of ", itr))
	}
}

# twdat$link for example, is now a list of lists of characters.
# But we don't need it to be a list of lists, so apply unlist
# to each element:
twd <- lapply(twdat, unlist)

# get posix dates for each:
twd$thisdateposx <- as.POSIXlt(twd$thisdate, origin="1970-01-01")
twd$origdateposx <- as.POSIXlt(twd$origdate, origin="1970-01-01")

# now we can make it into a data frame:
twdata <- as.data.frame(twd)


# (a) Modify the code to above so that you are recording the tweets' authors (easy)

# (b) Get code above to loop over another series of twitter queries relevant
# to your research.

# (c) Modify code above to handle errors appropriately. 


###############################################################################
################# Yahoo full probablistic entity extraction ###################
###############################################################################

library('RCurl')
library('RJSONIO')
baseurl <- "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20contentanalysis.analyze%20where%20text%3D'"
text <- "American artists generally eschew the work of Italian painters and French chefs.  They like NASA no more than BP."
urltext <- URLencode(text)  
endurl <- "'%3B&diagnostics=true"
url <- paste(baseurl, urltext, endurl, sep="")
entitydat <- fromJSON(getURL(url))

# entity:
sapply(entitydat$query$results$entities$entity, function(x) x$text["content"] )

# type:
entitydat$query$results$entities$entity[[2]]$types$type[[1]]

# (a) Use this to extract entities mentioned in the example above.

##################################################################
#################### Hit NYTimes API #############################
##################################################################

# Sign up for an API Key here: 
"http://developer.nytimes.com/apps/register"

# Read up on the API documentation.

# (a) Scrape data from NYT
requrl <- "http://api.nytimes.com/svc/politics/v3/us/legislative/congress/112/house/members.json?"
apikey <- "api-key=<your api key here>" 
urlcontents <- getURL(paste(requrl, apikey, sep=""))
urlJSONparse <- fromJSON(urlcontents)

# the good stuff is here:
urlJSONparse$results[[1]]$members

# Get rid of "next_election" var and fix middle_name var.
# This was really hard to figure out.  See if you can figure
# out what's going on in the code below:
for(i in 1:length(urlJSONparse$results[[1]]$members)){
    try(urlJSONparse$results[[1]]$members[[i]]$next_election <- NULL, silent=T)
    if(is.null(urlJSONparse$results[[1]]$members[[i]]$middle_name)){
        urlJSONparse$results[[1]]$members[[i]]$middle_name <- NA
    }
}

### (b) Now bind it all in a data frame (hard)
### you want to use lapply( to apply data frame to every element in
### urlJSONparse$results[[1]]$members, then bind everything together
### using rbind. 

### Hack/Google around for 10 minutes then ask for help with this one... 

### (c) now you get the senate data.

# and comibine them:
dat <- rbind(sendat[c("bioid","PARTY","STATE","DIST", "LNAME", "FNAME", "TITLE" )], 
        congdat[c("bioid","PARTY","STATE","DIST", "LNAME", "FNAME", "TITLE"  )] )

# Full party names
dat$P_FULL <- c("Democrat", "Republican", "Independent")[dat$PARTY]

# (d) go to nytimes.com documentation and figure out how to srape newspaper 
# article summaries

##########################################
######## scrape politifact data: #########
##########################################

# From here:
# https://github.com/rtelmore/Politifact/blob/master/src/load.R
# https://github.com/rtelmore/Politifact/blob/master/src/do.R

library('plyr')

names <- c("tim-pawlenty", "michele-bachmann", "newt-gingrich", 
		"rick-perry", "mitt-romney", "ron-paul")

base.url <- "http://www.politifact.com/personalities/"

df <- data.frame(count=NULL, candidate=NULL, category=NULL)

for(name in names){
	full.url <- paste(base.url, name, "/", sep="")
	doc <- htmlTreeParse(full.url, useInternalNodes=T)
	nset.stats <- getNodeSet(doc, 
			"//ul[@class='chartlist']//span[@class='count']") 
	table.stats <- ldply(nset.stats, function(x) xmlSApply(x, xmlValue))
	table.stats$candidate <- rep(name, length(table.stats$text))
	nset.cats <- getNodeSet(doc, "//ul[@class='chartlist']//a") 
	tmp <- ldply(nset.cats, function(x) xmlSApply(x, xmlValue))
	table.stats$category <- tmp$text
	names(table.stats) <- c("count", "candidate", "category")
	df <- rbind(df, table.stats)
}

# FIXME: Use regex (str_replace_all()) or gsub to fix this: 
table.stats$count <- as.numeric(table.stats$count)

p <- ggplot(data=df, aes(x=category, y=as.numeric(count)))
p + geom_bar() + 
		facet_wrap(~ candidate) +
		scale_y_continuous("number of statements") +
		opts(axis.text.x=theme_text(angle=-90))

p <- ggplot(data=df, aes(x=candidate, y=as.numeric(count)))
#p + geom_bar(position="dodge")
p + geom_bar() + 
		facet_wrap(~ category) +
		scale_y_continuous("number of statements") +
		opts(axis.text.x=theme_text(angle=-90))

## Round Two  6 Sept 2012
names <- c("mitt-romney", "paul-ryan", "joe-biden", "barack-obama")

base.url <- "http://www.politifact.com/personalities/"

df <- data.frame(count=NULL, candidate=NULL, category=NULL)

for(name in names){
	full.url <- paste(base.url, name, "/", sep="")
	doc <- htmlTreeParse(full.url, useInternalNodes=T)
	nset.stats <- getNodeSet(doc, 
			"//ul[@class='chartlist']//span[@class='count']") 
	table.stats <- ldply(nset.stats, function(x) xmlSApply(x, xmlValue))
	table.stats$candidate <- rep(name, length(table.stats$text))
	nset.cats <- getNodeSet(doc, "//ul[@class='chartlist']//a") 
	tmp <- ldply(nset.cats, function(x) xmlSApply(x, xmlValue))
	table.stats$category <- tmp$text
	names(table.stats) <- c("count", "candidate", "category")
	df <- rbind(df, table.stats)
}

# FIXME: Use regex (str_replace_all()) or gsub to fix this: 
table.stats$count <- as.numeric(table.stats$count)

p <- ggplot(data=df, aes(x=category, y=as.numeric(count)))
p + geom_bar() + 
		facet_wrap(~ candidate) +
		scale_y_continuous("number of statements") +
		opts(axis.text.x=theme_text(angle=-90))


##########################################
######## scrape baseball stats:  #########
##########################################

# from: http://www.slideshare.net/rtelmore/user-2012-talk

## Dependencies:
library('XML')
library('ggplot2')

base.url <- "http://www.baseball-reference.com/boxes"
teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
		"FLA", "HOU", "KCR", "ANA", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK",
		"PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBD", "TEX", "TOR", "WSN")
years <- 2005:2009
months <- 4:10
days <- 1:31
urls <- c()
for (team in teams){
	for (year in years){
		out.string <- paste(Sys.time(), "--", team, year, sep = " ")
		print(out.string)
		for (month in months){
			for (day in days){
				ifelse(length(as.character(month) == 1), 
						date.url <- paste(team, year, 0, month, day, sep=""),
						date.url <- paste(team, year, month, day, sep="")
				)
				for (i in 0:1){
					full.url <- paste(paste(base.url, team, date.url, sep="/"), i, 
							".shtml", sep="")
					try({
								table.stats <- readHTMLTable(full.url)
								pbyp.stats <- table.stats[["play_by_play"]]
								stats.df <- pbyp.stats[!is.na(as.numeric(pbyp.stats$Out)), 
										c("Inn", "Out", "Pit(cnt)Sequence", 
												"Pitcher")]
								stats.df$Pitches <- unlist(lapply(strsplit(
														stats.df$"Pit(cnt)Sequence", ","), 
												function(x) as.numeric(x[1])))
								stats.df$Inning <- unlist(lapply(strsplit(stats.df$Inn, "[a-z]"), 
												function(x) as.numeric(x[2])))
								stats.agg <- aggregate(stats.df[, "Pitches"], 
										by=list(stats.df$Inning), FUN="sum")
								if(sum(stats.agg$x==6) != 0){
									stats.agg.2 <- aggregate(stats.df[, "Pitches"], 
											by=list(stats.df$Inning), FUN="length")
									if(stats.agg.2$x[stats.agg$x==6]==6){
										urls <- c(urls, c(full.url))
									}                            
								}
								
							}, TRUE)
				}
			}
		}
	}  
}

write.table(urls, "../data/outfile.csv", row.names=F, col.names=F)
final.string <- paste(Sys.time(), " -- Finished :)", sep = "")
print(final.string)











