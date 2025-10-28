# Acquiring Text Data Workshop part 3 
# 
# Author: Solomon Messing
###############################################################################

# If you decide to use more data, ths script should probably be run on a unix 
# cluster. 

#########################################################
# To run stuff below on Stanford unix servers w/ nohup:
########################################################
# This will allow you to keep R running if/when 
# you log out. Change cornXX to something else (01-25).

# ssh <your SUNET account (email prefix)>@cornXX.stanford.edu
# kinit;aklog
# pagsh
# kinit;aklog
# screen
# keeptoken
# <run command suggested by keeptoken>
# nohup R 
# CTRL-a, d

# to check: 
# ssh <your SUNET account (email prefix)>@cornXX.stanford.edu
# screen -rdA

#########################
###### CLEAN DATA #######
#########################

# Get some data:
load(file("http://dl.dropbox.com/u/25710348/CSSscraping/data/twdata.Rda"))

twdata$content <- as.character(twdata$content)

# De-duplicate:
twdata <- twdata[!duplicated(twdata$link),] 

# First, detect and extract all (tags)
samptext = tolower(twdata$content)

# Get each tweet with a tag
tagexp = "<INSERT TAG REG EXPRESSION HERE>"

# (a) use regular expressions to extract tags from text and save them in a
# vector for use later when we are modeling the text

library('stringr')
# use str_extract() and str_replace_all()

#### FEATURE EXTRACTION #####
# How might we produce an indicator variable showing whether the 
# text contains the phrase "Obama is a Muslim"?
OM <- grep("Obama is a Muslim", twdata$content) 

# Notice how many more results we get with approximate matching!
OMa <- agrep("Obama is a Muslim", twdata$content) 

#### MASHUP ###
## (b) Remember when we hit the nytimes site to get data on representatives? 
## now let's use that data to augment our tweets.  Note that this was merged
## with each representative's twitter account, which I got via mecahnical turk.

load(file("http://www.stanford.edu/~messing/mtreps.Rda"))

# Match is a lovely way to do this.  Play with the code below just running
# the stuff inside match() to get a sense of what's happening:

twdata$accountTo <- tolower(twdata$accountTo)  
mtreps$twitteracc <- tolower(mtreps$twitteracc) 

twdata$bioid <- mtreps$bioid[match(twdata$accountTo, mtreps$twitteracc)] 
twdata$PARTY <- mtreps$PARTY[match(twdata$accountTo, mtreps$twitteracc)] 


# (c) Replicate above for relevant variables, like party, state, district, 
# name, title 

## Use this ugly but useful function to clean up the text:
# Function to strip useless characters (forked from Graham Horwood)
tidyChar <- function(v,lang="en",include="punct",exclude="") {
    expr = data.frame(matrix(
                    c("[\u202b-\u202f]|[\u205f-\u2064]","", # invis
                            "\\.{3,}","\u2026", # ellipse
                            "(\n|\r)+"," ", # cr
                            "[\u200e-\u200f]|[\u202a-\u202e]","", # rllr
                            "_+"," ", #underscore
                            "[\u2000-\u200d]"," " # space
                    ),
                    ncol=2,byrow=T,
                    dimnames=list(
                            c("invis","ellipse", "cr","rllr","underscore","space"),
                            c("ptn","rep")
                    )
            )
    )
    for (e in rownames(expr)[which(!rownames(expr) %in% include)]) {
        v = gsub(expr[e,"ptn"],expr[e,"rep"],v,perl=T)
    }
    
    v = gsub("'", "", v)
    
    # US, U.S., u.s. --> us
    v = gsub("US", "usa", v)
    v = gsub("U\\.S\\.", "usa", v)	
    v = gsub("u\\.s\\.", "usa", v)
    
    # removes multiple spaces
    v = gsub(" +"," ",v)
    
    # removes punctuation using regex (not perl, which doesn't seem to work so well).
    v = gsub("[[:punct:]]{1,9}", " ", v)
    
    # replaces /, |, [, and ] charcters with a space:
    v = gsub("[\\/|\\|\\[|\\]]{1,9}", " ", v)
    v = gsub("[\\|]{1,9}", " ", v)
    
    return(v)
}
twdata$content <- tidyChar(twdata$content) 

# Quick LOOK
# What is the distribution of tweets to legislators? 
tblAcctTo <- table(twdata$accountTo)
hist(tblAcctTo)

# what is the distribution of tweets to parties
tblParty <- table(twdata$PARTY)
round( prop.table(tblParty), 2) 

# (d) How does the distribution look by state? 

#########################
##### LOAD INTO DTM #####
#########################

# Create corpus
#install.packages('Snowball')
#install.packages('tm')
#install.packages('rJava')
#install.packages('openNLP', type='source')
#install.packages('openNLPmodels.en')
#library('Snowball')
library('rJava')
.jinit(parameters="-Xmx4g") # give java a lot of memory (4G) 

library('tm')
library('openNLP')
library('openNLPmodels.en') # needed for english POS

# remove stopwords:
twdata$content <- removeWords(twdata$content, stopwords()) 

# POS tagging using openNLP
twdata$contentPOS <- tagPOS(twdata$content)

head(twdata$contentPOS)

corpPOS <- Corpus(VectorSource( twdata$contentPOS ))
corp <- Corpus(VectorSource( twdata$content ))

dtm <-  DocumentTermMatrix(corp, 
        control = list(stripWhitespace=TRUE,
                stopwords = TRUE,
                removePunctuation=TRUE #,
        # stemming = TRUE, 
        # tokenize=NGramTokenizer gives trigrams (no arguement = unigram)
        )
)

dtmPOS <-  DocumentTermMatrix(corpPOS, 
        control = list( stripWhitespace=TRUE,
                stopwords = TRUE,
                removePunctuation=TRUE,
                tolower=FALSE #, # don't change punctuation
        # stemming = TRUE, 
        #tokenize=NGramTokenizer gives trigrams (no arguement = unigram)
        )
)

# Consider saving these objects if you scale up your data:
#save(dtm, file="data/dtm.Rda")
#save(dtm, file="data/dtmPOS.Rda")

# check out DTMs
findFreqTerms(dtm, 20, Inf)
findFreqTerms(dtmPOS, 20, Inf)

# Finds associations between words 
# Use this only if you're on a server!
# findAssocs(dtm, "obama", 0.1)

# Remove common and rare terms.

# calls super-fast col_sums() function to get term counts:
termCount <- slam::col_sums( dtm )
summary(termCount)

termsToInclude <- termCount[which(termCount > 2 & 
                        termCount < quantile(termCount, .999))]
sort(termsToInclude, decreasing=T)[1:50]

# Do same for POS 
termCountPOS <- slam::col_sums( dtmPOS )
summary(termCountPOS)

termsToIncludePOS <- termCountPOS[which(termCountPOS > 2 & 
                        termCountPOS < quantile(termCountPOS, .999))]
sort(termsToIncludePOS, decreasing=T)[1:50]

# Now limit to these terms:
dtm <- dtm[, which(dtm$dimnames$Terms %in% names(termsToInclude))] 
dtmPOS <- dtmPOS[, which(dtmPOS$dimnames$Terms %in% names(termsToIncludePOS))] 


##### NOTE #####
# Those who prefer may feel free to use this data set instead:

# documented here: http://www.infochimps.com/datasets/twitter-census-stock-tweets
# stocktweets <- read.delim("http://www.stanford.edu/~messing/stock_symbol_keywords.tsv.bz2")



################################################################
################### A LITTLE LEARNING ##########################
################################################################

# I first cover how to use glmnet's implementation of LASSO, Ridge, 
# and ElasticNet learning models first, because glmnet scales
# to very large data sets because (1) it works with sparse matrix
# representations rather than data frames, (2) cross-validation
# is very well-implemented, and (3) it works with continous as
# well as discrete outcome measures.  

# glmnet is nice because it will cross-validate VERY quickly, and  
# do so using a path algorithm that validates across a wide range  
# of lambda values.  Note that one can fit a wide  
# range of models by setting alpha and lambda appropriately.  
# For example, You can get the same estimates as from glm by 
# setting alpha=1 and lambda=0, and you get to keep your fast cross validation).

# But it might not work as well as say SVM or sLDA or other methods.

## Wouldn't hurt to read up: http://www.stanford.edu/~hastie/Papers/glmnet.pdf

library('glmnet')

## We need to convert our TDM object to something more useful.
## And let's do this without sacrificing our lovely sparse data
## structure: 

library('Matrix')
library('slam')

# Convert dtm to Matrix object so we can use it with glmnet:
setAs("slam::simple_triplet_matrix", "Matrix::dgTMatrix", 
        function(from) { 
            new("dgTMatrix", 
                    i = as.integer(from$i - 1L), 
                    j = as.integer(from$j - 1L), 
                    x = from$v, 
                    Dim = c(from$nrow, from$ncol), 
                    Dimnames = from$dimnames) 
        }
) 

class(dtm) <- "simple_triplet_matrix" 
x <- as(dtm, "dgTMatrix")

# First we need to make a y vector: 
y <- ifelse(twdata$PARTY=="D", 1, 0)

# need to remove NAs 
ind <- which(!is.na(y))

y <- y[ind]
x <- x[ind,]

# This is your model matrix.  So,  
# If you have other features, put them to in X matrix.  
# If they are categorical, use model.matrix to get what you need! 

# Linear
glmmod.cv0 <- cv.glmnet(x = x, y = y, alpha = 0.001, nfolds=5, type.measure="mse")
glmmod.cv05 <- cv.glmnet(x = x, y = y, alpha = 0.5, nfolds=5, type.measure="mse")
glmmod.cv1 <- cv.glmnet(x = x, y = y, alpha = 1, nfolds=5, type.measure="mse")

# Logit
glmmod.cv0log <- cv.glmnet(x = x, y = y, alpha = 0, nfolds=5, 
        family="binomial", type.measure="class")
glmmod.cv05log <- cv.glmnet(x = x, y = y, alpha = 0.5, nfolds=5, 
        family="binomial", type.measure="class")
glmmod.cv1log <- cv.glmnet(x = x, y = y, alpha = 1, nfolds=5, 
        family="binomial", type.measure="class")

# hack to get glmmod.cv0log to plot
glmmod.cv0log$nz <- rep("", 100)

pdf('lambda_cv.pdf', width=9)
par(mfrow = c(2,3))
plot( glmmod.cv0, ylim=c(.1, .6) );title("Ridge alpha=0\n")
plot( glmmod.cv05, ylim=c(.1, .6) );title("Elastic Net alpha=.5\n")
plot( glmmod.cv1, ylim=c(.1, .6) );title("LASSO alpha=1\n")
plot( glmmod.cv0log, ylim=c(.1, .5) );title("Logit Ridge alpha=0\n")
plot( glmmod.cv05log, ylim=c(.1, .5) );title("Logit Elastic Net alpha=.5\n")
plot( glmmod.cv1log, ylim=c(.1, .5) );title("Logit LASSO alpha=1\n")
dev.off()

# (e) Now code this up for the POS model and compare the difference. 

# (f) Now get the words with the highest and lowest coeficients:
bestmod <- glmnet(x = x, y = y, alpha = 0.001)
bestmod$lambda

# lowest value of lambda:
coefs <- bestmod$beta[,100]
lowest <- sort(coefs)[1:100]
highest <- sort(coefs, decreasing=T)[1:100]

# take a guess which is which...

# (g) use this approach to predict the presence of a frequently used tag 

################################################################
################## A LITTLE MORE LEARNING ######################
################################################################

# RTextTools is nice because the package authors have streamlined
# things to make these models as accessible as possible to non-experts
# However, they do not provide the facility to adjust tuning 
# parameters in each model, nor does it appear that 

#install.packages("RTextTools")
library('RTextTools')

# Create labels
y <- ifelse(twdata$PARTY=="D", 1, 0)

# create a corpus used for learning
# note that RTextTools plays well with tm:
corpus <- create_container(matrix=dtm,
        labels = y, trainSize = 1:4000,
        virgin=FALSE)

# Cross-validate a bunch of different models:
SVM <- cross_validate(corpus,10,"SVM")
SVM$meanAccuracy # very nice


# These may take a LONG time, make sure you are on a server
SLDA <- cross_validate(corpus,10,"SLDA")
MAXENT <- cross_validate(corpus,10,"MAXENT")
BAGGING <- cross_validate(corpus,10,"BAGGING")
BOOSTING <- cross_validate(corpus,10,"BOOSTING")
RF <- cross_validate(corpus,10,"RF")
NNET <- cross_validate(corpus,10,"NNET")
TREE <- cross_validate(corpus,10,"TREE")
GLMNET <- cross_validate(corpus,10,"GLMNET")

