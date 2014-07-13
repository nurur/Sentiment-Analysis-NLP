#Sentiment Analysis of Beer Consumers
#
#
#Score 5 differnt beers using consumer's sentiment
#Make histogram plots of scores for each individual beer  
#Make scatter plot between ASCI index and result obtained from the Twitter
#==================================================================================

#Load required libraries 
library(twitteR)
library(ROAuth)
library(RCurl) 
library(plyr)
library(stringr)
library(doBy)
library(ggplot2)
#==================================================================================
#Necessary parts to connect to Twitter
#set the directory where the "twitter authentication.Rdata" file is
setwd("/Users/nurur/ugw")

options(RCurlOPtions = list(cainfo = system.file("CurlSSL","cacert.pem", package="Rcurl")))

#load the authetication data that has been saved 
load("twitter authentication.Rdata")

#Provide OAuth access Token to the twitter session
registerTwitterOAuth(cred)
#==================================================================================

# Define a working directory 
workDir     = '/Users/nurur/ugw/prog/gitDocument/Sentiment-Analysis-NLP/'
# Locate the lexicon files within the working directory 

hu.liu.pos <- scan('words-positive-HL.txt', what='character', comment.char=';')
hu.liu.neg <- scan('words-negative-HL.txt', what='character', comment.char=';')

bill.mac.pos <- scan('words-positive-BM.txt', what='character')
bill.mac.neg <- scan('words-negative-BM.txt', what='character')

# Add industry specific words
pos.words    = c(hu.liu.pos, bill.mac.pos, 'upgrade')
neg.words    = c(hu.liu.neg, bill.mac.pos, bill.mac.neg, 
                 'wft', 'wait', 'waiting', 'epicfail', 'mechanical')
#==================================================================================


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
{
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub('\\W', ' ', sentence)

    #print(sentence)
    #convert to lower case:
    sentence = tolower(sentence)


    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')

    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)

    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)

    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


heineken.tweets = searchTwitter('@Heineken', n=1000)
heineken.text   = laply(heineken.tweets, function(t) t$getText())
heineken.scores = score.sentiment(heineken.text, pos.words, neg.words, .progress='text')
heineken.scores$beer = 'Heineken'
heineken.scores$code = 'HK'

samAdams.tweets = searchTwitter('@SamuelAdamsBeer', n=1000)
samAdams.text   = laply(samAdams.tweets, function(t) t$getText())
samAdams.scores = score.sentiment(samAdams.text, pos.words, neg.words, .progress='text')
samAdams.scores$beer = 'SamAdams'
samAdams.scores$code = 'SA'

guinness.tweets = searchTwitter('@Guinness',    n=1000)
guinness.text   = laply(guinness.tweets, function(t) t$getText())
guinness.scores = score.sentiment(guinness.text, pos.words, neg.words, .progress='text')
guinness.scores$beer = 'Guinness'
guinness.scores$code = 'GN'

dosEquis.tweets = searchTwitter('@DosEquis',    n=1000)
dosEquis.text   = laply(dosEquis.tweets, function(t) t$getText())
dosEquis.scores = score.sentiment(dosEquis.text, pos.words, neg.words, .progress='text')
dosEquis.scores$beer = 'DosEquis'
dosEquis.scores$code = 'DE'

corona.tweets   = searchTwitter('@Corona',      n=1000)
corona.text     = laply(corona.tweets, function(t) t$getText())
corona.scores   = score.sentiment(corona.text, pos.words, neg.words, .progress='text')
corona.scores$beer = 'Corona'
corona.scores$code = 'CN'


print('                           ')
print('Data collection is done ...')
#================================================================================================
print('Now combining the data  ...')
print('                           ')
# Combine all data
all.scores <- rbind(heineken.scores, samAdams.scores, guinness.scores, 
                    dosEquis.scores, corona.scores)



all.scores$very.pos <- as.numeric(all.scores$score >= +2)
all.scores$very.neg <- as.numeric(all.scores$score <= -2)

#Create the Twitter data frame 
twitter.df = ddply(all.scores, c('beer', 'code'), summarise, 
                   pos.count = sum(very.pos), neg.count = sum(very.neg))

twitter.df$all.count <- twitter.df$pos.count + twitter.df$neg.count
twitter.df$score     <- round(100 * twitter.df$pos.count / twitter.df$all.count)


#Use the package doBy to order from negative score to positive score 
twittOrdered.df = orderBy(~-score, twitter.df)
#
print('The most popular beers are ...')
print('                              ')
View(twittOrdered.df)
#==================================================================================


# Make a histogram plot from the combined data
# specify a dataframe
# make a separate plot for each airline
ggplot (data=all.scores, mapping = aes(x=score, fill=beer))  
+ geom_bar(binwidth=1) + facet_grid(beer~., scales='free_y') 



#Regular histogram plot
#hist(heineken.scores$score)

#ggplot2 histogram plot
#par(mfrow=c(3,2))
#qplot(heineken.scores$score, geom="histogram", alpha=I(.5),
#   main="Distribution of Gas Milage", 
#   xlab="Sentiment Score",
#   ylab="Density")
#par(new=TRUE)
#qplot(samAdams.scores$score)


