#Sentiment Analysis of Airline Consumers
#
#
#Score 7 differnt airlines using passenger's sentiment
#Make histogram plots of scores for each individual airline  
#Make scatter plot between ASCI index and result obtained from the Twitter
#==================================================================================

#Load required libraries 
library(twitteR)
library(ROAuth)
library(RCurl) 
library(RJSONIO)
library(plyr)
library(stringr)
library(doBy)
library(ggplot2)
library(XML)
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
posTextFile = paste(workDir, 'words-positive-HL.txt', sep='') 
negTextFile = paste(workDir, 'words-negative-HL.txt', sep='') 

#set the directory 
setwd(workDir)


#=================================================================================================
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
	require(plyr)
	require(stringr)

	# we got a vector of sentences. plyr will handle a list
	# or a vector as an "l" for us
	# we want a simple array of scores back, so we use
	# "l" + "a" + "ply" = "laply":
	scores = laply(sentences, function(sentence, pos.words, neg.words) {

		# clean up sentences with R's regex-driven global substitute, gsub():
		sentence = gsub('[[:punct:]]', '', sentence)
		sentence = gsub('[[:cntrl:]]', '', sentence)
		sentence = gsub('\\d+', '', sentence)

                #############################################################
                # clean the unicode characters which appears in the text
                # this part is missing in the original code!!!!
                sentence = gsub('\\W', ' ', sentence)
                #############################################################

		# convert to lower case:
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
# =================================================================================

# Load the opinion lexicon text files 
hu.liu.pos <- scan(posTextFile, what='character', comment.char=';')
hu.liu.neg <- scan(negTextFile, what='character', comment.char=';')

# Add industry specific words
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wft', 'wait', 'waiting', 'epicfail', 'mechanical')



# =================================================================================

# Get the airline data from twitter
delta.twitter        = searchTwitter('@delta',         n=2500)
print ('Done with DL searching ... ...')  

american.twitter     = searchTwitter('@AmericanAir',   n=2500)
print ('Done with AA searching ... ...')  

continental.twitter  = searchTwitter('@continental',   n=2500)
print ('Done with CO searching ... ...')  

united.twitter        = searchTwitter('@united',       n=2500)
print ('Done with UA searching ... ...')  

jetblue.twitter       = searchTwitter('@jetblue',      n=2500)
print ('Done with BC searching ... ...')  

southwest.twitter     = searchTwitter('@Southwest',    n=2500)
print ('Done with WN searching ... ...')  

usair.twitter         = searchTwitter('@USAirways',    n=2500)
print ('Done with US searching ... ...')  
#==================================================================================


# Process the data
delta.text              = laply(delta.twitter, function(t) t$getText())
delta.scores            = score.sentiment(delta.text, pos.words, neg.words, .progress='text')
delta.scores$airline    = 'Delta'
delta.scores$code       = 'DL'

american.text           = laply(american.twitter,function(t) t$getText())
american.scores         = score.sentiment(american.text, pos.words, neg.words, .progress='text')
american.scores$airline = 'Amertican'
american.scores$code    = 'AA'

continental.text        = laply(continental.twitter,function(t) t$getText())
continental.scores      = score.sentiment(continental.text, pos.words, neg.words, .progress='text')
continental.scores$airline = 'Continental'
continental.scores$code = 'CO'

united.text             = laply(united.twitter,function(t) t$getText())
united.scores           = score.sentiment(united.text, pos.words, neg.words, .progress='text')
united.scores$airline   = 'United'
united.scores$code      = 'UA'

jetblue.text            = laply(jetblue.twitter,function(t) t$getText())
jetblue.scores          = score.sentiment(jetblue.text, pos.words, neg.words, .progress='text')
jetblue.scores$airline  = 'JetBlue'
jetblue.scores$code     = 'BC'

southwest.text          = laply(southwest.twitter,function(t) t$getText())
southwest.scores        = score.sentiment(southwest.text, pos.words, neg.words, .progress='text')
southwest.scores$airline= 'Southwest'
southwest.scores$code   = 'WN'

usair.text              = laply(usair.twitter,function(t) t$getText())
usair.scores            = score.sentiment(usair.text, pos.words, neg.words, .progress='text')
usair.scores$airline    = 'US Airways'
usair.scores$code       = 'US'


print('                           ')
print('Data collection is done ...')
print('Now combining the data  ...')
print('                           ')
#================================================================================================


# Combine all data
all.scores <- rbind(american.scores, continental.scores, delta.scores, 
                    jetblue.scores, united.scores, usair.scores, 
		    southwest.scores)
 
#==================================================================================


# Make a histogram plot from the combined data
ggplot (data=all.scores, mapping = aes(x=score, fill=airline)) +   ## specify a dataframe
        geom_bar(binwidth=1) +
        facet_grid(airline~., scales='free_y')            ## make a separate plot for each airline
        #theme_bw() + scale_fill_brewer()


all.scores$very.pos <- as.numeric(all.scores$score >= +2)
all.scores$very.neg <- as.numeric(all.scores$score <= -2)


#==================================================================================
#Create the Twitter data frame 
twitter.df = ddply(all.scores, c('airline', 'code'), summarize, 
                   pos.count = sum(very.pos), neg.count = sum(very.neg))

twitter.df$all.count <- twitter.df$pos.count + twitter.df$neg.count
twitter.df$score     <- round(100 * twitter.df$pos.count / twitter.df$all.count)


#Use the package doBy to order from negative score to positive score 
orderBy(~-score, twitter.df)
#
View(twitter.df)

#Write the data frame 
#write.csv(twitter.df,   file = 'twitter.csv' ) 
#write.table(twitter.df, file = 'twitter.tb' ) 
#==================================================================================


#Create the ACSI data frame 
# Relating to airlines American Customer Satisfaction Index (ACSI) score
# Get the url
acsi.url = "http://www.theacsi.org/?option=com_content&view=article&id=147&catid=14&Itemid=212&i=Airlines"
# Read the data frame from the url
acsi.df <- readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)


# For each row, only keep column 1 (name) and 20 (2012 score) 
acsi.df <- acsi.df[,c(1,20)]
View(acsi.df)

# Name the columns of the ASCI table
colnames(acsi.df) = c('airline', 'score')

acsi.df$code  <- c('BC','WN',NA, NA,'DL','AA','US','UA','CO',NA)
acsi.df$score <- as.numeric(acsi.df$score)  # Problem here ... missing Northwest

# Merge the tables by matching the code
compare.df <- merge(twitter.df, acsi.df, by='code',
                    suffixes=c('.twitter', '.acsi'))

#View the merged table
View(compare.df)

# Retain only those with high "counts" 
#compare.df <- subset(compare.df, all.count > 90)


#Make the scatter plot between ASCI index and result obtained from Twitter
ggplot(compare.df) + 
       coord_cartesian(xlim=c(50, 90)) +
       geom_point( aes(x=score.twitter, y=score.acsi, color=airline), size=5) +
       geom_smooth(aes(x=score.twitter, y=score.acsi, group = 1), se=F, method="lm") +
       theme_bw() +
       theme(legend.position=c(0.2,0.85))
