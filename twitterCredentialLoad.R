library(twitteR)
library(ROAuth)
library(RCurl) 

options(RCurlOPtions=list(cainfo=system.file("CurlSSL","cacert.pem",
        package="Rcurl")))

#set the directory 
setwd('/Users/nurur/ugw/')

#load the authetication data that has been saved 
load("twitter authentication.Rdata")

#Provide OAuth access Token to the twitter session
registerTwitterOAuth(cred)