library(twitteR)
library(ROAuth)
library(RCurl) 
options(RCurlOPtions=list(cainfo=system.file("CurlSSL","cacert.pem",package="Rcurl")))

setwd("/Users/nurur/ugw/")

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

KEY <- "eCslRfFl4CIaz4MSgrqVXA"
SECRET <-"BfhvC8p4ZVGgxPiq4G6WCTR8GJVx7iGfeKKLVOQvbo4"
## create an object that will save the authenticated object -- we can for later sessions 
## will need to navigate to website and type in data to generate the file
## NOTE: Only need to do this part once!!!
cred <- OAuthFactory$new(consumerKey = KEY,
                         consumerSecret = SECRET,
                         requestURL = "https://api.twitter.com/oauth/request_token", accessURL = "https://api.twitter.com/oauth/access_token", authURL = "https://api.twitter.com/oauth/authorize")
cred$handshake(cainfo="cacert.pem")
registerTwitterOAuth(cred)
save(cred, file="twitter authentication.Rdata")