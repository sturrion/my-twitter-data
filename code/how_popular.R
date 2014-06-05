
source("code/configuration.R")
source("code/twitter_functions.R")

library(httr)
library(plyr)

# Accessing Twitter from R
myapp <- oauth_app(app.name, key=key, secret=secret)
sig <- sign_oauth1.0(myapp, token = token, token_secret = token.secret)

# Get user info
user.relationships <- get.user.relationships(my.twitter.user, sig)

# Get followers relationships
user.followers <- get.followers.relationships(my.twitter.user, sig)

# Get friends relationships
user.friends <- get.friends.relationships(my.twitter.user, sig)

# Merge followers and friends
relationships <- merge(user.followers, user.friends, all=TRUE)

relationships[c("is.follower", "is.friend")][is.na(relationships[c("is.follower", "is.friend")])] <- FALSE


# plotting
library(ggplot2) 
library(scales)

# followers data
g <- ggplot(user.followers, aes(x=unlist(followers.count), 
                                y=unlist(friends.count)))
g <- g + geom_point(colour = "#00BFC4")
g <- g + scale_x_continuous(labels = comma)
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(x = "Followed by...") 
g <- g + labs(y = "Following to...")
g <- g + labs(title="Followers Relationships")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold"))
g <- g + geom_point(colour="#F8766D", 
                      aes(x=user.relationships$followers.count, 
                          y=user.relationships$friends.count))

graph.followers <- g

# log followers data
g <- ggplot(user.followers, aes(x=log10(unlist(followers.count)), 
                                y=log10(unlist(friends.count))))
g <- g + geom_point(colour = "#00BFC4")
g <- g + labs(x = expression("log"[10]*" followed by...")) 
g <- g + labs(y = expression("log"[10]*" following to...")) 
g <- g + labs(title="Followers Relationships")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold"))
g <- g + geom_point(colour="#F8766D", 
                    aes(x=log10(user.relationships$followers.count), 
                        y=log10(user.relationships$friends.count)))

graph.log.followers <- g

# friends data
g <- ggplot(user.friends, aes(x=unlist(followers.count), 
                                y=unlist(friends.count)))
g <- g + geom_point(colour = "#00BFC4")
g <- g + scale_x_continuous(labels = comma)
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(x = "Followed by...") 
g <- g + labs(y = "Following to...")
g <- g + labs(title="Friends Relationships")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold"))
g <- g + geom_point(colour="#F8766D", 
                    aes(x=user.relationships$followers.count, 
                        y=user.relationships$friends.count))
graph.friends <- g

# log friends data
g <- ggplot(user.friends, 
            aes(x=log10(unlist(followers.count)), 
                y=log10(unlist(friends.count))))
g <- g + geom_point(colour ="#00BFC4")
g <- g + labs(x = expression("log"[10]*" followed by...")) 
g <- g + labs(y = expression("log"[10]*" following to...")) 
g <- g + labs(title="Friends Relationships")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold"))
g <- g + geom_point(colour="#F8766D", 
                    aes(x=log10(user.relationships$followers.count), 
                        y=log10(user.relationships$friends.count)))

graph.log.friends <- g


# print plots
png(filename = "images/twitter_relationships.png", width = 640, height = 640) ## Create my plots in a PNG file

multiplot(graph.followers, graph.log.followers,
          graph.friends, graph.log.friends,
            cols=2)

dev.off()  ## Don't forget to close the PNG device!

