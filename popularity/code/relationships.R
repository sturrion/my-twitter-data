
source("code/configuration.R")
source("code/twitter_functions.R")

library(httr)
library(plyr)

# Accessing Twitter from R
myapp <- oauth_app(app.name, key=key, secret=secret)
sig <- sign_oauth1.0(myapp, token = token, token_secret = token.secret)

# Get user info
data.relationships <- cbind("user", get.user.relationships(my.twitter.user, sig))
colnames(data.relationships)[1] <- "type" 

# Get followers relationships
user.followers <- get.followers.relationships(my.twitter.user, sig)

# Get friends relationships
user.friends <- get.friends.relationships(my.twitter.user, sig)

# Merge followers and friends
relationships <- merge(user.followers, user.friends, all=TRUE, row.names = )
relationships[c("is.follower", "is.friend")][is.na(relationships[c("is.follower", "is.friend")])] <- FALSE

rm(user.followers, user.friends)

rows.to.add <- cbind("both", relationships[relationships$is.follower & relationships$is.friend,])
colnames(rows.to.add)[1] <- "type" 
data.relationships <- rbind(data.relationships, rows.to.add[,1:4])

rows.to.add <- cbind("friend", relationships[!relationships$is.follower & relationships$is.friend,])
colnames(rows.to.add)[1] <- "type" 
data.relationships <- rbind(data.relationships, rows.to.add[,1:4])

rows.to.add <- cbind("follower", relationships[relationships$is.follower & !relationships$is.friend,])
colnames(rows.to.add)[1] <- "type" 
data.relationships <- rbind(data.relationships, rows.to.add[,1:4])

rm(relationships, rows.to.add)

# plotting
library(ggplot2) 
library(scales)

# relationships graph
g <- ggplot(data.relationships, aes(x=log10(followers.count), 
                                    y=log10(friends.count),
                                    color=type))
g <- g + geom_point()
g <- g + labs(x = expression("log"[10]*" followed by...")) 
g <- g + labs(y = expression("log"[10]*" following to...")) 
g <- g + labs(title="User Relationship Types")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold"))

# print plot
png(filename = "images/user_relationship_types.png", width = 640, height = 640)

print(g)

dev.off()  ## Don't forget to close the PNG device!

