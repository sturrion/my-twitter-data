################################################################################
# This function returns a list object with a variety of information about the 
# user specified by the required user.name (twitter screen name) parameter.
################################################################################
get.user <- function(user.name, sign.oauth) {

    request <- paste0("https://api.twitter.com/1.1/users/show.json?screen_name=", 
                      user.name,
                      "&include_entities=false")
    
    response <- GET(request, sign.oauth)
    
    # Converting the json object to a data frame
    user.list <- content(response)
    user.list
    
}

################################################################################
# It returns a data frame with these fields:
#   screen name
#   number of followers
#   number of friends (people whom the user follows)
################################################################################
get.user.relationships <- function(user.name, sign.oauth) {
    user <- get.user(user.name, sign.oauth)
    
    user.relationships <- data.frame(user[["screen_name"]], 
                                     user[["followers_count"]], 
                                     user[["friends_count"]])
    colnames(user.relationships) <- c("screen.name", "followers.count", "friends.count")
    
    user.relationships
}

################################################################################
# It returns the follower list of a user
################################################################################
get.followers <- function(user.name, sign.oauth) {
    
    request <- paste0("https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=", 
                       user.name, 
                       "&count=200&skip_status=true&include_user_entities=false")
    
    response <- GET(request, sign.oauth)
    
    followers = content(response)
    followers.list <- followers[["users"]]
    
    followers.list
}

################################################################################
# It returns a data frame for the followers of a user with these fields:
#   screen name
#   number of followers
#   number of friends (people whom the user follows)
################################################################################
get.followers.relationships <- function(user.name, sign.oauth) {
    
    followers <- get.followers(user.name, sign.oauth)
    
    followers.relationships <- ldply(followers, function(x) {
        data.frame(screen.name=x[["screen_name"]],
                   followers.count=x[["followers_count"]],
                   friends.count=x[["friends_count"]])
    })
    
    followers.relationships$is.follower=TRUE
    followers.relationships
}

################################################################################
# It returns the friend list of a user
################################################################################
get.friends <- function(user.name, sign.oauth) {
    
    request <- paste0("https://api.twitter.com/1.1/friends/list.json?cursor=-1&screen_name=", 
                      user.name, 
                      "&count=200&skip_status=true&include_user_entities=false")
    
    response <- GET(request, sign.oauth)
    
    friends = content(response)
    friends.list <- friends[["users"]]
    
    friends.list
}

################################################################################
# It returns a data frame for the friends of a user with these fields:
#   screen name
#   number of followers
#   number of friends (people whom the user follows)
################################################################################
get.friends.relationships <- function(user.name, sign.oauth) {
    
    friends <- get.friends(user.name, sign.oauth)
    
    friends.relationships <- ldply(friends, function(x) {
        data.frame(screen.name=x[["screen_name"]],
                   followers.count=x[["followers_count"]],
                   friends.count=x[["friends_count"]])
    })
    friends.relationships$is.friend=TRUE
    friends.relationships
}

# This is from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
