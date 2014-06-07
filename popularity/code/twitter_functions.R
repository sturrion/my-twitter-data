################################################################################
# Function to check twitter status
################################################################################
get.status <- function(sign.oauth) {
    request <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    
    response <- GET(request, sign.oauth)
    
    # Converting the json object to a data frame
    status <- content(response)
    
    status
}

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
    
    print(followers[["next_cursor_str"]])
    
    if (followers[["next_cursor_str"]] != 0) {
        status <- get.status(sign.oauth)
        remaining <- paste0("status$resources$followers$`/followers/list`$remaining",
                            status$resources$followers$`/followers/list`$remaining)
        print(remaining)
    }
    
    while (!is.null(followers[["next_cursor_str"]]) && followers[["next_cursor_str"]] != "0") {
        request <- paste0("https://api.twitter.com/1.1/followers/list.json?cursor=",
                          followers[["next_cursor_str"]]
                          ,"&screen_name=", 
                          user.name, 
                          "&count=200&skip_status=true&include_user_entities=false")
        
        response <- GET(request, sign.oauth)
        followers = content(response)
        followers.list <- c(followers.list, followers[["users"]])
        
        print(followers[["next_cursor_str"]])
        Sys.sleep(60)
    } 
    
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
    
    print(friends[["next_cursor_str"]])
    
    if (friends[["next_cursor_str"]] != 0) {
        status <- get.status(sign.oauth)
        remaining <- paste0("status$resources$friends$`/friends/list`$remaining",
                        status$resources$friends$`/friends/list`$remaining)
        print(remaining)
    }
    
    while (!is.null(friends["next_cursor_str"] ) && friends[["next_cursor_str"]] != "0") {
        request <- paste0("https://api.twitter.com/1.1/friends/list.json?cursor=",
                          friends[["next_cursor_str"]]
                          ,"&screen_name=", 
                          user.name, 
                          "&count=200&skip_status=true&include_user_entities=false")
        
        response <- GET(request, sign.oauth)
        friends = content(response)
        friends.list <- c(friends.list, friends[["users"]])
        
        print(friends[["next_cursor_str"]])
        Sys.sleep(60)
    } 
    
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

