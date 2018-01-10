# 
# 
# data table structure:
# timestamp
# actions{friend request, friend accept, unfriend}
# reviever id
# requester id
# 
# questions:
#   
# what is the trend of frequest over time
#    -- things to consider # 
# 
# data table structure:
# timestamp
# actions{friend request, friend accept, unfriend}
# reviever id
# requester id
# 
# questions:
#   
# what is the trend of frequest over time
#    -- things to consider 
# count the number of friend requests by user
#    -- to break this down first filter by actions
#    -- t
# who has the most requests
# how to find the user with most friend requests

dataFramTest <- data.frame(date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"))

dataFramTest$users <- sample( LETTERS[1:5], 90, replace=TRUE, prob=c(0.1, 0.2, 0.65, 0.05,0.05) )
dataFramTest$actions <- sample( c("friend_Request", "friend_Acceptance", "unfriend"), 90, replace=TRUE, prob=c(0.1, 0.2, 0.65) )


#this finds the user with the max number of friend acceptances
dataFramTest%>%
  filter(actions == "friend_Acceptance")%>%
  group_by(users)%>%
  count(users)%>%
  ungroup(users)%>%
  filter(n == max(n))



dataFramTest%>%
  group_by(users)%>%
  count(actions)


# finds max friends per group
dataFramTest%>%
  group_by(users)%>%
  count(actions)%>%
  spread(actions,n)%>%
  mutate(totalFriends = friend_Acceptance - unfriend)%>%
  ungroup(users)%>%
  slice(which.min(totalFriends))

#finds rate of acceptance over time
dataFramTest%>%
  mutate(monthNum = month(date))%>%
  mutate(yearNum = year(date))%>%
  group_by(yearNum,monthNum)%>%
  count(actions)%>%
  spread(actions,n)%>%
  mutate(totalActions = friend_Acceptance+friend_Request+unfriend)%>%
  mutate(rate = friend_Acceptance/totalActions)


group_by(users)%>%count(actions)%>%
  spread(actions,n)%>%
  mutate(totalActions = friend_Acceptance + friend_Request+unfriend)


# count the number of friend requests by user
#    -- to break this down first filter by actions
#    -- t
# who has the most requests
# how to find the user with most friend requests

mtcars%>%filter(cyl==c(4,6))%>%group_by(cyl)%>%count(cyl)


# playing with slice


mtcars%>%arrange(cyl)
mtcars%>%group_by(cyl)%>%
  summarise(wtSum = sum(wt), hpSum = sum(hp))

mtcars%>%arrange(cyl)
mtcars%>%group_by(cyl)%>%
  summarise(cylCount = n())
mtcars%>%arrange(cyl)
mtcars%>%group_by(cyl)%>%
  count(cyl)


mtcars%>%summarise(n_distinct(cyl))
mtcars%>%mutate(n_distinct(cyl))

 
