# Question 4

# a. load in the data
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')

# b. print out the data
system('cat dates.json')

# c. 
# convert json to list
library(jsonlite)
library(tidyverse)
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# d. check type of object
class(mydf)
class(mydf$date)

# list out first 10 rows
head(mydf, 10)
