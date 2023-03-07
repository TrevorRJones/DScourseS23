# Question 3
library(rvest)
pacman::p_load(tidyverse, rvest, lubridate, janitor, data.table, hrbrthemes)

# getting combine measurements from 2023 Combine for Quarterbacks
url <- 'https://nflcombineresults.com/nflcombinedata.php?year=&pos=QB&college='
html_path <- '#wrapper > div.content > div > div'
df <- read_html(url) %>% html_nodes(html_path) %>% html_table() %>% '[['(1)
head(df)


# Question 4
# getting data from yahoo finance, since google finance stopped providing data in March 2018
library(quantmod)
library(tidyverse)

# Set the stock symbol and the date range
symbol <- "F"
start_date <- as.Date("2022-01-01")
end_date <- Sys.Date()

# Get the stock prices
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)

# Convert to a data frame
Ford <- as.data.frame(F)
head(Ford)

# Creating a variable of interest: the gain or loss from overnight holding strategy:
Ford1 <- as.data.frame(F) %>% 
  mutate(overnight_gain_loss = F.Open - lag(F.Close)) %>% 
  na.omit()
tail(Ford1)

summary_table <- Ford1 %>% 
  summarize(mean_gain_loss = mean(overnight_gain_loss),
            sd_gain_loss = sd(overnight_gain_loss),
            min_gain_loss = min(overnight_gain_loss),
            max_gain_loss = max(overnight_gain_loss))

# View the summary table
summary_table

# At least for Ford, the overnight holding strategy does not work well. 
