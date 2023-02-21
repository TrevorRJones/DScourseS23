# 1.
  # install.packages(sparklyr)
  # install.packages(tidyverse)
library(sparklyr)
library(tidyverse)

# get and connect to spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# load in iris data 
df1 <- as_tibble(iris)

# load tibble into spark
df <- copy_to(sc, df1)

class(df1)
class(df)

colnames(df1)
colnames(df)
# one uses . one uses _ between words, other than that they are the same

df %>% select(Sepal_Length, Species) %>% head(6) %>% print()

# where sepal length is greater than 5.5
df %>% filter(Sepal_Length > 5.5) %>% head(6) %>% print()

#11
df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head(6) %>% print()

#12
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head() %>% print()

#13
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head()
df2 %>% arrange(Species) %>% head() %>% print()
