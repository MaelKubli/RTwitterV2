# RTwitterV2
This Repository contains code to loop through timelines and the academic search API of the new Twitter API v2.


## Istallation
```r
devtools::install_github("MaelKubli/RTwitterV2")
library(RTwitterV2)
```

## Demo
Getting Tweets from a specific users timeline via `get_timelines_v2()`. This function gets the last n tweets from a user's timeline (max n = 3200).

```r
Bearer_Token <- "" #Insert your Bearer Token

test <- get_timelines_v2(token = Bearer_Token, user_id = "14273050", n = 100)
```

Getting Users Information

```r
Bearer_Token <- "" #Insert your Bearer Token
test <- get_user_v2(token = Bearer_Token, user_ids = "959432550400831488,62777265,14273050")
```

Getting Tweets from a Query with a set timeframe (Academic Track Required!)

```r
Bearer_Token <- "" #Insert your Beaer Token

query_content <- "#TwitterDev"

lower <- "2021-01-01T00:00:01Z"
upper <- "2021-02-01T00:00:01Z"

test <- full_archive_search(token = Bearer_Token, 
                             search_query = query_content, 
                             start_time = lower, 
                             end_time = upper, 
                             n = 250000)

```

Apart from collecting tweets with a start_time and end_time you can also search Tweets within a range of id's by setting a lower and upper bound via since_id and until_id or simply search a sample of X tweets by setting n to a given number.
If you are going to collect tweets in a big manner. I advise you to loop to smaller timeframes or id ranges, since the package is still experimental and might stop working after a prolonged continuous search of tweets. 

For example, collect Tweets with the hashtag #abst21 from 2021 like this:

```r
# libraries
library(readr)
library(dplyr)
library(RTwitterV2)

# Set Directory
setwd("Your Directory of choice")

# Bearer Token
Bearer_Token <- "" # Your Bearer-Token

# query
query <- "#abst21"

# timeframe
days <- seq(as.Date("2021-01-01"),as.Date("2021-04-01"), by =  "day")


# collect tweets
df <- NULL

for(i in 2:length(days)){
  lower <- paste0(days[i-1],"T00:00:01Z")
  upper <- paste0(days[i],"T00:00:01Z")
  
  tmp <- full_archive_search(token = Bearer_Token, 
                              search_query = query, 
                              start_time = lower, 
                              end_time = upper, 
                              n = 250000)
  
  if(is.null(df)==T){
    df <- tmp
    # setwd(parent_path) # set working directory (if necessary)
    write_csv(df, "abst21_Jan-Mar.csv")
  } else {
    df <- dplyr::bind_rows(df,tmp)
    # setwd(parent_path) # set working directory (if necessary)
    write_csv(df, "abst21_Jan-Mar.csv")
  }
  cat(paste0(lower," to ", upper, " has been colleted!\n"))  
}

```

With the archive search it is not only possible to search for tweets with certain keywords but also to collect the entire timeline of a user without the restriction of the last 3200 tweets. 
For example if you would like to collect all tweets from the @TwitterDev account:

```R
Bearer_Token <- "" #Insert your Beaer Token

query_content <- "from:TwitterDev"

test <- full_archive_search(token = Bearer_Token, 
                            search_query = query_content, 
                            n = 20000)

```