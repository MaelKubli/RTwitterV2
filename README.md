# RTwitterV2
This Repository contains code to loop through timelines and the academic search API of the new Twitter API v2.


## Installation
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

Stream Tweets (Sampled Stream Only)
```r
Bearer_Token <- "" #Insert your Bearer Token

sampled_stream(token = Bearer_Token,
               timeout = 300,
               backfill = 1,
               file_name = "test_pkg_001.json")
               
path <- file("test_pkg_001.json")
tdf <- parse_sampled_stream(path = path)
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
                              n = 250000,
                              n_try = 10)
  
  if(is.null(df)==T){
    df <- tmp
    # setwd(parent_path) # set working directory (if necessary)
  } else {
    df <- dplyr::bind_rows(df,tmp)
    # setwd(parent_path) # set working directory (if necessary)
  }
  cat(paste0(lower," to ", upper, " has been colleted!\n"))  
}

write_csv(df, "abst21_Jan-Mar.csv")

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

If you want to search Tweets by location only this can be done best with the function full_archive_search_locations(). 
Be aware that the filtering operators for locations will not match on ReTweets, since Retweet's places are attached to the original Tweet. 
It will also not match on places attached to the original Tweet of a Quote Tweet.
Additionaly you need to keep in mind that only small percentage of Tweets have geo-tagged information associated with them. 
Thus, relying only on geo-tagged Tweets alone might introduce some bias in your dataset. 
You can still add a normal query to the function with the search_query Operator, which will be combined with the location search. 

```R
Bearer_Token <- "" #Insert your Beaer Token

#place_country search
tmp <- full_archive_search_locations(token = Bearer_Token, n = 1000, api_wait = 15, n_try = 10, JSON = FALSE,
                                     country = "US")

#point_radius search
tmp <- full_archive_search_locations(token = Bearer_Token, n = 1000, api_wait = 15, n_try = 10, JSON = FALSE,
                                     longitude = 8.550000 , latitude = 47.3666700, radius = 10)

#bounding_box search
tmp <- full_archive_search_locations(token = Bearer_Token, n = 1000, api_wait = 15, n_try = 10, JSON = FALSE,
                                     bounding_box = c(8.500000, 47.36600, 8.590000, 47.36700))

```

Filtering: 

```R
library(RTwitterV2)

#To exclude retweets just add the filter -is:retweet to the query itself:
query <- "#Biden OR #Trump -is:retweet"

lower <- "2020-06-01T00:00:01Z"
upper <- "2021-06-01T00:00:01Z"

tmp <- full_archive_search(token = Bearer_Token, search_query = query, n = 2500,
start_time=lower, end_time=upper, JSON = FALSE)

unique(tmp$is_retweet)
```

Streaming Tweets (1 % Stream)
```R
library(RTwitterV2)

setwd("YOUR DESIRED PATH")
sampled_stream(Token = your_token, timeout = 60, backfill = 0,
               file_name = "test_stream_1.json", verbose = T,
               parse = F)
               
path <- file("test_stream_001.json")
tdf <- parse_stream(path = path, filtered = F)
               
```

Streaming Tweets (Filtered Stream)
```R
library(RTwitterV2)

setwd("YOUR DESIRED PATH")

# Add Filter Rule
filtered_stream_add_rule(token = Bearer_Token,
                         value = "#Trump",
                         tag = "Test")

# Get all Rules
filtered_stream_get_rules(token = Bearer_Token)

# Start a Filtered Stream
filtered_stream(token = Bearer_Token,
                timeout = 25,
                backfill = 0,
                file_name = "test_filtered_stream_001.json")

# Remove Rule either by its id or its actual value
filtered_stream_delete_rule(token = Bearer_Token,
                            id = "",
                            value = "")
                            
path <- file("test_filtered_stream_001.json")
# Parse Stream 
tdf <- parse_stream(path = path, filtered = T)
```
