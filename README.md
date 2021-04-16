# RTwitterV2
This Repository contains code to loop through timelines and the academic search API of the new Twitter API v2.


## Istallation
```r
devtools::install_github("MaelKubli/RTwitterV2)
```

## Demo
Getting Tweets from a specific users timeline via `get_timelines_v2()`. This function gets the last n tweets from a users timeline (max n = 3200).

```r
Bearer_Token <- "" #Insert your Bearer Token

test <- get_timelines_v2(token = Bearer_Token, user_id = "14273050", n = 100)
```

Getting Users Information

```r
Bearer_Token <- "" #Insert your Bearer Token
test <- get_user_v2(token = Bearer_Token, user_ids = "959432550400831488,62777265,14273050")
```

Getting Tweets from a Query (Academic Track Required!)

```r
Bearer_Token <- "" #Insert your Beaer Token

query_content <- "#TwitterDev"

lower <- "2021-01-01T00:00:01Z"
upper <- "2021-02-01T00:00:01Z"

test <- .full_archive_search(token = Bearer_Token, 
                             search_query = query_content, 
                             start_time = lower, 
                             end_time = upper, 
                             n = 250000)

```

