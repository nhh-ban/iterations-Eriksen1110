library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)

### 4a: time-function
source("functions/data_transformations.r")

# Test the function
to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)

### 5: Final volume query: 
source("gql-queries/vol_qry.r")

# Extract 1 random station
stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) -> randome_station
  
# Query the data, and make a line plot over the volume
vol_qry(
    id = randome_station$id,
    from = to_iso8601(randome_station$latestData, -4),
    to = to_iso8601(randome_station$latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>%  
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  xlab("Dates") +
  ylab("Volume") +
  ggtitle(paste("Traffic during a 4 days timeperiode at", randome_station$name )) +
  theme_classic()




