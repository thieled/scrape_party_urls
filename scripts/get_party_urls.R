#############################
#
# Scrape Party Websites from Wiki
# Author: Daniel Thiele
# Date: 2024-01-22
#
#############################

rm(list=ls())

library(magrittr)
library(tidyverse)
library(rvest)
library(pbapply)


# download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# link datasets (select only linked parties)
pf_df <- partyfacts |> filter(dataset_key == "parlgov")


# Merge wikipedia page ----------------------------------------------------

wiki_pf_df <- read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/main/import/wikipedia/wikipedia.csv"
) |>
  select(-country, -name_short, -name)


merged_df <- left_join(pf_df, wiki_pf_df)



# Scraper function -------------------------------------------------------

get_party_url <- function(wiki_url){
  
  party_url <- tryCatch(
    {
      rvest::read_html(wiki_url)  |>
        rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "infobox-full-data", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "text", " " ))]') |>
        rvest::html_attr('href')
    },
    error = function(e) {
      message(paste0("An error occured while calling rvest::read_html: ", e))
      NA
    }
  )
  
  if(length(party_url) == 0){
    party_url <- NA
  }
  
  return(party_url)
}


# Sample & Example  ------------------------------------------------------------------

set.seed(42)
sample_df <- merged_df |>
  filter(!is.na(url),
         is.na(year_dissolved)) |>
  slice_sample(n = 10)



# For one party
get_party_url(sample_df$url[[2]])  


# For the whole df
# Note: pbsapply is just sapply with a nice process bar
sample_df$party_urls <-  pbapply::pbsapply(sample_df$url, get_party_url)

head(sample_df$party_urls)
