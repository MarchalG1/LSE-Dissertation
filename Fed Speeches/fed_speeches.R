rm(list = ls())
gc()

library(tidyverse)
library(httr)
library(jsonlite)

download_speeches = F

if(download_speeches == T){

base_url <- "https://datasets-server.huggingface.co/rows"
dataset <- "istat-ai/ECB-FED-speeches"
config <- "default"
split <- "train"
page_size <- 100


size_url <- "https://datasets-server.huggingface.co/size?dataset=istat-ai%2FECB-FED-speeches"
size_resp <- GET(size_url)
size_info <- fromJSON(content(size_resp, as = "text", encoding = "UTF-8"))
total_rows <- size_info$size$dataset$num_rows

all_pages <- list()
offset <- 0
i <- 1

while (offset < total_rows) {
  url <- paste0(
    base_url,
    "?dataset=", URLencode(dataset, reserved = TRUE),
    "&config=", config,
    "&split=", split,
    "&offset=", offset,
    "&length=", page_size
  )
  
  resp <- GET(url)
  
  if (status_code(resp) != 200) {
    warning(paste("Failed at offset", offset, "- status", status_code(resp)))
    Sys.sleep(2)
    next  # retry same offset instead of skipping it
  }
  
  page_data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  all_pages[[i]] <- page_data$rows
  
  cat("Fetched", offset, "-", min(offset + page_size, total_rows), "of", total_rows, "\n")
  
  offset <- offset + page_size
  i <- i + 1
  
  Sys.sleep(0.5)
}

speeches_full <- bind_rows(all_pages)

# Verify
nrow(speeches_full)

write_csv(speeches_full, "Fed Speeches/fed_speeches.csv")

}

speeches_full <- read_csv("Fed Speeches/fed_speeches.csv")

###OLD
{
# # Build the request URL
# # url <- "https://datasets-server.huggingface.co/rows?dataset=istat-ai%2FECB-FED-speeches&config=default&split=train&offset=0&length=100"
# # url <- "https://datasets-server.huggingface.co/splits?dataset=istat-ai%2FECB-FED-speeches"
# # url <- "https://huggingface.co/api/datasets/istat-ai/ECB-FED-speeches/parquet/default/train"

# url <- "https://datasets-server.huggingface.co/rows?dataset=istat-ai%2FECB-FED-speeches&config=default&split=train&offset=0&length=100"


# # GET request
# resp <- GET(url)

# # Check status
# stop_for_status(resp)

# # Parse JSON content
# data_raw <- content(resp, as = "text", encoding = "UTF-8")
# data_json <- fromJSON(data_raw, flatten = TRUE)

# # The actual rows are nested under $rows$row
# speeches_df <- data_json$rows$

# speeches_df$rows

# # Inspect structure
# # str(speeches_df)
# # head(speeches_df)

# size_url <- "https://datasets-server.huggingface.co/size?dataset=istat-ai%2FECB-FED-speeches"
# size_resp <- GET(size_url)
# size_info <- fromJSON(content(size_resp, as = "text", encoding = "UTF-8"))
# print(size_info$size$dataset$num_rows)
}