# This code downloads 5,000 governor speeches from 1997
# (with 1 1996 speech) to 2025. 
# https://huggingface.co/datasets/istat-ai/ECB-FED-speeches
# Downloaded on July 6, 2025
# The texts were obtained from the BIS

# I then added a code that downloads 1996 speeches fo


rm(list = ls())
gc()

library(tidyverse)
library(httr)
library(jsonlite)

#Dummy for actually downloading the speeches.
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

speeches_hf_full <- read_csv("Fed Speeches/fed_speeches.csv") 
speeches_hf <- speeches_hf_full %>%
  select(-truncated_cells) %>%
  rename_with(~ sub("^row\\.", "", .x)) %>%
  rename_with(~ sub("^row_", "", .x)) %>%
  select(date, author, country, 
    title_hf = title, text_hf = text, clean_text_hf = clean_text,
    description_hf = description, idx_huggingface = idx, url_hf = url) %>%
  mutate(date = as.Date(date)) 

speeches_kaggle_full <- read_csv("Fed Speeches/fed_speeches_1996_2020.csv") 
speeches_kaggle <- speeches_kaggle_full %>%
  rename(url_kaggle = link) %>%
  mutate(country = "United States",
    date = as.Date(as.character(date), format = "%Y%m%d"),
    author = gsub("Governor |Chairman |Chair |Vice Chair |Vice Chairman |for Supervision |\\.|", "", speaker)) %>%
  select(date, author, country, event_ka = event,
    title_ka = title, text_ka = text) %>%
  arrange(date)

###There's some duplicates in this, but there's no reason to filter exactly. I'll double check each sub sample.
# Row 54 of `x` matches multiple rows in `y`.
# Row 50 of `y` matches multiple rows in `x`.
speeches_final = full_join(speeches_kaggle, speeches_hf, by = c("date", "author", "country")) %>%
  arrange(date) %>% relocate(date, author, title_hf, title_ka, clean_text_hf, text_ka) %>%
  mutate(clean_text_hf = tolower(clean_text_hf), text_ka = tolower(text_ka)) 


write_csv(speeches_final, "Fed Speeches/speeches_final.csv")


price_stability <- speeches_final %>%
  filter(str_detect(clean_text_hf, "price stability") | str_detect(text_ka, "price stability")) %>%
  filter(str_detect(clean_text_hf, "independence") | str_detect(text_ka, "independence")) %>%
  filter(country == "United States")

write_csv(price_stability, "Fed Speeches/price_stability.csv")
  

independence <- speeches_final %>%
  filter(str_detect(clean_text_hf, "independence"))

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