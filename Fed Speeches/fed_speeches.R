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

#Dummy for reproducing the full speeches CSV
full_speeches_csv = F

if(download_speeches){

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

if(full_speeches_csv){
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
  rename(url_ka = link) %>%
  mutate(country = "United States",
    date = as.Date(as.character(date), format = "%Y%m%d"),
    author = gsub("Governor |Chairman |Chair |Vice Chair |Vice Chairman |for Supervision |\\.|", "", speaker)) %>%
  select(date, author, country, event_ka = event,
    title_ka = title, text_ka = text, url_ka) %>%
  arrange(date)

###There's some duplicates in this, but there's no reason to filter exactly. I'll double check each sub sample.
# Row 54 of `x` matches multiple rows in `y`.
# Row 50 of `y` matches multiple rows in `x`.
speeches_final = full_join(speeches_kaggle, speeches_hf, by = c("date", "author", "country")) %>%
  arrange(date)  %>%
  mutate(clean_text_hf = tolower(clean_text_hf), text_hf = tolower(text_hf),
    text_ka = tolower(text_ka)) 


# --- split long text columns for Excel (32,767 char/cell limit) ---
limit <- 32000L  # safety margin

split_into_chunks <- function(x, size = limit) {
  x <- if (is.na(x)) "" else x
  n <- nchar(x)
  if (n == 0) return("")
  starts <- seq(1L, n, by = size)
  substring(x, starts, pmin(starts + size - 1L, n))
}

split_column <- function(df, col) {
  vals <- df[[col]]
  max_chunks <- max(1L, purrr::map_int(vals, ~ length(split_into_chunks(.x))))

  chunk_cols <- purrr::map(vals, split_into_chunks) %>%
    purrr::map(~ { length(.x) <- max_chunks; .x }) %>%
    purrr::transpose() %>%
    purrr::map(~ purrr::map_chr(.x, ~ if (is.null(.x)) NA_character_ else .x))

  col_names <- c(col, paste0(col, 2:max_chunks))
  df[[col]] <- NULL
  for (i in seq_len(max_chunks)) df[[col_names[i]]] <- chunk_cols[[i]]

  # keep the new columns where the original was
  dplyr::relocate(df, dplyr::all_of(col_names))
}

speeches_final <- speeches_final %>%
  split_column("clean_text_hf") %>%
  split_column("text_hf") %>%
  split_column("text_ka") %>% relocate(date, author, title_hf, title_ka, contains("clean_text_hf"), 
    contains("text_ka"))
# --- end split ---


write_csv(speeches_final, "Fed Speeches/speeches_final.csv")
}

speeches_final <- read_csv("Fed Speeches/speeches_final.csv") 



speeches_usa <- speeches_final %>%
  filter(country == "United States") %>%
  mutate(
    cukierman = if_else(if_any(c(starts_with("clean_text_hf"),
          starts_with("text_hf"), starts_with("text_ka")),
                ~ str_detect(coalesce(., ""), "cukierman")), T, F),
    rogoff = if_else(if_any(c(starts_with("clean_text_hf"),
          starts_with("text_hf"), starts_with("text_ka")),
                ~ str_detect(coalesce(., ""), "rogoff")), T, F),
    posen = if_else(if_any(c(starts_with("clean_text_hf"),
          starts_with("text_hf"), starts_with("text_ka")),
                ~ str_detect(coalesce(., ""), "posen")), T, F),
    alesina = if_else(if_any(c(starts_with("clean_text_hf"),
          starts_with("text_hf"), starts_with("text_ka")),
                ~ str_detect(coalesce(., ""), "alesina")), T, F),
    kydland = if_else(if_any(c(starts_with("clean_text_hf"),
          starts_with("text_hf"), starts_with("text_ka")),
                ~ str_detect(coalesce(., ""), "kydland")), T, F)
  ) %>%
  arrange(-posen, date)

write_csv(speeches_usa, "Fed Speeches/speeches_usa.csv")

paper_speeches <- speeches_usa %>%
  filter(cukierman|rogoff|posen|alesina|kydland)

write_csv(paper_speeches, "Fed Speeches/paper_speeches.csv")

# independence <- speeches_final %>%
#   filter(if_any(c(starts_with("clean_text_hf"), starts_with("text_ka")),
#                 ~ str_detect(., "independence")))  %>%
#   filter(country == "United States")
# dual_mandate <- speeches_final %>%
#   filter(if_any(c(starts_with("clean_text_hf"), starts_with("text_ka")),
#                 ~ str_detect(., "dual mandate")))  %>%
#   filter(country == "United States")

# write_csv(independence, "Fed Speeches/independence.csv")
# write_csv(dual_mandate, "Fed Speeches/dual_mandate.csv")

# price_stability <- speeches_final %>%
#   filter(if_any(c(starts_with("clean_text_hf"), starts_with("text_ka")),
#                 ~ str_detect(., "price stability"))) %>%
#   filter(if_any(c(starts_with("clean_text_hf"), starts_with("text_ka")),
#                 ~ str_detect(., "independence"))) %>%
#   filter(country == "United States")

# write_csv(price_stability, "Fed Speeches/price_stability.csv")