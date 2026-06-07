rm(list = ls())
gc()

library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)

raw <- fromJSON("~/Dissertation/General/binder.json", simplifyVector = FALSE)

names(raw)


# Scalar metadata (one row)
meta <- tibble(
  identifier      = raw$identifier$identifier,
  identifierType  = raw$identifier$identifierType,
  publicationYear = raw$publicationYear,
  publisher       = raw$publisher,
  resourceType    = raw$resourceType$resourceType,
  resourceGeneral = raw$resourceType$resourceTypeGeneral,
  title           = raw$titles[[1]]$title
)

# Nested fields (one row per entry)
creators     <- raw$creators     %>% map(\(x) as_tibble(compact(x))) %>% list_rbind()
contributors <- raw$contributors %>% map(\(x) as_tibble(compact(x))) %>% list_rbind()
dates        <- raw$dates        %>% map(\(x) as_tibble(compact(x))) %>% list_rbind()

meta
creators
contributors
dates