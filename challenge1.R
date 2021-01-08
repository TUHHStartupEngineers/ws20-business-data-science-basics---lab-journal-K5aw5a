# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

# Loading all Data Tables

#patent_tbl

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl%>% glimpse()

#assignee_tbl

col_types <- list(
  id = col_character(),
  type= col_double(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
  
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

assignee_tbl%>%glimpse()

#patent_assignee_tbl

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
  
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl%>%glimpse()

#uspc_tbl

col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_double()
  
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

uspc_tbl%>%glimpse()

# converting tables to data.table

names(assignee_tbl)[names(assignee_tbl)=="id"] <- "assignee_id"

class(patent_tbl)
setDT(patent_tbl)

class(assignee_tbl)
setDT(assignee_tbl)

class(patent_assignee_tbl)
setDT(patent_assignee_tbl)

class(uspc_tbl)
setDT(uspc_tbl)

patent_assignee_tbl%>%glimpse()
assignee_tbl%>%glimpse()

# ----  Question 1 : Patent Dominance

#merging assignee_tbl and patent_assignee_tbl
tic()
combined_assignee <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_assignee %>% glimpse()

#setting the default key and then ordering the data
setkey(combined_assignee, "assignee_id")
key(combined_assignee)

?setorder()
setorderv(combined_assignee, c("assignee_id","type","organization"))

# 10 US Companies 

tic()

combined_assignee %>% 
  filter(type == 2)%>%
  filter(!is.na(patent_id)) %>%
  group_by(organization)%>%
  count(organization, sort = T)%>%
  glimpse(, eval=TURE)
toc()

