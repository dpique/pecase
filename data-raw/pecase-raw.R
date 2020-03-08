## code to prepare `pecase` dataset goes here
# PECASE = Presidential Early Career Awards for Scientists and Engineers

library(tidyverse)
library(rvest)
library(here)
library(janitor)

w <- read_html("wiki_pecase.htm")

## Parsing the wikipedia entry on PECASE
## 1. Largest category - years
w.categ_names <- w %>% html_nodes(css = "h3 + p + div") %>% .[-1]
w.years <- w %>% html_nodes(css = "div + h3 > .mw-headline")

## 2. Subcategory - each year
w.2015.prior <- w %>% html_nodes(css = "#mw-content-text > div > div:nth-child(16)")
w.2015.prior.id <- w.2015.prior %>% html_nodes("h4") %>% html_node("span") %>% html_attr("id") ##%>% html_attrs()
w.2015.prior.nodes <- w.2015.prior %>% html_nodes("ul") %>% do.call(list, .) %>% lapply(function(x) html_nodes(x, "li") %>% html_text) %>% set_names(w.2015.prior.id) #html_nodes("")

## 3. Do this for each year
get_id <- function(categ_names){
  categ_id <- categ_names %>% html_nodes("h4") %>% html_node("span") %>% html_attr("id")
  categ_name <- categ_names %>% html_nodes("h4") %>% html_node("span") %>% html_text()
  result <- sapply(1:length(categ_name), function(x) ifelse(categ_name[x] == "", categ_id[x], categ_name[x]))
  return(result)
}

get_nodes <- function(get_id_res, categ_names){
  categ_names %>% html_nodes("ul") %>% do.call(list, .) %>% lapply(function(x) html_nodes(x, "li") %>% html_text) %>% set_names(get_id_res)
}

res <- lapply(w.categ_names, function(x) get_id(x) %>% get_nodes(x)) %>% set_names( html_text(w.years)) #%>% plyr::ldply()

unnest_list_to_df_with_yr <- function(x){
  tibble(name_instit = x[[1]]) %>% mutate(year = names(x), dept = names(x[[1]])) %>% unnest()
}

res_df <- lapply(1:length(res), function(x) unnest_list_to_df_with_yr(res[x])) %>%
  plyr::ldply() %>%
  as_tibble() %>%
  separate(name_instit, into = c("name", "instit"), sep = ",", extra = "merge") %>%
  mutate_all(str_trim)

#clean up the df
res_df2 <- res_df %>% mutate(dept = ifelse(grepl("National_Institutes_of_Health", dept), "Department of Health and Human Services", dept),
                             dept = ifelse(grepl("National Institutes of Health", dept, fixed = TRUE), "Department of Health and Human Services", dept),
                             dept = ifelse(grepl("Department_of_Veterans", dept), "Department of Veterans Affairs", dept))


w.agencies <- w %>% html_nodes("#mw-content-text > div > ul:nth-child(9)") %>% html_nodes("li") %>% html_text()
w.2015 <- w %>% html_nodes(css = "#mw-content-text > div > div:nth-child(13)") #%>% html_text()

w.2015.2 <- w.2015 %>%
  html_nodes("p") %>%
  html_text() %>%
  tibble("col1" = .) %>%
  separate(col = "col1", into = paste0("col", 1:5), sep = ",") %>%
  mutate_all(str_trim) %>%
  mutate(dept = coalesce(col5, col4, col3))

w.2015.3 <- w.2015.2 %>%
  select(col1, col2, dept) %>%
  rename(name = col1, instit = col2) %>%
  mutate(year = "2015") %>%
  select(year, dept, name, instit)

pecase <- bind_rows(w.2015.3, res_df2) %>% mutate(year=as.numeric(year))
#this is the final df


usethis::use_data(pecase, overwrite = TRUE)
