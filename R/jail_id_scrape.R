#library(tidyverse)
library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(lubridate)
library(tidyr)
library(purrr)
library(fst)

set_config( config( ssl_verifypeer = 0L ) )


headers = c(
  `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8',
  `Accept-Language` = 'en-US,en;q=0.8',
  `Cache-Control` = 'no-cache',
  `Connection` = 'keep-alive',
  `Content-Type` = 'application/x-www-form-urlencoded',
  `Origin` = 'https://imljail.shelbycountytn.gov',
  `Pragma` = 'no-cache',
  `Referer` = 'https://imljail.shelbycountytn.gov/IML',
  `Sec-Fetch-Dest` = 'document',
  `Sec-Fetch-Mode` = 'navigate',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-User` = '?1',
  `Sec-GPC` = '1',
  `Upgrade-Insecure-Requests` = '1',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36'
)



inmate_colnames <- c("inmate_name_last_first_middle", "booking_number", "permanent_id", 
                     "date_of_birth", "release_date")


parse_jail_page <- function(res, time){
  inmate_colnames <- c("inmate_name_last_first_middle", "booking_number", "permanent_id", 
                       "date_of_birth", "release_date")
  
  res_html_table <- content(res, "text") %>% read_html() %>% 
    html_elements(css = "table")
  
  
  jail_list <- res_html_table %>%
    .[[9]] %>%
    html_table(header = F)
  
  colnames(jail_list) <- inmate_colnames
  
  result_ids <- res_html_table %>%
    .[[9]]%>%
    html_children() %>%
    html_elements(css = ".underlined") %>%
    html_attr(name = "href") %>%
    str_extract(pattern = "javascript:submitInmate\\(\\'(.*)\\','.*?'\\)", group = 1) 
  
  page_jail_list <- cbind(jail_list, "result_id" = result_ids) %>%
    mutate(timestamp = time)
  
  
  curr_progress_string <- res_html_table %>%
    .[[7]] %>%
    html_children() %>%
    html_children() %>%
    .[[1]] %>%
    html_text(trim = T) %>%
    str_squish()
  
  print(curr_progress_string)
  return(page_jail_list)
}


curr_time <- Sys.time() %>% with_tz("America/Chicago")

full_jail_df <- data.frame(stringsAsFactors = F)


cookies = c(NA_character_
)

data = list(
  `flow_action` = 'searchbyid',
  `quantity` = '10',
  `systemUser_identifiervalue` = '',
  `searchtype` = 'PIN',
  `systemUser_includereleasedinmate` = 'Y',
  `systemUser_includereleasedinmate2` = 'Y',
  `systemUser_firstName` = '',
  `systemUser_lastName` = '',
  `systemUser_dateOfBirth` = '',
  `releasedB` = 'checkbox',
  `identifierbox` = 'PIN',
  `identifier` = ''
)



res <- httr::POST(url = 'https://imljail.shelbycountytn.gov/IML', httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies), body = data, encode = 'form', user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"))

res_cookies <- res$cookies$value
names(res_cookies) <- res$cookies$name


results_num <- content(res, "text") %>% read_html() %>% 
  html_elements(css = "table") %>%
  .[[7]] %>%
  html_children() %>%
  html_children() %>%
  .[[1]] %>%
  html_text(trim = T) %>%
  str_squish() %>%
  str_extract(pattern = "of (.*) results", group = 1) %>% 
  as.numeric()

pages_num <- round(results_num / 30 + .5)




full_jail_df <- map_dfr(.x = seq_len(pages_num-1), .f = function(x){
  beg <- Sys.time()
  
  next_data = list(
    `flow_action` = 'next',
    `currentStart` = as.character((x*30)+1)
  )
  
  res2 <- httr::POST(url = 'https://imljail.shelbycountytn.gov/IML', httr::add_headers(.headers=headers), httr::set_cookies(.cookies = res_cookies), body = next_data, encode = 'form', user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"))
  
  
  
  end <- Sys.time()
  
  print(paste0(x, " ------- ", as.numeric(end - beg)))
  return(parse_jail_page(res2, curr_time))
})

full_jail_df <- bind_rows(full_jail_df, parse_jail_page(res, curr_time))

# saveRDS(full_jail_df, paste0("data/results_jail_", format(curr_time, "%Y-%m-%d_%H_%M_%S"), ".rds"))
write_fst(full_jail_df, path = paste0("data/results_jail_", format(curr_time, "%Y-%m-%d_%H_%M_%S"), ".fst"))

#total_filepath <- list.files(path = "./data", pattern = "total_") %>% paste0("./data/", .)

# total_jail <- readRDS("./data/total_jail.rds") %>%
#   bind_rows(full_jail_df)

# saveRDS(total_jail, "data/total_jail.rds")
# write_fst(total_jail, "data/total_jail.fst", compress = 100)

new_files <- list.files(path = "data/", pattern = "results.*.fst", full.names = T) %>% map_dfr(.f = read_fst)


# readRDS("data/total_jail.rds") %>% fst::write.fst(path = "data/total_jail.fst", compress = 100)

total_jail <- read_fst("data/total_jail.fst") %>%
  bind_rows(new_files)

total_ids <- total_jail%>%
  filter(!is.na(timestamp)) %>%
  group_by(timestamp) %>%
  summarise(value = n_distinct(permanent_id)) %>%
  mutate(name = "n_id_total")

current_pop <- total_jail %>%
  filter(!is.na(timestamp)) %>%
  filter(mdy(release_date) > timestamp |
           is.na(release_date) | release_date == "") %>%
  group_by(timestamp) %>%
  summarise(value = n_distinct(permanent_id)) %>%
  mutate(name = "n_id_current")

all_individuals_touched <- total_jail %>%
  filter(!is.na(timestamp)) %>%
  group_by(permanent_id) %>%
  arrange(permanent_id, timestamp) %>%
  slice(1) %>%
  group_by(timestamp) %>%
  summarise(value = n_distinct(permanent_id)) %>%
  mutate(value = cumsum(value),
         name = "all_individuals")
#is this missing something? right end of line is blank

all_ids_times <- expand.grid(timestamp = total_jail$timestamp %>% unique(), 
                             permanent_id = total_jail$permanent_id %>% unique()) %>%
  filter(!is.na(timestamp))

arrivals_departures <- all_ids_times %>%
  left_join({total_jail %>%
      filter(!is.na(timestamp)) %>%
      filter(mdy(release_date) > timestamp |
               is.na(release_date) | release_date == "") %>%
      distinct(permanent_id, timestamp) %>%
      mutate(present = T)}, by = c("timestamp", "permanent_id")) %>%
  group_by(permanent_id) %>%
  mutate(new_arrival = is.na(lag(present, 1, order_by = timestamp)) & !is.na(present),
         new_departure = !is.na(lag(present, 1, order_by = timestamp)) & is.na(present)) %>%
  group_by(timestamp) %>%
  summarise(arrivals = sum(new_arrival),
            departures = sum(new_departure) * -1) %>%
  pivot_longer(cols = c("arrivals", "departures")) 



metrics_time_df <- bind_rows(total_ids, current_pop) %>%
  bind_rows(all_individuals_touched) %>%
  bind_rows(arrivals_departures)

saveRDS(metrics_time_df, file = "data/metrics_time_df.rds")
write_fst(metrics_time_df, path = "data/metrics_time_df.fst")
