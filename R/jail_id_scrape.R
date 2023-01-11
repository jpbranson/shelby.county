library(tidyverse)
library(httr)
library(rvest)
library(lubridate)


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




for(i in seq_len(pages_num-1)){
  beg <- Sys.time()
  
  next_data = list(
    `flow_action` = 'next',
    `currentStart` = as.character((i*30)+1)
  )
  
  res2 <- httr::POST(url = 'https://imljail.shelbycountytn.gov/IML', httr::add_headers(.headers=headers), httr::set_cookies(.cookies = res_cookies), body = next_data, encode = 'form', user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"))
  
  full_jail_df <- bind_rows(full_jail_df, parse_jail_page(res2, curr_time))
  
  
  end <- Sys.time()
  
  print(paste0(i, " ------- ", as.numeric(end - beg)))
}

full_jail_df <- bind_rows(full_jail_df, parse_jail_page(res, curr_time))

saveRDS(full_jail_df, paste0("data/results_jail_", format(curr_time, "%Y-%m-%d_%H_%M_%S"), ".rds"))