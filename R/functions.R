#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Time resolutions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

time_resolution <- c("SECOND", "MINUTE", "MINUTE_2", "MINUTE_3", "MINUTE_5", "MINUTE_10", 
              "MINUTE_15", "MINUTE_30", "HOUR", "HOUR_2", "HOUR_3", "HOUR_4", 
              "DAY", "WEEK", "MONTH")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to authenticate ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_login <- function(body.input, api.key = ""){
  
  r <- httr::POST("https://demo-api.ig.com/gateway/deal/session",
                  add_headers(`VERSION` = "2",
                              `X-IG-API-KEY` = api.key),
                  content_type_json(),
                  body = body.input,
                  encode = "json")
  
  if(r$status_code == 200){print("Login is good")} else {(print("BAD LOGIN - PLEASE CHECK LOGIN DETAILS"))}
  
  stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding="UTF-8")
  
  # save access tokens
  cst     <<- r$headers$cst
  token   <<- r$headers$`x-security-token`
  api_key <<-api.key
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to logout ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_logout <- function(){
  
  r <- httr::DELETE("https://demo-api.ig.com/gateway/deal/session",
                    add_headers(`VERSION` = "1",
                                `X-IG-API-KEY` = api_key,
                                `CST` = cst,
                                `X-SECURITY-TOKEN` = token),
                    content_type_json())
  
  if(r$status_code == 204){print("Logout successful")} else {(print("BAD LOGOUT - PLEASE CHECK LOGIN DETAILS"))}
  
  stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding="UTF-8")
  
  if(r$status_code == 204){
  # clear tokens
  cst     <<- 0
  token   <<- 0
  api_key <<- 0}
  
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to download history ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_history <- function(epic, mkt.name = "", from, to, res, qty = 10){
  
  from <- str_replace_all(from, ":", "%3A")
  to   <- str_replace_all(to, ":", "%3A")
  
  first_link <- "https://demo-api.ig.com/gateway/deal/prices/"
  http_link <- glue(first_link, epic, "?resolution=", res, "&from=", from, "&to=", to, "&max=", qty, "&pageSize=100000&pageNumber=1") 
                 
  
  

  dl <- httr::GET(http_link,
                 add_headers(`VERSION` = "3",
                             `X-IG-API-KEY` = api_key,
                             `CST` = cst,
                             `X-SECURITY-TOKEN` = token),
                 content_type_json())

  dl_1 <- httr::content(dl, "parsed", "application/json", encoding="UTF-8")
  
  
  dl_2 <- dl_1[["prices"]]
  
  
  dl_3 <- map_df(dl_2, ~extract_prices_from_list(.x)) %>% format_hist
  
  attr(dl_3, "name") <- mkt.name
  
  return(dl_3)
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract price history search results ####
# There are two functions that work together to drag out the results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

extract_prices <- function(type, list.of.prices){
  
  x <- list.of.prices[type] %>% list.rbind() %>% as.character() %>% t %>% as.tibble()
  
  bid        <- glue::glue(type, "_bid")
  ask        <- glue::glue(type, "_ask")
  lasttraded <- glue::glue(type, "_lasttraded")
  
  y <- dplyr::rename(x,  !!bid := V1,
                     !!ask := V2,
                     !!lasttraded := V3)
  
  return(y)
}



extract_prices_from_list <- function(one.list.of.prices){
  
  
  price_data <- map_dfc(c("openPrice", "highPrice", "lowPrice", "closePrice"), ~extract_prices(.x, one.list.of.prices)) 
  
  date_1 <- one.list.of.prices[1] %>% as.tibble
  date_2 <- one.list.of.prices[2] %>% as.tibble
  vol    <- one.list.of.prices[7] %>% as.tibble
  
  clean_price_data <- bind_cols(date_1, date_2, price_data, vol) 
  
  return(clean_price_data)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to format price history ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

format_hist <- function(df){
  
  df %>%
    mutate(snapshotTime = ymd_hms(snapshotTime)) %>%
    mutate_at(vars(contains("Price"), contains("Volume")), as.numeric)
    

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to search for markets ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_search <- function(search_string){
  
  search_string <- str_replace(search_string, " ", "%20")
  link <- glue("https://demo-api.ig.com/gateway/deal/markets?searchTerm=", search_string)
  
  search_markets <- httr::GET(link,
                              add_headers(`VERSION` = "1",
                                          `X-IG-API-KEY` = api_key,
                                          `CST` = cst,
                                          `X-SECURITY-TOKEN` = token),
                              content_type_json())
  
  parsed_output <- parsed_content(search_markets)
  market_names  <- parsed_output[["markets"]]
  
  no_of_lists <- length(market_names)
  output <- map_df(1:no_of_lists, ~get_markets(.x, market_names))
  
  return(output) 
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract search results ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_markets <- function(x, dl.list = market_names){
  
  # dl.list[[x]] %>% bind_rows()   # this does not work due to a NULL result in one of the values
  rlist::list.rbind(dl.list[[x]]) %>% t %>% as.tibble
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract search results in OHLCV format ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_ohlc <- function(x, bid.ask = "ask"){
  
  y <- x %>%
    select("snapshotTime", contains(bid.ask), "lastTradedVolume") %>%
    stats::setNames(., c("Date", "Open", "High", "Low", "Close", "Volume"))
  
  return(y)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to rename ohlcv with a prefix taken from attributes name ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

add_prefix <- function(ohlc.df) {
  
  pre_fix <- attr(ohlc.df, "name") %>% clean_out_names()  
  
  new_names <- stats::setNames(ohlc.df, 
                               c("Date", paste0(pre_fix, "_", c("Open", "High", "Low", "Close", "Volume"))))
  return(new_names)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# consolidated Function to extract OHLCV and rename with prefix taken from attributes name ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_ohlc_rename <- function(x, bid.ask = "ask"){
  
  get_ohlc(x) %>%
    add_prefix(.)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to clean out special characters ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


clean_out_names <- function(text.input){
  
  text.input %>% 
    gsub("'", "", .) %>% 
    gsub("\"", "", .) %>% 
    gsub("%", "percent", .) %>% 
    gsub("^[ ]+", "", .) %>% 
    make.names(.) %>% 
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>% 
    # tolower(.) %>%
    gsub("_$", "", .)
  
  
}


