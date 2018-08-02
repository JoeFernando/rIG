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
                  httr::add_headers(`VERSION` = "2",
                              `X-IG-API-KEY` = api.key),
                  httr::content_type_json(),
                  body = body.input,
                  encode = "json")

  if(r$status_code == 200){print("Login is good")} else {(print("BAD LOGIN - PLEASE CHECK LOGIN DETAILS"))}

  httr::stop_for_status(r)
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
                    httr::add_headers(`VERSION` = "1",
                                `X-IG-API-KEY` = api_key,
                                `CST` = cst,
                                `X-SECURITY-TOKEN` = token),
                    httr::content_type_json())

  if(r$status_code == 204){print("Logout successful")} else {(print("BAD LOGOUT - PLEASE CHECK LOGIN DETAILS"))}

  httr::stop_for_status(r)
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

  from <- stringr::str_replace_all(from, ":", "%3A")
  to   <- stringr::str_replace_all(to, ":", "%3A")

  first_link <- "https://demo-api.ig.com/gateway/deal/prices/"
  http_link <- glue::glue(first_link, epic, "?resolution=", res, "&from=", from, "&to=", to, "&max=", qty, "&pageSize=100000&pageNumber=1")




  dl <- httr::GET(http_link,
                  httr::add_headers(`VERSION` = "3",
                             `X-IG-API-KEY` = api_key,
                             `CST` = cst,
                             `X-SECURITY-TOKEN` = token),
                 httr::content_type_json())

  dl_1 <- httr::content(dl, "parsed", "application/json", encoding="UTF-8")

  allowance <<- dl_1$metadata$allowance %>% dplyr::bind_rows()

  dl_2 <- dl_1[["prices"]]


  dl_3 <- purrr::map_df(dl_2, ~extract_prices_from_list(.x)) %>% format_hist

  attr(dl_3, "name") <- mkt.name

  return(dl_3)


}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract price history search results ####
# There are two functions that work together to drag out the results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

extract_prices <- function(type, list.of.prices){

  requireNamespace("dplyr")

  x <- list.of.prices[type] %>% rlist::list.rbind() %>% as.character() %>% t %>% tibble::as.tibble()

  bid        <- glue::glue(type, "_bid")
  ask        <- glue::glue(type, "_ask")
  lasttraded <- glue::glue(type, "_lasttraded")

  y <- dplyr::rename(x,  !!bid := V1,
                     !!ask := V2,
                     !!lasttraded := V3)

  return(y)
}



extract_prices_from_list <- function(one.list.of.prices){


  price_data <- purrr::map_dfc(c("openPrice", "highPrice", "lowPrice", "closePrice"), ~extract_prices(.x, one.list.of.prices))

  date_1 <- one.list.of.prices[1] #%>% tibble::as.tibble
  date_2 <- one.list.of.prices[2] #%>% tibble::as.tibble
  vol    <- one.list.of.prices[7] #%>% tibble::as.tibble

  clean_price_data <- dplyr::bind_cols(date_1, date_2, price_data, vol)

  return(clean_price_data)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to format price history ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

format_hist <- function(df){

  df %>%
    dplyr::mutate(snapshotTime = lubridate::ymd_hms(snapshotTime)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("Price"), dplyr::contains("Volume")), as.numeric)


}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to search for markets ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_search <- function(search_string){

  search_string <- stringr::str_replace_all(search_string, " ", "%20")
  link <- glue::glue("https://demo-api.ig.com/gateway/deal/markets?searchTerm=", search_string)

  search_markets <- httr::GET(link,
                              httr::add_headers(`VERSION` = "1",
                                          `X-IG-API-KEY` = api_key,
                                          `CST` = cst,
                                          `X-SECURITY-TOKEN` = token),
                              httr::content_type_json())

  parsed_output <- httr::parsed_content(search_markets)
  market_names  <- parsed_output[["markets"]]

  no_of_lists <- length(market_names)
  output <- purrr::map_df(1:no_of_lists, ~get_markets(.x, market_names))

  return(output)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract search results ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_markets <- function(x, dl.list = market_names){

  # dl.list[[x]] %>% bind_rows()   # this does not work due to a NULL result in one of the values
  rlist::list.rbind(dl.list[[x]]) %>% t %>% tibble::as.tibble()

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to extract search results in OHLCV format ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_ohlc <- function(x, bid.ask = "ask"){

  requireNamespace("dplyr")

  y <- x %>%
    dplyr::select("snapshotTime", dplyr::contains(bid.ask), "lastTradedVolume") %>%
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Update eod date in a list ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_eod_list <- function(mkt.name,
                        to.date = (Sys.Date() + lubridate::ddays(1)),
                        list.of.df,
                        epic.table,
                        epic.code.col_name = "epic",
                        epic.name.col.name = "instrumentName"){

  # find epic code
  epic_code <- epic.table %>%
    filter(!!rlang::sym(epic.name.col.name) == mkt.name) %>%
    select(!!rlang::sym(epic.code.col_name)) %>%
    as.character()


  # find start date
  last_date  <- max(list.of.df[[mkt.name]]$snapshotTime)
  start_date <- (last_date - lubridate::ddays(1)) %>% as.Date %>% as.character

  dl_data <- ig_history(epic = epic_code, mkt.name = mkt.name,
                        from = start_date, to = to.date, res = "DAY")

  all_data <- bind_rows(list.of.df[[mkt.name]] %>% filter(snapshotTime < last_date), dl_data) %>% unique

  attr(all_data, "name") <- mkt.name

  return(all_data)


}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Update eod dategiven a dataframe ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_eod <- function(mkt.name = "",
                   to.date = (Sys.Date() + lubridate::ddays(1)),
                   df.w.hist,
                   epic.code){

  # find start date
  last_date  <- max(df.w.hist$snapshotTime)
  start_date <- (last_date - lubridate::ddays(1)) %>% as.Date %>% as.character

  dl_data <- ig_history(epic = epic.code, mkt.name = mkt.name,
                        from = start_date, to = to.date, res = "DAY", qty = 1000)

  all_data <- bind_rows(df.w.hist %>% filter(snapshotTime < last_date), dl_data) %>% unique

  attr(all_data, "name") <- mkt.name

  return(all_data)


}




