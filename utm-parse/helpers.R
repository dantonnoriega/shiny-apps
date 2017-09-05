# helpers

get_utm <- function(f) {
  
  parse_utm <- function(x) {
    # empty frame
    ef <- structure(list(cf_affiliate_id = character(1), utm_source = character(1), utm_medium = character(1), utm_campaign = character(1), utm_term = character(1), utm_content = character(1), webinar_delay = character(1), purchase_product_ids = character(1), purchase_taxamo_transaction_key = character(1), purchase_payment_method_nonce = character(1), purchase_stripe_customer_token = character(1), upsell = character(1), purchase_product_id = character(1), purchase_stripe_customer_id = character(1)), .Names = c("cf_affiliate_id", "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content", "webinar_delay", "purchase_product_ids", "purchase_taxamo_transaction_key", "purchase_payment_method_nonce", "purchase_stripe_customer_token", "upsell", "purchase_product_id", "purchase_stripe_customer_id"), row.names = integer(1), class = c("tbl_df", "tbl", "data.frame"))
    
    if(is.na(x)) return(ef)
    
    y <- rev(gregexpr(",[ ]+:[^\\>]*\\>\\{", x, perl = T)[[1]])[1]
    z <- gregexpr("\\}", x, perl = T)[[1]][1]
    
    # purchase
    a <- trimws(substring(x, y + 1, z))
    a <- substring(a, gregexpr("\\{", a, perl = T)[[1]][1], gregexpr("\\}", a, perl = T)[[1]][1])
    
    # outer
    l <- substring(x, 1, y - 1)
    r <- substring(x, z + 1, nchar(x))
    b <- paste0(l, r)
    
    # skip fail is cool
    # https://stackoverflow.com/questions/35347537/using-strsplit-in-r-ignoring-anything-in-parentheses
    parse_vec <- function(x) {
      x %>%
        gsub('\\"', "'", .) %>%
        strsplit(., '\\[[^\\]]+,(*SKIP)(*FAIL)|,\\s*', perl=TRUE) %>%
        '[['(1) %>%
        lapply(. %>% strsplit('>') %>% '[['(1)) %>%
        lapply(., gsub, pattern = "[':{}]", replacement = "") %>%
        do.call('rbind', .) %>%
        as.data.frame()
    }
    
    a <- parse_vec(a) %>%
      dplyr::mutate(V2 = replace(V2, V2 == "", NA_character_)) %>%
      tibble::column_to_rownames('V1') %>%
      t() %>%
      tibble::as_tibble() %>%
      setNames(paste0('purchase_', names(.)))
    b <- parse_vec(b) %>%
      dplyr::mutate(V2 = replace(V2, V2 == "", NA_character_)) %>%
      tibble::column_to_rownames('V1') %>%
      t() %>%
      tibble::as_tibble()
    
    dplyr::bind_cols(b, a)
    
  }
  
  # PARSE AND CLEAN DATA ------------------------------------------------
  # get data
  v <- readr::read_csv(f)
  
  nms <- names(v) %>%
    gsub("[ ]", "_", .) %>%
    tolower()
  
  v <- setNames(v, nms)
  
  # parse additional info
  p <- v[['additional_info']]
  
  k <- p %>%
    lapply(parse_utm) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(purchase_product_ids = gsub("\\[|\\]", "", purchase_product_ids)) %>%
    tidyr::separate(purchase_product_ids, c('purchase_product_id1', 'purchase_product_id2')) %>%
    dplyr::mutate_at(dplyr::vars(-purchase_stripe_customer_id), dplyr::funs(tolower(.))) %>%
    dplyr::mutate_all(dplyr::funs(replace(., . == "", NA)))
  
  p <- p %>%
    lapply(. %>%
             gsub(pattern = '\\"', replacement = "'", .) %>%
             '[['(1))
  
  # drop additional_info and bind the parse utm vals
  q <- v %>%
    dplyr::select(-additional_info) %>%
    dplyr::bind_cols(., k)
  
  # export
  return(q)
  
}