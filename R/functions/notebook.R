library(mRpostman)
library(xml2)
library(rvest)
library(stringr)
library(lubridate)

con <- mRpostman::configure_imap(url = "imaps://imap.gmail.com",
                                 username = "shettysanjay01@gmail.com",
                                 password = "oary bshv ifeu qkze")

con$list_server_capabilities()

con$select_folder("INBOX")

zomato_emails <- con$search_string(expr = "Your Zomato order from",
                                   where = "SUBJECT") |>
  con$fetch_text()


decode_qp <- function(x) {
  x <- gsub("=\r?\n", "", x)
  
  x <- gsub("=([A-Fa-f0-9]{2})", "\\\\x\\1", x)
  
  x <- stringi::stri_unescape_unicode(x)
  x <- iconv(x, from = "UTF-8", to = "UTF-8")
  
  x
}

extract_amount <- function(x) {
  as.numeric(stringr::str_extract(x, "\\d+(?:\\.\\d+)?"))
}


g <- decode_qp(zomato_emails$text10025) %>%
  rvest::read_html()
  

amount_text <- g |>
  html_elements(xpath = "//div[contains(text(),'Paid')]") |>
  html_text()



amount_text <- g |>
  html_elements(xpath = "//div[contains(normalize-space(.), 'Paid') and
    (contains(., '0') or contains(., '1') or contains(., '2') or
     contains(., '3') or contains(., '4') or contains(., '5') or
     contains(., '6') or contains(., '7') or contains(., '8') or
     contains(., '9'))]") |>
  html_text(trim = TRUE)

amount <- extract_amount(amount_text)|>
  as.numeric()

  amount <- g |>
  html_elements(xpath = "//div[contains(normalize-space(.), 'Paid')]") |>
  html_text(trim = TRUE) |>
  str_extract("₹\\s*\\d+") |>
  str_remove("₹") |>
  as.numeric()
