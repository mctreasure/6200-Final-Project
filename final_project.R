# library(httr)
# library(janitor)
# library(jsonlite)
# library(pdftools)
# library(lubridate)
# library(purrr)
# library(rvest)
# library(tesseract)
# library(tidyverse)
# library(tinytable)
# library(usethis)
# library(xml2)
# library(knitr)


website_links = c("https://www.metro.ca/en/online-grocery/aisles/fruits-vegetables",
                      "https://www.zehrs.ca/food/fruits-vegetables/c/28000?navid=flyout-L2-fruits-vegetables",
                      "https://www.walmart.ca/en/browse/grocery/fruits-vegetables/10019_6000194327370?icid=cp_l2_page_grocery_produce_shop_all_89673_FOPA0AUQJ3")

# walmart_fv_category = website_links[3]
# 
# walmart_fv_category
# 
# fv_walmart = read_html(walmart_fv_category)
# fv_walmart
