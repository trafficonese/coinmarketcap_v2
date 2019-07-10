# usethis::use_data_raw()

pro_url <- "pro-api.coinmarketcap.com"
usethis::use_data(pro_url, overwrite = T)

test_url <- "sandbox-api.coinmarketcap.com"
usethis::use_data(test_url, overwrite = T)
