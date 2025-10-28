# strava_auth.R

# client_id:159269
# client_secret:d4c0bdc4848cda0194dd97802c1cbed75b80fbb5
# access_token:d369047b147afba82576f0493dc6395c767a2f2b
# refresh_token:f6c312b1d07c195823993005f59bc47b8ebf770a

app_name <- "a string"

app_client_id <- #an integer
app_secret <- "a long hash"

  stoken <- httr::config(token = strava_oauth(app_name,
                                              app_client_id,
                                              app_secret,
                                              app_scope="activity:read_all",
                                              cache = TRUE))