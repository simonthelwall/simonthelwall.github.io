# data manipulation functions

get_strava_data <- function(){
  z <- get_activity_list(stoken, after = dmy("01-01-2015"))
  z <- compile_activities(z)
  return(z)
}

clean_strava_data <- function(x){
  x <- x %>% 
    select(average_speed, distance, elapsed_time, max_speed, moving_time, 
           trainer, 
           sport_type, start_date_local, total_elevation_gain, average_cadence, 
           average_heartrate, max_heartrate, pr_count) %>% 
    mutate(
      distance_type = case_when(
        between(distance, 4.5, 5.5) & trainer == FALSE ~ "5k", 
        between(distance, 9.5, 11) & trainer == FALSE ~ "10k", 
        TRUE ~ "Other"
      ), 
      distance_type = factor(distance_type, levels = c("5k", "10k", "Other")),
      dt = as.Date(start_date_local, format = "%Y-%m-%d"),
      tm = strptime(start_date_local, format = "%Y-%m-%dT%H:%M:%SZ"), 
      yr = year(start_date_local), 
      day_of_year = strftime(dt, format = "%j")
    ) %>% 
    filter(!(moving_time == 1200 & max_speed == 0))
  return(x)
}

get_runs <- function(x){
  x <- filter(x, sport_type == "Run")
  return(x)
}

cumulate_runs <- function(x){
  cum_runs <- x %>% 
    arrange(yr, day_of_year) %>% 
    group_by(yr) %>% 
    mutate(cum_dist = cumsum(distance)) %>% 
    select(yr, day_of_year, cum_dist) %>% 
    mutate(yr_lbl = if_else(row_number() == max(row_number()), yr, NA_real_)) %>% 
    ungroup() %>% 
    mutate(day_of_year = as.numeric(day_of_year), 
           line_clr = case_when(
             yr == max(yr) ~ "#E63946", 
             between(yr, left = max(yr) - 6, right = max(yr)-1) ~ "#457B9D", 
             TRUE ~ "#A8DADC"
           )
           # , yr = as.factor(yr)
    )
  return(cum_runs)
}

create_perftab <- function(x){
  z <- x %>%
    group_by(yr, distance_type) %>% 
    summarise(mn_time_mins = mean(elapsed_time) / 60, n_runs = n()) %>% 
    arrange(distance_type, yr) %>% 
    pivot_wider(id_cols = yr, names_from = distance_type, 
                values_from = c(mn_time_mins, n_runs)) %>% 
    arrange(desc(yr))
    # mean_runs
    
    last_5 <- x %>% filter(yr == max(yr)) %>% 
      group_by(yr, distance_type) %>% 
      filter(between(row_number(), left = max(row_number()) - 5, 
                     right = max(row_number()))) %>% 
      summarise(mn_time_mins = mean(elapsed_time) / 60) %>% 
      mutate(yr = "last 5 runs", n_runs = 5) %>% 
      pivot_wider(id_cols = yr, names_from = distance_type, 
                  values_from = c(mn_time_mins, n_runs)) %>% 
      ungroup()
    
    z <- last_5 %>% bind_rows(., mutate(z, yr = as.character(yr)))
    
    z <- z %>% 
      mutate(across(starts_with("n_runs"), as.integer)) %>% 
      rename(Year = yr, 'Mean 5k time (mins)' = mn_time_mins_5k, 
             'Mean 10k time (mins)' = mn_time_mins_10k, 
             'Mean time - Other (mins)' = mn_time_mins_Other,
             'n runs 5k' = n_runs_5k, 'n runs 10k' = n_runs_10k, 
             'n runs - Other' = n_runs_Other)
}