# Used in statcast_db.r to scrape data from Baseball Savant.
scrape_statcast_savant_pitcher_date <- function(start_date, end_date) {
  
  # extract year
  year <- substr(start_date, 1,4)
  
  # Base URL.
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=",year,"%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
  
  payload <- utils::read.csv(url)
  
  if ("fielder_2" %in% names(payload)) {
    
    payload$pitcher   <- as.character(payload$pitcher)   %>% as.integer
    payload$fielder_2 <- as.character(payload$fielder_2) %>% as.integer
    payload$fielder_3 <- as.character(payload$fielder_3) %>% as.integer
    payload$fielder_4 <- as.character(payload$fielder_4) %>% as.integer
    payload$fielder_5 <- as.character(payload$fielder_5) %>% as.integer
    payload$fielder_6 <- as.character(payload$fielder_6) %>% as.integer
    payload$fielder_7 <- as.character(payload$fielder_7) %>% as.integer
    payload$fielder_8 <- as.character(payload$fielder_8) %>% as.integer
    payload$fielder_9 <- as.character(payload$fielder_9) %>% as.integer
  }
  if (("fielder_2.1" %in% names(payload))) {
    payload$fielder_2.1 <- as.character(payload$fielder_2) %>% as.integer
  }
  if (length(payload$pitch_type) > 0) {
    
    # Clean up formatting.
    payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
    payload$des <- as.character(payload$des)
    payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
    payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
    payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
    payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
    payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
    payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
    payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
    payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
    payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
    payload$woba_denom <- as.character(payload$woba_denom) %>% as.numeric()
    payload$woba_value <- as.character(payload$woba_value) %>% as.numeric()
    payload$babip_value <- as.character(payload$babip_value) %>% as.numeric()
    payload$iso_value <- as.character(payload$iso_value) %>% as.numeric()
    payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
    payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
    payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
    payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
    payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
    payload$ax <- as.character(payload$ax) %>% as.numeric()
    payload$ay <- as.character(payload$ay) %>% as.numeric()
    payload$az <- as.character(payload$az) %>% as.numeric()
    payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
    payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
    payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
    payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
    payload$launch_speed_angle <- as.character(payload$launch_speed_angle) %>% as.numeric()
    payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
    payload$estimated_ba_using_speedangle <- as.character(payload$estimated_ba_using_speedangle) %>% as.numeric()
    payload$estimated_woba_using_speedangle <- as.character(payload$estimated_woba_using_speedangle) %>% as.numeric()
    payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
    payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
    payload$zone <- as.character(payload$zone) %>% as.numeric()
    payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
    payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
    payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    payload$home_team <- as.character(payload$home_team)
    payload$away_team <- as.character(payload$away_team)
    
    return(payload)
  }
  
  else {
    vars <- names(payload)
    df <- lapply(vars, function(x) x <- NA)
    names(df) <- names(payload)
    payload_na <- bind_rows(df)
    
    return(payload_na)
    
    Sys.sleep(sample(x = runif(20, min = .01, max = 1), size = 1))
  }
}