# Add interaction terms for lasso and ridge reg

add_interaction <- function(df) {
  df <- df %>%
    mutate(o_swing_miss = o_swing * o_miss, z_swing_contact = z_swing * z_contact,
           o_swing_miss_non_zone = o_swing * o_miss * non_zone,
           z_swing_contact_zone = z_swing * z_contact * zone) 
  
}