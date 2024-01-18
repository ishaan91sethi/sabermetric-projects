# Adds two columns in the combo dataframes with the handedness of the batter and pitcher.
add_bat_pitch_sides <- function(df, p_side, b_side)
{
  cbind(df, p_side = rep(p_side, nrow(df)), b_side = rep(b_side, nrow(df)))
}