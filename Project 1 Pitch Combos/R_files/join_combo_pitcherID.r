# Joins pitch by pitch data with combo data into one table. Every pitch will have the expected xwoba in its row.
join_combo_pitcherID <- function(id, combo)
{
  inner_join(id, combo, by = c("pitch_type", "zone", "p_side", "b_side"))
}