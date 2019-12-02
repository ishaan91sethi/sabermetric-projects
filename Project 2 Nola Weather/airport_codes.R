# Anaheim - FUL, Atlanta - MGE for Suntrust and Turner (approximation), Boston - BOS, Chicago - CGX (only Cubs),
# Cincinnati-LUK, Denver - BKF, Detroit - DET, Los Angeles - FUL, Miami - MIA, Milwaukee - MKE, 
# Minneapolis - MSP, New York - LGA, Philadelphia - PHL, Phoenix - PHX, Pittsburgh - PIT, San Diego - SAN,
# San Francisco - SFO, Seattle - BFI, St.Louis - STL, Washington DC - DCA

# Function returns a table with two columns: home team abbreviations and airport codes for riem Package.
airport_codes <- function() {
  team_list <- c("LAA", "ATL", "BOS", "CHC", "CIN", "COL", "DET", "LAD", "MIA", "MIL", "MIN", "NYM", "PHI",
                 "ARI", "PIT", "SDP", "SFG", "SEA", "STL", "WSN")
  airport_codes <- c("FUL", "MGE", "BOS", "MDW", "LUK", "BKF", "DET", "FUL", "MIA", "MKE", "MSP", "LGA", "PHL", "PHX",
                     "PIT", "SAN", "SFO", "BFI", "STL", "DCA")
  return(tibble(Team = team_list, airport_codes))
}
