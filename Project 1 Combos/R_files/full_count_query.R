library(RSQLite)
library(dplyr)
library(ggplot2)
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 
# Table name is statcast_data


query <- "SELECT description, events AS plate_outcome, pitch_number FROM statcast_data WHERE balls == 3 AND 
          strikes == 2"


# Workspace
full_counts <- dbGetQuery(conn, query)

dates <- dbGetQuery(conn, "SELECT DISTINCT game_year FROM statcast_data")
table(full_counts$description)
table(full_counts$plate_outcome)

test <- head(full_counts)

change_descrip <- function(df) {
  for (i in 1:nrow(df)) {
    if (df[i, 1] %in% c("ball", "blocked_ball", "intent_ball")) {
      df[i, 1] <- "walk"
    }
    
    else if (df[i, 1] %in% c("called_strike", "foul_bunt", "foul_tip", "missed_bunt", "swinging_strike",
                        "swinging_strike_blocked")) {
      df[i, 1] <- "strikeout"
    }
    
    else if (df[i, 1] == "hit_by_pitch") { # Do nothing
    }
    
    else if (substring(df[i ,1], 1, 3) == "hit") {
      df[i, 1] <- df[i, 2]
      if (!(df[i, 1] %in% c("single", "double", "triple", "home_run", "catcher_interf"))) {
        df[i, 1] <- "contact_out_or_error"
      }
    }
  }
  return(df)
}
# counted errors as out


full_counts_clean <- change_descrip(full_counts)

good_bad <- function(outcome) {
  if (outcome %in% c("strikeout", "contact_out_or_error")) { 
    return("bad")
  }
  else if (outcome == "foul") return("foul")
  return("good")
}

good_bad_vec <- Vectorize(good_bad)

good_bad2 <- function(outcome) {
  if (outcome %in% c("single", "double", "triple", "home_run")) { 
    return("hit")
  }
  else if (outcome %in% c("catcher_interf", "hit_by_pitch")) return("misc")
  return(outcome)
}
good_bad_vec2 <- Vectorize(good_bad2)

full_counts_clean <- full_counts_clean %>%
  filter(pitch_number <= 13) %>%
  mutate(simple_outcome = good_bad_vec(description), simple_outcome2 = good_bad_vec2(description))


ggplot(data = full_counts_clean, aes(x = pitch_number, group = factor(description), fill = factor(description))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Full Count Outcomes based on Plate Appearance Pitch Number") +
  xlab("Pitch Number") +
  ylab("Percentage")


ggplot(data = full_counts_clean, aes(x = pitch_number, group = factor(simple_outcome), 
                                     fill = factor(simple_outcome))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Full Count Outcomes based on Plate Appearance Pitch Number") +
  xlab("Pitch Number") +
  ylab("Percentage")

ggplot(data = full_counts_clean, aes(x = pitch_number, group = factor(simple_outcome2), 
                                     fill = factor(simple_outcome2))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Full Count Outcomes based on Plate Appearance Pitch Number") +
  xlab("Pitch Number") +
  ylab("Percentage")


ggplot(data = full_counts_clean, aes(pitch_number)) +
  geom_bar(aes(fill = description))



head(full_counts_clean)
table(full_counts_clean$description)

# Add wOBA column

woba_value <- function(description) {
  if (description %in% c("catcher_interf", "foul")) return(NA)
  else if (description == "contact_out_or_error") return(0)
  else if (description %in% c("walk", "hit_by_pitch")) return(0.7)
  else if (description == "single") return(0.9)
  else if (description == "double") return(1.25)
  else if (description == "triple") return(1.6)
  else if (description == "home_run") return(2)
}

woba_value_vec <- Vectorize(woba_value)

head(full_counts_clean) %>%
  mutate(woba = woba_value(description)) %>%
  group_by(pitch_number) %>%
  summarise(occurrences)




query2 <- "SELECT game_pk, player_name, description, events AS plate_outcome, game_type, 
           balls, strikes, inning, woba_value, at_bat_number, pitch_number FROM 
           statcast_data WHERE pitch_number > 4"

new_df <- dbGetQuery(conn, query2)
dbDisconnect(conn)
new_df2 <- as_tibble(new_df) 
# Reorder variables and PA_id, which combines the game id with the PA number
new_df2 <- new_df2 %>%
  select(game_pk, balls, strikes, pitch_number, at_bat_number, description, plate_outcome, everything()) %>%
  mutate(PA_id = paste0(as.character(game_pk), as.character(at_bat_number)))

all_full_count_pitches <- new_df2 %>%
  filter(balls == 3, strikes == 2)


# PA's with 3-2 count on five pitches
full_count_in_5 <- new_df2 %>%
  filter(pitch_number == 6, balls == 3, strikes == 2) 

# ID number of PA's that satisfy our requirement
pa_id_satisfied <- full_count_in_5$PA_id

# pa_count_full_5 <- nrow(full_count_in_5)


change_descrip2 <- function(df) {
  for (i in 1:nrow(df)) {
    curr_descrip <- df[i, 6]
    if (curr_descrip %in% c("ball", "blocked_ball", "intent_ball")) {
      df[i, 6] <- "walk"
    }
    
    else if (curr_descrip %in% c("called_strike", "foul_bunt", "foul_tip", "missed_bunt", "swinging_strike",
                             "swinging_strike_blocked")) {
      df[i, 6] <- "strikeout"
    }
    
    else if (curr_descrip == "hit_by_pitch") { # Do nothing
    }
    
    else if (substring(curr_descrip, 1, 3) == "hit") {
      df[i, 6] <- df[i, 7]
      if (!(df[i, 6] %in% c("single", "double", "triple", "home_run", "catcher_interf"))) {
        df[i, 6] <- "contact_out_or_error"
      }
    }
  }
  return(df)
}
all_full_count_pitches <- change_descrip2(all_full_count_pitches)

final_data <- all_full_count_pitches %>%
  filter(PA_id %in% pa_id_satisfied, plate_outcome != "intent_walk")



woba_value <- function(descriptions) {
  sapply(descriptions, function(description) {
    output <- NA
    if (description %in% c("catcher_interf", "foul")) return(output)
    else if (description %in% c("contact_out_or_error", "strikeout")) output <- 0
    else if (description %in% c("walk", "hit_by_pitch")) output <- 0.7
    else if (description == "single") output <- 0.9
    else if (description == "double") output <- 1.25
    else if (description == "triple") output <- 1.6
    else if (description == "home_run") output <- 2.0
    return(output)
  })
}





final_data <- final_data %>%
  mutate(formula_woba = as.numeric(woba_value(description))) #%>%
  #select(description, woba_value, formula_woba)

final_data %>%
  group_by(pitch_number) %>%
  summarize(count = n(), statcast_woba_avg = mean(woba_value, na.rm = TRUE), 
            formula_woba_avg = mean(formula_woba, na.rm = TRUE))



temp <- final_data %>%
  filter(woba_value != formula_woba) %>%
  select(woba_value, formula_woba, everything())

table(temp$plate_outcome)





key_at_bat_game_pk <-vector("numeric", pa_count_full_5)
key_at_bat_pa_num <- vector("numeric", pa_count_full_5)


for (i in 1:pa_count_full_5) {
  key_at_bat_game_pk[i] <- full_count_in_5[[i, "game_pk"]]
  key_at_bat_pa_num[i] <- full_count_in_5[[i, "at_bat_number"]]
}

final_data <- tibble()
colnames(final_data) <- colnames(all_full_count_pitches)









for (i in 1:nrow(all_full_count_pitches)) {
  curr_pk <- all_full_count_pitches[[i, "game_pk"]]
  curr_ab <- all_full_count_pitches[[i, "at_bat_number"]]
  for (j in 1:pa_count_full_5) {
    if (curr_pk == key_at_bat_game_pk[[j]] & curr_ab == key_at_bat_pa_num[[j]]) {
      final_data <- bind_rows(final_data, all_full_count_pitches[i, ])
      break
    }
  }
}
curr <- all_full_count_pitches[[1, "at_bat_number"]]

x <- data.frame(x = 1:3, y = 3:5)

# add indices variable
# 






test <- test %>%
  #filter(pitch_number <= 13) %>%
  mutate(simple_outcome2 = good_bad_vec2(description))

test <- change_descrip(test)

