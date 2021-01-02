# Matrix approach
set.seed(9)
start_time <- Sys.time()
rf_model_ff <- randomForest(x = train_ff[, c(3, 4, 7, 8, 15, 16)], y = train_ff$barrel, 
                            ntree = 100, mtry = 3)
end_time <- Sys.time()
end_time - start_time # Time difference of 3.467752 mins vs 3.512603 mins
# Time difference isn't noticable