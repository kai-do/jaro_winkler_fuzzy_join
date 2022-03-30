
#
#
#patients_df <- read.csv("patients.csv")
#patients_df <- read.csv("patients_10-01-21_thu_02-28-22.csv")
#
#df <- patients_df %>% 
#  filter(!(patients_df$patient_last_name == "NULL"))
#
#df <- sqldf("SELECT ln_fn_dob FROM df LIMIT 5000")
#
#test_list <- df$ln_fn_dob
#
#list <- test_list
#
#test_fuzzy <- fuzzy_join(test_list, precision = 0.9) #, row_limit = 1000)
#
#View(test_fuzzy$df)

fuzzy_join <- function(list, precision = 0.93, row_limit = NULL){
  
  matches <- c()
  names <- list()
  matched_names <- list()
  unique <- data.frame()
  start_time <- Sys.time()
  
  i <- 1
  if (is.null(row_limit)) {
    max_i <- length(list)
  } else {
    max_i <- row_limit
  }
  
  for (i in i:max_i) {
    if (i == 1) {
      loop_df <- as.data.frame(cbind(name = list[], match_score = jarowinkler(list[i],list[1:max_i]))) 
    } else if (i <= max_i) {
      loop_df <- as.data.frame(cbind(name = loop_df$name[], match_score = jarowinkler(loop_df$name[i],loop_df$name[1:max_i])))
    }
    
    if (i <= max_i) {
      matches <- loop_df %>%
        filter(match_score > precision)
      names[[i]] <- paste(matches$name[[1]],"-",format(matches$match_score[[1]], digits = 3))
      matched_names[[i]] <- paste(matches$name,"-",format(matches$match_score, digits = 3))
      
      loop_df <- loop_df[!(loop_df$name %in% matches),]
      
      if (is.null(row_limit)) {
        max_i <- nrow(loop_df)
        max_list_length <- nrow(df)
      } else if (!is.null(row_limit) & i == 1) {
        max_list_length <- row_limit 
      }
      
      if (i %% 10 == 0 | i == max_i) {
        cat("\r===============================================================================================================")
        cat(paste0("\r======| Rows Remaining: ", (max_i - i), " | Unique Matches: ", max_list_length - (max_list_length - length(unique(names))), " of ", max_list_length, " | Progress: ", format((i / max_i) * 100, digits = 3), "% | Time Elapsed: ", format(difftime(Sys.time(), start_time, units = "mins"), digits = 3), " |"))
      }
    }
  }
  match_df <- data.frame(cbind(names = names, matched_names = matched_names))
  unique_matches <- length(unique(match_df$names))
  
  return(list(df = match_df, unique_matches = unique_matches, precision = precision))
}


