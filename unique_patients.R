require("tidyverse")
require("RecordLinkage")
require("sqldf")


patients_df <- read.csv("patients.csv")
patients_df <- read.csv("patients_10-01-21_thu_02-28-22.csv")

df <- patients_df %>% 
  filter(!(patients_df$patient_last_name == "NULL"))

df <- sqldf("SELECT ln_fn_dob FROM df")

i <- 1
max_i <- nrow(df)

matches <- c()
#names(matches) <- "name"

unique_patients <- data.frame()
names <- list()
matched_names <- list()
start_time <- Sys.time()

for (i in i:max_i) {
if (i == 1) {
  test_df <- as.data.frame(cbind(name = df$ln_fn_dob[], match_score = jarowinkler(df$ln_fn_dob[i],df$ln_fn_dob[]))) 
} else if (i <= max_i){
  test_df <- as.data.frame(cbind(name = test_df$name[], match_score = jarowinkler(test_df$name[i],test_df$name[])))
}
  matches <- test_df %>%
    filter(match_score > 0.93)
  names[[i]] <- paste(matches$name[[1]],"-",format(matches$match_score[[1]], digits = 3))
  matched_names[[i]] <- paste(matches$name,"-",format(matches$match_score, digits = 3))
  
  test_df <- test_df[!(test_df$name %in% matches),]
  
  max_i <- nrow(test_df)
  
  if (i %% 10 == 0) {
   cat(paste0("\rProgress: ", format((i / max_i) * 100, digits = 3), "% | Rows Remaining: ", (max_i - i), " | Time Elapsed: ", format(difftime(Sys.time(), start_time, units = "mins")), digits = 3))
  }
  
  unique_patients <- data.frame(cbind(names = names, matched_names = matched_names))
  unique_matches <- length(unique(unique_patients$names))
}

View(wordlist)
View(df)

unique2 <- unique(unique_patients$names)

View(unique2)

unique3 <- list()
n <- 1
max_n <- length(unique_patients)
for (n in 1:max_n) {
unique3[[n]] <- unique_patients[[n]][[1]][[1]]
}


unique4 <- unique(unique3)

ref <- unique(df$ln_fn_dob)
words <- ref

wordlist <- expand.grid(words = words, ref = ref, stringsAsFactors = FALSE)
wordlist <- wordlist %>% 
  group_by(words) %>% 
  mutate(match_score = jarowinkler(words, ref)) %>%
  #mutate(if (match_score > 0.93){})
  filter(match_score < 0.9999999 & match_score > 0.93) #%>%
#summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

sqldf("SELECT * FROM wordlist")


