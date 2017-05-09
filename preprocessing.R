dataset <- read.csv("data/1608531_enwiki.csv", sep = "\t", stringsAsFactors = F)

# calculate max number of workers
max_workers <- 0
for (idx in 1:length(dataset[, 1])) {
  split <- strsplit(dataset$rating_list[idx], ",")[[1]]
  max_workers <- max(max_workers, length(split))
}

# make data frame with fixed number of columns
for (idx in 1:max_workers) {
  dataset[, paste("worker", idx, sep = "_")] <- NA
}
dataset$max_workers <- 0
dataset$std_dev <- 0
dataset$std_dev_4 <- 0
dataset$std_dev_8 <- 0
dataset$std_dev_12 <- 0

dif_std_dev_4 <- 0
dif_std_dev_8 <- 0
dif_std_dev_12 <- 0

dif_std_dev <-0



# populate data frame
for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$rating_list[idx], ",")[[1]]
  dataset$max_workers[idx] <- length(split)
  
  for (jdx in 1:length(split)) {
    dataset[idx, paste("worker", jdx, sep = "_")] <- as.numeric(split[jdx])
    vector <- append(vector, as.numeric(split[jdx]))

  dataset$std_dev[idx] <- sd (vector)
  }
  
  vector <- c()
  for (jdx in 1:4) {
    vector <- append(vector, as.numeric(split[jdx]))
    
  dataset$std_dev_4[idx] <- sd (vector)

  }
  
  vector <- c()
  for (jdx in 1:8) {
    vector <- append(vector, as.numeric(split[jdx]))
    
  dataset$std_dev_8[idx] <- sd (vector)
  }
  
  vector <- c()
  for (jdx in 1:12) {
    vector <- append(vector, as.numeric(split[jdx]))
    
  dataset$std_dev_12[idx] <- sd (vector)
  }
  
  dif_std_dev_4 <- dif_std_dev_4 + abs(dataset$std_dev_4[idx] - dataset$std_dev[idx])
  dif_std_dev_8 <- dif_std_dev_8 + abs(dataset$std_dev_8[idx] - dataset$std_dev[idx])
  dif_std_dev_12 <- dif_std_dev_12 + abs(dataset$std_dev_12[idx] - dataset$std_dev[idx])
  

}
dif_std_dev_4 <- dif_std_dev_4 / length(dataset[, 1])
dif_std_dev_8 <- dif_std_dev_8 / length(dataset[, 1])
dif_std_dev_12 <- dif_std_dev_12 / length(dataset[, 1])

#calculate mean + std_dev per country
ja_mean = 0
ja_std_dev = 0
ja_counter = 0

en_mean = 0
en_std_dev = 0
en_counter = 0

es_mean = 0
es_std_dev = 0
es_counter = 0

zh_mean = 0
zh_std_dev = 0
zh_counter = 0

no_mean = 0
no_std_dev = 0
no_counter = 0

for (idx in 1:length(dataset[, 1])){
    
    if (dataset$language[idx] == "ja"){
      ja_mean <- ja_mean + dataset$mean_rating[idx]
      ja_std_dev <- ja_std_dev + dataset$std_dev[idx]
      ja_counter <- ja_counter + 1
    }
  
    else if (dataset$language[idx] == "en"){
      en_mean <- en_mean + dataset$mean_rating[idx]
      en_std_dev <- en_std_dev + dataset$std_dev[idx]
      en_counter <- en_counter + 1
    }

    else if (dataset$language[idx] == "es"){
      es_mean <- es_mean + dataset$mean_rating[idx]
      es_std_dev <- es_std_dev + dataset$std_dev[idx]
      es_counter <- es_counter + 1
    }   
  
    else if (dataset$language[idx] == "zh"){
      zh_mean <- zh_mean + dataset$mean_rating[idx]
      zh_std_dev <- zh_std_dev + dataset$std_dev[idx]
      zh_counter <- zh_counter + 1
    }
  
   else if (dataset$language[idx] == "no"){
      no_mean <- no_mean + dataset$mean_rating[idx]
      no_std_dev <- no_std_dev + dataset$std_dev[idx]
      no_counter <- no_counter + 1
    }
}

ja_mean <- ja_mean/ja_counter 
ja_std_dev <- ja_std_dev/ja_counter

en_mean <- en_mean/en_counter 
en_std_dev <- en_std_dev/en_counter

es_mean <- es_mean/es_counter 
es_std_dev <- es_std_dev/es_counter

zh_mean <- zh_mean/zh_counter 
zh_std_dev <- zh_std_dev/zh_counter

no_mean <- no_mean/no_counter 
no_std_dev <- no_std_dev/no_counter

#calculate mean + std_dev of sentence length

for (idx in 1:length(dataset[, 1])){
  if (dataset$length[idx] <= 5){
    mean_len_5 <- mean_len_5 + dataset$length[idx]
  }
  else if (dataset$length[idx] >=6 & dataset$length[idx] <= 10){
    #print (dataset$length[idx])
  }
  else if (dataset$length[idx] > 10){
    #print (dataset$length[idx])
  }
}

write.csv(dataset, "data/pre-processed-data.csv", row.names = F)

# save(dataset, file="dataset.Rdata")
# load(dataset)