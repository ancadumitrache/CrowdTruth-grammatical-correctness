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

std_dev_dropoff <- 0 


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

len5_mean = 0
len5_std_dev = 0
len5_counter = 0

len10_mean = 0
len10_std_dev = 0
len10_counter = 0

len15_mean = 0
len15_std_dev = 0
len15_counter = 0

for (idx in 1:length(dataset[, 1])){
  
  if (dataset$length[idx] >= 5 && dataset$length[idx] <= 10){
    #print(dataset$length[idx])
    len5_mean <- len5_mean + dataset$mean_rating[idx]
    len5_std_dev <- len5_std_dev + dataset$std_dev[idx]
    len5_counter <- len5_counter + 1
  }
  
  else if (dataset$length[idx] >= 11 & dataset$length[idx] <= 15){
    len10_mean <- len10_mean + dataset$mean_rating[idx]
    len10_std_dev <- len10_std_dev + dataset$std_dev[idx]
    len10_counter <- len10_counter + 1
  }
  
  else if (dataset$length[idx] >= 16 & dataset$length[idx] <= 25){
    len15_mean <- len15_mean + dataset$mean_rating[idx]
    len15_std_dev <- len15_std_dev + dataset$std_dev[idx]
    len15_counter <- len15_counter + 1
  }
}

len5_mean <- len5_mean/len5_counter 
len5_std_dev <- len5_std_dev/len5_counter

len10_mean <- len10_mean/len10_counter 
len10_std_dev <- len10_std_dev/len10_counter

len15_mean <- len15_mean/len15_counter 
len15_std_dev <- len15_std_dev/len15_counter


#Plot the plateauing in SD 

for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$rating_list[idx], ",")[[1]]

  for (jdx in 1:length(split)) {
    vector <- append(vector, as.numeric(split[jdx]))
  }
  
  std_dev_max <- sd (vector)
  vector <- vector[-length(vector)]
  vector <- vector[-length(vector)]
  std_dev_max_minus_2 <- sd (vector)

  dataset$std_dev_dropoff[idx] <- abs(std_dev_max - std_dev_max_minus_2)
}

#Plot dropoff per amount of workers

length_sd_1 <- c()
length_sd_2 <- c()
length_sd_3 <- c()
length_sd_4 <- c()
length_sd_5 <- c()
length_sd_6 <- c()
length_sd_7 <- c()
length_sd_8 <- c()
length_sd_9 <- c()
length_sd_10 <- c()
length_sd_11 <- c()
length_sd_12 <- c()
length_sd_13 <- c()
length_sd_14 <- c()
length_sd_15 <- c()
length_sd_16 <- c()
length_sd_17 <- c()
length_sd_18 <- c()
length_sd_19 <- c()
length_sd_20 <- c()

for (idx in 1:length(dataset[, 1])) {
  
  for (jdx in 1:max_workers) {
    
    if (dataset$max_workers[idx] == 12) {
      length_sd_12 <- append(length_sd_12, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 13) {
      length_sd_13 <- append(length_sd_13, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 14) {
      length_sd_14 <- append(length_sd_14, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 15) {
      length_sd_15 <- append(length_sd_15, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 16) {
      length_sd_16 <- append(length_sd_16, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 17) {
      length_sd_17 <- append(length_sd_17, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 18) {
      length_sd_18 <- append(length_sd_18, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 19) {
      length_sd_19 <- append(length_sd_19, dataset$std_dev_dropoff[idx])
    }
    else if (dataset$max_workers[idx] == 20) {
      length_sd_20 <- append(length_sd_20, dataset$std_dev_dropoff[idx])
    }
  }
}

avg_sd_dropoff_length_12 <- mean(length_sd_12)
avg_sd_dropoff_length_13 <- mean(length_sd_13)
avg_sd_dropoff_length_14 <- mean(length_sd_14)
avg_sd_dropoff_length_15 <- mean(length_sd_15)
avg_sd_dropoff_length_16 <- mean(length_sd_16)
avg_sd_dropoff_length_17 <- mean(length_sd_17)
avg_sd_dropoff_length_18 <- mean(length_sd_18)
avg_sd_dropoff_length_19 <- mean(length_sd_19)
avg_sd_dropoff_length_20 <- mean(length_sd_20)

vectorplot <- c(avg_sd_dropoff_length_12, avg_sd_dropoff_length_13, avg_sd_dropoff_length_14, avg_sd_dropoff_length_15, avg_sd_dropoff_length_16, avg_sd_dropoff_length_17, avg_sd_dropoff_length_18, avg_sd_dropoff_length_19, avg_sd_dropoff_length_20)

plot(vectorplot, type = "l", main = "SD Dropoff", ylab ="SD Dropoff", xlab ="number of workers", xaxt='n')
axis(1, at=1:9, labels= 12:20)