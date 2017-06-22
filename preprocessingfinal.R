dataset <- read.csv("data/MOP2_final.csv", sep = ",", stringsAsFactors = F)
dataset <- read.csv("data/MOP4_final.csv", sep = ",", stringsAsFactors = F)
dataset <- read.csv("data/MOP4_vs_MOP2_final.csv", sep = ";", stringsAsFactors = F)

# calculate max number of workers
#max_workers <- 0
#for (idx in 1:length(dataset[, 1])) {
#  split <- strsplit(dataset$merged_ratings[idx], ",")[[1]]
#  max_workers <- max(max_workers, length(split))
#}

# make data frame with fixed number of columns
#for (idx in 1:max_workers) {
#  dataset[, paste("worker", idx, sep = "_")] <- NA
#}

#dataset$max_workers <- 0
#dataset$std_dev <- 0
#dataset$mean_rating <-0
#dif_std_dev <-0
#std_dev_dropoff <- 0 


# populate data frame MOP 4
for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$merged_ratings[idx], ",")[[1]]
  dataset$max_workers[idx] <- length(split)
  dataset$mean_rating[idx] <- mean(as.numeric(c(strsplit((c(dataset$merged_ratings[idx])), ",")[[1]])))
  dataset$std_dev[idx] <- sd(as.numeric(c(strsplit((c(dataset$merged_ratings[idx])), ",")[[1]])))
}

# populate data frame MOP 2
for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$how_natural[idx], ",")[[1]]
  dataset$max_workers[idx] <- length(split)
  dataset$mean_rating[idx] <- mean(as.numeric(c(strsplit((c(dataset$how_natural[idx])), ",")[[1]])))
  dataset$std_dev[idx] <- sd(as.numeric(c(strsplit((c(dataset$how_natural[idx])), ",")[[1]])))
  
}
  #for (jdx in 1:length(split)) {
  #  dataset[idx, paste("worker", jdx, sep = "_")] <- as.numeric(split[jdx])
  #  vector <- append(vector, as.numeric(split[jdx]))
  #}

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

len8_mean <- c()
len9_mean <- c()
len10_mean <- c()
len11_mean <- c()
len12_mean <- c()
len13_mean <- c()
len14_mean <- c()
len15_mean <- c()
len16_mean <- c()
len17_mean <- c()
len18_mean <- c()
len19_mean <- c()
len20_mean <- c()
len21_mean <- c()
len22_mean <- c()
len23_mean <- c()
len24_mean <- c()
len25_mean <- c()

for (idx in 1:length(dataset[, 1])){
  if (!is.na(as.numeric(dataset$length[idx]))){
    
    if (as.numeric(dataset$length[idx]) == 8){
      len8_mean <- append(len8_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 9){
      len9_mean <- append(len9_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 10){
      len10_mean <- append(len10_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 11){
      len11_mean <- append(len11_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 12){
      len12_mean <- append(len12_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 13){
      len13_mean <- append(len13_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 14){
      len14_mean <- append(len14_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 15){
      len15_mean <- append(len15_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 16){
      len16_mean <- append(len16_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 17){
      len17_mean <- append(len17_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 18){
      len18_mean <- append(len18_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 19){
      len19_mean <- append(len19_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 20){
      len20_mean <- append(len20_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 21){
      len21_mean <- append(len21_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 22){
      len22_mean <- append(len22_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 23){
      len23_mean <- append(len23_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 24){
      len24_mean <- append(len24_mean, dataset$mean_rating[idx])
    }
    else if (dataset$length[idx] == 25){
      len25_mean <- append(len25_mean, dataset$mean_rating[idx])
    }
  }
}

########### Trying to create a for loop
#mean(len8_mean) 
#sd(len8_mean)
#for (idx in 1:25){
#  print(mean(lenidx_mean) "," sd(lenidx_mean))
#}



#Plot the plateauing in SD 

for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$combined_rating_list[idx], ",")[[1]]
  
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

#length_sd_1 <- c()
#length_sd_2 <- c()
#length_sd_3 <- c()
#length_sd_4 <- c()
#length_sd_5 <- c()
#length_sd_6 <- c()
#length_sd_7 <- c()
#length_sd_8 <- c()
#length_sd_9 <- c()
#length_sd_10 <- c()
#length_sd_11 <- c()
#length_sd_12 <- c()
#length_sd_13 <- c()
#length_sd_14 <- c()
#length_sd_15 <- c()
#length_sd_16 <- c()
#length_sd_17 <- c()
#length_sd_18 <- c()
#length_sd_19 <- c()
#length_sd_20 <- c()

#for (idx in 1:length(dataset[, 1])) {
  
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

#avg_sd_dropoff_length_12 <- mean(length_sd_12)
#avg_sd_dropoff_length_13 <- mean(length_sd_13)
#avg_sd_dropoff_length_14 <- mean(length_sd_14)
#avg_sd_dropoff_length_15 <- mean(length_sd_15)
#avg_sd_dropoff_length_16 <- mean(length_sd_16)
#avg_sd_dropoff_length_17 <- mean(length_sd_17)
#avg_sd_dropoff_length_18 <- mean(length_sd_18)
#avg_sd_dropoff_length_19 <- mean(length_sd_19)
#avg_sd_dropoff_length_20 <- mean(length_sd_20)

#vectorplot <- c(avg_sd_dropoff_length_12, avg_sd_dropoff_length_13, avg_sd_dropoff_length_14, avg_sd_dropoff_length_15, avg_sd_dropoff_length_16, avg_sd_dropoff_length_17, avg_sd_dropoff_length_18, avg_sd_dropoff_length_19, avg_sd_dropoff_length_20)

#plot(vectorplot, type = "l", main = "SD Dropoff", ylab ="SD Dropoff", xlab ="number of workers", xaxt='n')
#axis(1, at=1:9, labels= 12:20)

#populate dataset with complexity

for (idx in 1:length(dataset[, 1])) {
  complexity = 0
  split <- strsplit(dataset$text[idx], " ")[[1]]
  complexity <- complexity + (length (grep("^and$", split)))
  complexity <- complexity + (length (grep("^that$", split)))
  complexity <- complexity + (length (grep("^but$", split)))
  complexity <- complexity + (length (grep("^or$", split)))
  complexity <- complexity + (length (grep("^as$", split)))
  complexity <- complexity + (length (grep("^if$", split)))
  complexity <- complexity + (length (grep("^when$", split)))
  complexity <- complexity + (length (grep("^than$", split)))
  complexity <- complexity + (length (grep("^because$", split)))
  complexity <- complexity + (length (grep("^while$", split)))
  
  dataset$complexity[idx] <- complexity
}

#Calculate and print average complexity mean + sd

comp0_mean <- c()
comp1_mean <- c()
comp2_mean <- c()
comp3_mean <- c()

for (idx in 1:length(dataset[, 1])){
  if (!is.na(as.numeric(dataset$complexity[idx]))){
    
    if (as.numeric(dataset$complexity[idx]) == 0){
      comp0_mean <- append(comp0_mean, dataset$mean_rating[idx])
    }
    else if (dataset$complexity[idx] == 1){
      comp1_mean <- append(comp1_mean, dataset$mean_rating[idx])
    }
    else if (dataset$complexity[idx] == 2){
      comp2_mean <- append(comp2_mean, dataset$mean_rating[idx])
    }
    else if (dataset$complexity[idx] == 3){
      comp3_mean <- append(comp3_mean, dataset$mean_rating[idx])
    }
  }
}


#dropoff for <20 <30 <40 <50
dropoff20 <- c()
dropoff30 <- c()
dropoff40 <- c()
dropoff50 <- c()
dropoff60 <- c()

for (idx in 1:length(dataset[, 1])) {
  
  if (dataset$max_workers[idx] < 21) {
    dropoff20 <- append(dropoff20, dataset$std_dev_dropoff[idx])
  }
  
  else if (dataset$max_workers[idx] < 31) {
    dropoff30 <- append(dropoff30, dataset$std_dev_dropoff[idx])
  }
  
  else if (dataset$max_workers[idx] < 41) {
    dropoff40 <- append(dropoff40, dataset$std_dev_dropoff[idx])
  }
  
  else if (dataset$max_workers[idx] < 51) {
    dropoff50 <- append(dropoff50, dataset$std_dev_dropoff[idx])
  }
  
  else if (dataset$max_workers[idx] < 61) {
    dropoff60 <- append(dropoff60, dataset$std_dev_dropoff[idx])
  }
}

#calculate how many reasons have been given
for (idx in 1:length(dataset[, 1])) {
  split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
  dataset$number_of_reasons[idx] <- length(as.list(strsplit(split, ",")[[1]]))
}

#add comma at reasons
for (idx in 1:length(dataset[, 1])) {
  dataset$followup1[idx]<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
}

#compare difference_mean and difference_sd between mop2 & mop4 

for (idx in 1:length(dataset[, 1])) {
  
  mop4_mean <- as.numeric(strsplit(dataset$mean_rating[idx], " ")[[1]][1])
  mop2_mean <- as.numeric(strsplit(dataset$mean_rating[idx], " ")[[1]][2])
  dataset$mean_difference[idx] <- abs(mop4_mean - mop2_mean)
  
  mop4_sd <- as.numeric(strsplit(dataset$std_dev[idx], " ")[[1]][1])
  mop2_sd <- as.numeric(strsplit(dataset$std_dev[idx], " ")[[1]][2])
  dataset$sd_difference[idx] <- abs(mop4_sd - mop2_sd)
                                                          
}

#compare difference_mean and difference_sd to amount of workers

worker8_mean_dif <- c()
worker9_mean_dif <- c()
worker10_mean_dif <- c()
worker11_mean_dif <- c()
worker12_mean_dif <- c()
worker13_mean_dif <- c()
worker14_mean_dif <- c()
worker15_mean_dif <- c()
worker16_mean_dif <- c()
worker17_mean_dif <- c()
worker18_mean_dif <- c()
worker19_mean_dif <- c()
worker20_mean_dif <- c()

worker8_sd_dif <- c()
worker9_sd_dif <- c()
worker10_sd_dif <- c()
worker11_sd_dif <- c()
worker12_sd_dif <- c()
worker13_sd_dif <- c()
worker14_sd_dif <- c()
worker15_sd_dif <- c()
worker16_sd_dif <- c()
worker17_sd_dif <- c()
worker18_sd_dif <- c()
worker19_sd_dif <- c()
worker20_sd_dif <- c()

for (idx in 1:length(dataset[, 1])){
  if (!is.na(as.numeric(dataset$mean_difference[idx]))){
    if (!is.na(as.numeric(dataset$sd_difference[idx]))){
      
      if (as.numeric(dataset$max_workers[idx]) == 8){
        worker8_mean_dif <- append(worker8_mean_dif, dataset$mean_difference[idx])
        worker8_sd_dif <- append(worker8_sd_dif, dataset$sd_difference[idx])
      }
      else if (as.numeric(dataset$max_workers[idx]) == 9){
        worker9_mean_dif <- append(worker9_mean_dif, dataset$mean_difference[idx])
        worker9_sd_dif <- append(worker9_sd_dif, dataset$sd_difference[idx])
      }
    }
  }
}


#exporting dataset as csv
write.csv(dataset, file = "MOP2_final.csv",row.names=FALSE, na="")
write.csv(dataset, file = "MOP4_final_mergemop2.csv",row.names=FALSE, na="")