dataset <- read.csv("data/MOP2_final.csv", sep = ",", stringsAsFactors = F)
dataset <- read.csv("data/MOP4_final.csv", sep = ",", stringsAsFactors = F)
dataset <- read.csv("data/MOP4_vs_MOP2.csv", sep = ";", stringsAsFactors = F)
dataset <- read.csv("data/MOP2_final_1.csv", sep = ";", stringsAsFactors = F)

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


#Plot the plateauing in SD 

for (idx in 1:length(dataset[, 1])) {
  vector <- c()
  split <- strsplit(dataset$merged_ratings[idx], ",")[[1]]
  
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


#dropoff for per worker

for (idx in 1:40){ 
  assign(paste("dropoff_mean",idx, sep="_"), (c(NULL)))
  assign(paste("dropoff_std_dev",idx, sep="_"), (c(NULL)))
  assign(paste("workers_std_dev",idx, sep="_"), (c(NULL)))
  
}

#for (idx in 1:length(dataset[, 1])) {
  temp_mean <- (paste("dropoff_mean",dataset$max_workers[3], sep = "_"))
  
  if (!is.na(as.numeric(dataset$max_workers[idx]))){
    for (jdx in 16:40){
      if (as.numeric(dataset$max_workers[idx] == jdx)){
        temp_mean <- append(as.name(temp_mean), dataset$mean_rating[idx])
        
      }
    }
  }
}

for (idx in 1:length(dataset[, 1])) {
  
  if (!is.na(as.numeric(dataset$max_workers[idx]))){
    if (as.numeric(dataset$max_workers[idx] == 16)){
      dropoff_mean_16 <- append(dropoff_mean_16, dataset$mean_rating[idx])
      dropoff_std_dev_16 <- append(dropoff_std_dev_16, dataset$std_dev_dropoff[idx])
      workers_std_dev_16 <- append(workers_std_dev_16, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 17)){
      dropoff_mean_17 <- append(dropoff_mean_17, dataset$mean_rating[idx])
      dropoff_std_dev_17 <- append(dropoff_std_dev_17, dataset$std_dev_dropoff[idx])
      workers_std_dev_17 <- append(workers_std_dev_17, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 18)){
      dropoff_mean_18 <- append(dropoff_mean_18, dataset$mean_rating[idx])
      dropoff_std_dev_18 <- append(dropoff_std_dev_18, dataset$std_dev_dropoff[idx])
      workers_std_dev_18 <- append(workers_std_dev_18, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 19)){
      dropoff_mean_19 <- append(dropoff_mean_19, dataset$mean_rating[idx])
      dropoff_std_dev_19 <- append(dropoff_std_dev_19, dataset$std_dev_dropoff[idx])
      workers_std_dev_19 <- append(workers_std_dev_19, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 20)){
      dropoff_mean_20 <- append(dropoff_mean_20, dataset$mean_rating[idx])
      dropoff_std_dev_20 <- append(dropoff_std_dev_20, dataset$std_dev_dropoff[idx])
      workers_std_dev_20 <- append(workers_std_dev_20, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 21)){
      dropoff_mean_21 <- append(dropoff_mean_21, dataset$mean_rating[idx])
      dropoff_std_dev_21 <- append(dropoff_std_dev_21, dataset$std_dev_dropoff[idx])
      workers_std_dev_21 <- append(workers_std_dev_21, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 22)){
      dropoff_mean_22 <- append(dropoff_mean_22, dataset$mean_rating[idx])
      dropoff_std_dev_22 <- append(dropoff_std_dev_22, dataset$std_dev_dropoff[idx])
      workers_std_dev_22 <- append(workers_std_dev_22, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 23)){
      dropoff_mean_23 <- append(dropoff_mean_23, dataset$mean_rating[idx])
      dropoff_std_dev_23 <- append(dropoff_std_dev_23, dataset$std_dev_dropoff[idx])
      workers_std_dev_23 <- append(workers_std_dev_23, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 24)){
      dropoff_mean_24 <- append(dropoff_mean_24, dataset$mean_rating[idx])
      dropoff_std_dev_24 <- append(dropoff_std_dev_24, dataset$std_dev_dropoff[idx])
      workers_std_dev_24 <- append(workers_std_dev_24, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 25)){
      dropoff_mean_25 <- append(dropoff_mean_25, dataset$mean_rating[idx])
      dropoff_std_dev_25 <- append(dropoff_std_dev_25, dataset$std_dev_dropoff[idx])
      workers_std_dev_25 <- append(workers_std_dev_25, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 26)){
      dropoff_mean_26 <- append(dropoff_mean_26, dataset$mean_rating[idx])
      dropoff_std_dev_26 <- append(dropoff_std_dev_26, dataset$std_dev_dropoff[idx])
      workers_std_dev_26 <- append(workers_std_dev_26, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 27)){
      dropoff_mean_27 <- append(dropoff_mean_27, dataset$mean_rating[idx])
      dropoff_std_dev_27 <- append(dropoff_std_dev_27, dataset$std_dev_dropoff[idx])
      workers_std_dev_27 <- append(workers_std_dev_27, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 28)){
      dropoff_mean_28 <- append(dropoff_mean_28, dataset$mean_rating[idx])
      dropoff_std_dev_28 <- append(dropoff_std_dev_28, dataset$std_dev_dropoff[idx])
      workers_std_dev_28 <- append(workers_std_dev_28, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 29)){
      dropoff_mean_29 <- append(dropoff_mean_29, dataset$mean_rating[idx])
      dropoff_std_dev_29 <- append(dropoff_std_dev_29, dataset$std_dev_dropoff[idx])
      workers_std_dev_29 <- append(workers_std_dev_29, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 30)){
      dropoff_mean_30 <- append(dropoff_mean_30, dataset$mean_rating[idx])
      dropoff_std_dev_30 <- append(dropoff_std_dev_30, dataset$std_dev_dropoff[idx])
      workers_std_dev_30 <- append(workers_std_dev_30, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 31)){
      dropoff_mean_31 <- append(dropoff_mean_31, dataset$mean_rating[idx])
      dropoff_std_dev_31 <- append(dropoff_std_dev_31, dataset$std_dev_dropoff[idx])
      workers_std_dev_31 <- append(workers_std_dev_31, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 32)){
      dropoff_mean_32 <- append(dropoff_mean_32, dataset$mean_rating[idx])
      dropoff_std_dev_32 <- append(dropoff_std_dev_32, dataset$std_dev_dropoff[idx])
      workers_std_dev_32 <- append(workers_std_dev_32, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 33)){
      dropoff_mean_33 <- append(dropoff_mean_33, dataset$mean_rating[idx])
      dropoff_std_dev_33 <- append(dropoff_std_dev_33, dataset$std_dev_dropoff[idx])
      workers_std_dev_33 <- append(workers_std_dev_33, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 34)){
      dropoff_mean_34 <- append(dropoff_mean_34, dataset$mean_rating[idx])
      dropoff_std_dev_34 <- append(dropoff_std_dev_34, dataset$std_dev_dropoff[idx])
      workers_std_dev_34 <- append(workers_std_dev_34, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 35)){
      dropoff_mean_35 <- append(dropoff_mean_35, dataset$mean_rating[idx])
      dropoff_std_dev_35 <- append(dropoff_std_dev_35, dataset$std_dev_dropoff[idx])
      workers_std_dev_35 <- append(workers_std_dev_35, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 36)){
      dropoff_mean_36 <- append(dropoff_mean_36, dataset$mean_rating[idx])
      dropoff_std_dev_36 <- append(dropoff_std_dev_36, dataset$std_dev_dropoff[idx])
      workers_std_dev_36 <- append(workers_std_dev_36, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 37)){
      dropoff_mean_37 <- append(dropoff_mean_37, dataset$mean_rating[idx])
      dropoff_std_dev_37 <- append(dropoff_std_dev_37, dataset$std_dev_dropoff[idx])
      workers_std_dev_37 <- append(workers_std_dev_37, dataset$std_dev[idx])
      
    }
    
    else if (as.numeric(dataset$max_workers[idx] == 38)){
      dropoff_mean_38 <- append(dropoff_mean_38, dataset$mean_rating[idx])
      dropoff_std_dev_38 <- append(dropoff_std_dev_38, dataset$std_dev_dropoff[idx])
      workers_std_dev_38 <- append(workers_std_dev_38, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 39)){
      dropoff_mean_39 <- append(dropoff_mean_39, dataset$mean_rating[idx])
      dropoff_std_dev_39 <- append(dropoff_std_dev_39, dataset$std_dev_dropoff[idx])
      workers_std_dev_39 <- append(workers_std_dev_39, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 40)){
      dropoff_mean_40 <- append(dropoff_mean_40, dataset$mean_rating[idx])
      dropoff_std_dev_40 <- append(dropoff_std_dev_40, dataset$std_dev_dropoff[idx])
      workers_std_dev_40 <- append(workers_std_dev_40, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 15)){
      dropoff_mean_15 <- append(dropoff_mean_15, dataset$mean_rating[idx])
      dropoff_std_dev_15 <- append(dropoff_std_dev_15, dataset$std_dev_dropoff[idx])
      workers_std_dev_15 <- append(workers_std_dev_15, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 14)){
      dropoff_mean_14 <- append(dropoff_mean_14, dataset$mean_rating[idx])
      dropoff_std_dev_14 <- append(dropoff_std_dev_14, dataset$std_dev_dropoff[idx])
      workers_std_dev_14 <- append(workers_std_dev_14, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 13)){
      dropoff_mean_13 <- append(dropoff_mean_13, dataset$mean_rating[idx])
      dropoff_std_dev_13 <- append(dropoff_std_dev_13, dataset$std_dev_dropoff[idx])
      workers_std_dev_13 <- append(workers_std_dev_13, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 12)){
      dropoff_mean_12 <- append(dropoff_mean_12, dataset$mean_rating[idx])
      dropoff_std_dev_12 <- append(dropoff_std_dev_12, dataset$std_dev_dropoff[idx])
      workers_std_dev_12 <- append(workers_std_dev_12, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 11)){
      dropoff_mean_11 <- append(dropoff_mean_11, dataset$mean_rating[idx])
      dropoff_std_dev_11 <- append(dropoff_std_dev_11, dataset$std_dev_dropoff[idx])
      workers_std_dev_11 <- append(workers_std_dev_11, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 10)){
      dropoff_mean_10 <- append(dropoff_mean_10, dataset$mean_rating[idx])
      dropoff_std_dev_10 <- append(dropoff_std_dev_10, dataset$std_dev_dropoff[idx])
      workers_std_dev_10 <- append(workers_std_dev_10, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 9)){
      dropoff_mean_9 <- append(dropoff_mean_9, dataset$mean_rating[idx])
      dropoff_std_dev_9 <- append(dropoff_std_dev_9, dataset$std_dev_dropoff[idx])
      workers_std_dev_9 <- append(workers_std_dev_9, dataset$std_dev[idx])
      
    }
    else if (as.numeric(dataset$max_workers[idx] == 8)){
      dropoff_mean_8 <- append(dropoff_mean_8, dataset$mean_rating[idx])
      dropoff_std_dev_8 <- append(dropoff_std_dev_8, dataset$std_dev_dropoff[idx])
      workers_std_dev_8 <- append(workers_std_dev_8, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 7)){
      dropoff_mean_7 <- append(dropoff_mean_7, dataset$mean_rating[idx])
      dropoff_std_dev_7 <- append(dropoff_std_dev_7, dataset$std_dev_dropoff[idx])
      workers_std_dev_7 <- append(workers_std_dev_7, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 6)){
      dropoff_mean_6 <- append(dropoff_mean_6, dataset$mean_rating[idx])
      dropoff_std_dev_6 <- append(dropoff_std_dev_6, dataset$std_dev_dropoff[idx])
      workers_std_dev_6 <- append(workers_std_dev_6, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 5)){
      dropoff_mean_5 <- append(dropoff_mean_5, dataset$mean_rating[idx])
      dropoff_std_dev_5 <- append(dropoff_std_dev_5, dataset$std_dev_dropoff[idx])
      workers_std_dev_5 <- append(workers_std_dev_5, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 4)){
      dropoff_mean_4 <- append(dropoff_mean_4, dataset$mean_rating[idx])
      dropoff_std_dev_4 <- append(dropoff_std_dev_4, dataset$std_dev_dropoff[idx])
      workers_std_dev_4 <- append(workers_std_dev_4, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 3)){
      dropoff_mean_3 <- append(dropoff_mean_3, dataset$mean_rating[idx])
      dropoff_std_dev_3 <- append(dropoff_std_dev_3, dataset$std_dev_dropoff[idx])
      workers_std_dev_3 <- append(workers_std_dev_3, dataset$std_dev[idx])
    }
    else if (as.numeric(dataset$max_workers[idx] == 2)){
      dropoff_mean_2 <- append(dropoff_mean_2, dataset$mean_rating[idx])
      dropoff_std_dev_2 <- append(dropoff_std_dev_2, dataset$std_dev_dropoff[idx])
      workers_std_dev_2 <- append(workers_std_dev_2, dataset$std_dev[idx])
    }
  }
}

for (idx in 1:20){ 
  print(as.name(paste("dropoff_mean",idx, sep="_")))
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
{

worker1_mean_dif <- c()
worker2_mean_dif <- c()
worker3_mean_dif <- c()
worker4_mean_dif <- c()
worker5_mean_dif <- c()
worker6_mean_dif <- c()
worker7_mean_dif <- c()
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


worker1_sd_dif <- c()
worker2_sd_dif <- c()
worker3_sd_dif <- c()
worker4_sd_dif <- c()
worker5_sd_dif <- c()
worker6_sd_dif <- c()
worker7_sd_dif <- c()
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
      
      if (as.numeric(dataset$max_workers[idx]) == 2){
        worker2_mean_dif <- append(worker2_mean_dif, dataset$mean_difference[idx])
        worker2_sd_dif <- append(worker2_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 3){
        worker3_mean_dif <- append(worker3_mean_dif, dataset$mean_difference[idx])
        worker3_sd_dif <- append(worker3_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 4){
        worker4_mean_dif <- append(worker4_mean_dif, dataset$mean_difference[idx])
        worker4_sd_dif <- append(worker4_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 5){
        worker5_mean_dif <- append(worker5_mean_dif, dataset$mean_difference[idx])
        worker5_sd_dif <- append(worker5_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 6){
        worker6_mean_dif <- append(worker6_mean_dif, dataset$mean_difference[idx])
        worker6_sd_dif <- append(worker6_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 7){
        worker7_mean_dif <- append(worker7_mean_dif, dataset$mean_difference[idx])
        worker7_sd_dif <- append(worker7_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 8){
        worker8_mean_dif <- append(worker8_mean_dif, dataset$mean_difference[idx])
        worker8_sd_dif <- append(worker8_sd_dif, dataset$sd_difference[idx])
      }
      else if (as.numeric(dataset$max_workers[idx]) == 9){
        worker9_mean_dif <- append(worker9_mean_dif, dataset$mean_difference[idx])
        worker9_sd_dif <- append(worker9_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 10){
        worker10_mean_dif <- append(worker10_mean_dif, dataset$mean_difference[idx])
        worker10_sd_dif <- append(worker10_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 11){
        worker11_mean_dif <- append(worker11_mean_dif, dataset$mean_difference[idx])
        worker11_sd_dif <- append(worker11_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 12){
        worker12_mean_dif <- append(worker12_mean_dif, dataset$mean_difference[idx])
        worker12_sd_dif <- append(worker12_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 13){
        worker13_mean_dif <- append(worker13_mean_dif, dataset$mean_difference[idx])
        worker13_sd_dif <- append(worker13_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 14){
        worker14_mean_dif <- append(worker14_mean_dif, dataset$mean_difference[idx])
        worker14_sd_dif <- append(worker14_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 15){
        worker15_mean_dif <- append(worker15_mean_dif, dataset$mean_difference[idx])
        worker15_sd_dif <- append(worker15_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 16){
        worker16_mean_dif <- append(worker16_mean_dif, dataset$mean_difference[idx])
        worker16_sd_dif <- append(worker16_sd_dif, dataset$sd_difference[idx])
      } 
      
      else if (as.numeric(dataset$max_workers[idx]) == 17){
        worker17_mean_dif <- append(worker17_mean_dif, dataset$mean_difference[idx])
        worker17_sd_dif <- append(worker17_sd_dif, dataset$sd_difference[idx])
      }
      
      else if (as.numeric(dataset$max_workers[idx]) == 18){
        worker18_mean_dif <- append(worker18_mean_dif, dataset$mean_difference[idx])
        worker18_sd_dif <- append(worker18_sd_dif, dataset$sd_difference[idx])
        
      }  
      else if (as.numeric(dataset$max_workers[idx]) == 19){
        worker19_mean_dif <- append(worker19_mean_dif, dataset$mean_difference[idx])
        worker19_sd_dif <- append(worker19_sd_dif, dataset$sd_difference[idx])
      }
      else if (as.numeric(dataset$max_workers[idx]) == 20){
        worker20_mean_dif <- append(worker20_mean_dif, dataset$mean_difference[idx])
        worker20_sd_dif <- append(worker20_sd_dif, dataset$sd_difference[idx])
      }
    }
}
}

}

#calculate how many specific reasons
ja_reasons <- c()
zh_reasons <- c()
no_reasons <- c()
es_reasons <- c()
en_reasons <- c()


for (idx in 1:length(dataset[, 1])){
  if (!is.na((dataset$language[idx]))){
    
    if (dataset$language[idx] == "ja"){
      split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
      split <- c(as.numeric(strsplit(split, ",")[[1]]))
      for (jdx in 1:length(split)){
        if (!is.na((split[jdx]))){
          ja_reasons <- append(ja_reasons, split[jdx]) 
        }
      }
    }
    if (dataset$language[idx] == "zh"){
      split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
      split <- c(as.numeric(strsplit(split, ",")[[1]]))
      for (jdx in 1:length(split)){
        if (!is.na((split[jdx]))){
          zh_reasons <- append(zh_reasons, split[jdx]) 
        }
      }
    }
    if (dataset$language[idx] == "no"){
      split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
      split <- c(as.numeric(strsplit(split, ",")[[1]]))
      for (jdx in 1:length(split)){
        if (!is.na((split[jdx]))){
          no_reasons <- append(no_reasons, split[jdx]) 
        }
      }
    }
    if (dataset$language[idx] == "es"){
      split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
      split <- c(as.numeric(strsplit(split, ",")[[1]]))
      for (jdx in 1:length(split)){
        if (!is.na((split[jdx]))){
          es_reasons <- append(es_reasons, split[jdx]) 
        }
      }
    }
    if (dataset$language[idx] == "en"){
      split<- gsub("\n", ",", dataset$followup1[idx], fixed = TRUE)
      split <- c(as.numeric(strsplit(split, ",")[[1]]))
      for (jdx in 1:length(split)){
        if (!is.na((split[jdx]))){
          en_reasons <- append(en_reasons, split[jdx]) 
        }
      }
    }
  }
}

ja_table <- table(ja_reasons)
zh_table <- table(zh_reasons)
no_table <- table(no_reasons)
es_table <- table(es_reasons)
en_table <- table(en_reasons)


#
  vector <- c()
  split <- strsplit(dataset$merged_ratings[162], ",")[[1]]
  
  for (jdx in 1:length(split)) {
    vector <- append(vector, as.numeric(split[jdx]))
  }
  
  std_dev_max <- sd (vector)
  vector <- vector[-length(vector)]
  vector <- vector[-length(vector)]
  std_dev_max_minus_2 <- sd (vector)
  
  dataset$std_dev_dropoff[idx] <- abs(std_dev_max - std_dev_max_minus_2)


for (jdx in 1:length(split)) {
vector <- vector[-length(vector)]
cat( sd(vector), "\n")
}

#mean containers histogram
  mean_1_5 <- 0
  mean_2_0 <- 0
  mean_2_5 <- 0
  mean_3_0 <- 0
  mean_3_5 <- 0
  mean_4_0 <- 0
  
  
  
  for (idx in 1:length(dataset[, 1])) {
    
    if (!is.na(as.numeric(dataset$mean_rating[idx]))){
      if (as.numeric(dataset$mean_rating[idx] < 1.5)){
        mean_1_5 <- mean_1_5 + 1
      }  
      else if (as.numeric(dataset$mean_rating[idx] < 2)){
        mean_2_0 <- mean_2_0 + 1
      }  
      else if (as.numeric(dataset$mean_rating[idx] < 2.5)){
        mean_2_5 <- mean_2_5 + 1
      }  
      else if (as.numeric(dataset$mean_rating[idx] < 3)){
        mean_3_0 <- mean_3_0 + 1
      }  
      else if (as.numeric(dataset$mean_rating[idx] < 3.5)){
        mean_3_5 <- mean_3_5 + 1
      }  
      else if (as.numeric(dataset$mean_rating[idx] < 4)){
        mean_4_0 <- mean_4_0 + 1
      }  
    }}
  
#sd containers histogram
  sd_0_5 <- 0
  sd_1_0 <- 0
  sd_1_5 <- 0
  sd_2_0 <- 0
  sd_2_5 <- 0

  
  
  for (idx in 1:length(dataset[, 1])) {
    
    if (!is.na(as.numeric(dataset$std_dev[idx]))){
      if (as.numeric(dataset$std_dev[idx] < 0.5)){
        sd_0_5 <- sd_0_5 + 1
      }  
      else if (as.numeric(dataset$std_dev[idx] < 1)){
        sd_1_0 <- sd_1_0 + 1
      }  
      else if (as.numeric(dataset$std_dev[idx] < 1.5)){
        sd_1_5 <- sd_1_5 + 1
      }  
      else if (as.numeric(dataset$std_dev[idx] < 2)){
        sd_2_0 <- sd_2_0 + 1
      }  
      else if (as.numeric(dataset$std_dev[idx] < 2.5)){
        sd_2_5 <- sd_2_5 + 1
      }  
      
    }
    }
  
#### ONLY USE SENTENCES OF 20 WORKERS FOR DROPOFF
  for (idx in 2:20){ 
    assign(paste("rel_worker_mean",idx, sep="_"), (c(NULL)))
    assign(paste("rel_worker_sd",idx, sep="_"), (c(NULL)))
    
  }
  
  
  for (idx in 1:length(dataset[, 1])) {
    append_list <- c(NULL)
    if (!is.na(as.numeric(dataset$max_workers[idx]))){
      if (as.numeric(dataset$max_workers[idx] == 20)){
        split <- strsplit(dataset$how_natural[idx], ",")[[1]]
        
        for (jdx in 1:length(split)){
          append_list <- append(append_list, as.numeric(split[jdx]))
        }
        
        rel_worker_mean_20 <- append (rel_worker_mean_20, mean(append_list))
        rel_worker_sd_20 <- append (rel_worker_sd_20, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_19 <- append (rel_worker_mean_19, mean(append_list))
        rel_worker_sd_19 <- append (rel_worker_sd_19, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_18 <- append (rel_worker_mean_18, mean(append_list))
        rel_worker_sd_18 <- append (rel_worker_sd_18, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_17 <- append (rel_worker_mean_17, mean(append_list))
        rel_worker_sd_17 <- append (rel_worker_sd_17, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_16 <- append (rel_worker_mean_16, mean(append_list))
        rel_worker_sd_16 <- append (rel_worker_sd_16, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_15 <- append (rel_worker_mean_15, mean(append_list))
        rel_worker_sd_15 <- append (rel_worker_sd_15, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_14 <- append (rel_worker_mean_14, mean(append_list))
        rel_worker_sd_14 <- append (rel_worker_sd_14, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_13 <- append (rel_worker_mean_13, mean(append_list))
        rel_worker_sd_13 <- append (rel_worker_sd_13, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_12 <- append (rel_worker_mean_12, mean(append_list))
        rel_worker_sd_12 <- append (rel_worker_sd_12, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_11 <- append (rel_worker_mean_11, mean(append_list))
        rel_worker_sd_11 <- append (rel_worker_sd_11, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_10 <- append (rel_worker_mean_10, mean(append_list))
        rel_worker_sd_10 <- append (rel_worker_sd_10, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_9 <- append (rel_worker_mean_9, mean(append_list))
        rel_worker_sd_9 <- append (rel_worker_sd_9, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_8 <- append (rel_worker_mean_8, mean(append_list))
        rel_worker_sd_8 <- append (rel_worker_sd_8, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_7 <- append (rel_worker_mean_7, mean(append_list))
        rel_worker_sd_7 <- append (rel_worker_sd_7, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_6 <- append (rel_worker_mean_6, mean(append_list))
        rel_worker_sd_6 <- append (rel_worker_sd_6, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_5 <- append (rel_worker_mean_5, mean(append_list))
        rel_worker_sd_5 <- append (rel_worker_sd_5, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_4 <- append (rel_worker_mean_4, mean(append_list))
        rel_worker_sd_4 <- append (rel_worker_sd_4, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_3 <- append (rel_worker_mean_3, mean(append_list))
        rel_worker_sd_3 <- append (rel_worker_sd_3, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
        rel_worker_mean_2 <- append (rel_worker_mean_2, mean(append_list))
        rel_worker_sd_2 <- append (rel_worker_sd_2, sd(append_list))
        append_list <- append_list [-length(append_list)]
        
      }
    }
  }
      
#exporting dataset as csv
write.csv(dataset, file = "MOP2_final.csv",row.names=FALSE, na="")
write.csv(dataset, file = "MOP4_final_mergemop2.csv",row.names=FALSE, na="")

