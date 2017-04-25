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
