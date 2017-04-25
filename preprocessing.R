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

# populate data frame
for (idx in 1:length(dataset[, 1])) {
  split <- strsplit(dataset$rating_list[idx], ",")[[1]]
  dataset$max_workers[idx] <- length(split)
  for (jdx in 1:length(split)) {
    dataset[idx, paste("worker", jdx, sep = "_")] <- as.numeric(split[jdx])
  }
}
