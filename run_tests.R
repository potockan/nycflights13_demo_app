library(testthat) 

source("functions.R")

test_results <- test_dir(".", reporter="summary")

