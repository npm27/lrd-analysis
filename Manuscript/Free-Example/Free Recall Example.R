##Set up
library(lrd)

dat = read.csv("free_example_data.csv")
key = read.csv("free_example_key.csv")

arranged = arrange_data(dat, responses = "Response", id = "Sub.ID", sep = ", ")
