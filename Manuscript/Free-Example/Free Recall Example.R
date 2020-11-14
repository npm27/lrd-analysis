##Set up
library(lrd)

dat = read.csv("free_example_data.csv")
key = read.csv("free_example_key.csv")

##Convert from wide to long
arranged = arrange_data(dat, responses = "Response", id = "Sub.ID", sep = ", ")

##Score free recall data
scored_free = prop_correct_free(arranged,
                                responses = "response",
                                key = key$Answer_Key,
                                id = "Sub.ID",
                                cutoff = 1)

View(scored_free$DF_Scored)
