##Set up
library(lrd)

dat = read.csv("free_example_data.csv")
key = read.csv("free_example_key.csv")

##Convert from wide to long
arranged = arrange_data(dat, responses = "Response", id = "Sub.ID", sep = ", ")

##make sure everything is lowercase
arranged$response = tolower(arranged$response)
key$Answer_Key = tolower(key$Answer_Key)

##Score free recall data
scored_free = prop_correct_free(arranged,
                                responses = "response",
                                key = key$Answer_Key,
                                id = "Sub.ID",
                                cutoff = 1,
                                group.by = "Disease.Condition")

scored_free$DF_Scored #Trial level

scored_free$DF_Participant #Participant level

scored_free$DF_Group #Group level
