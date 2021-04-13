####Set up####
library(lrd)

dat = read.csv("sentences.csv")

scored_sentences = prop_correct_sentence(dat, responses = "Response", key = "Sentence",
                                         key.trial = "Trial.ID", id = "Sub.ID", id.trial = "Trial.ID",
                                         cutoff = 0, token.split = " ", group.by = "Condition")

scored_sentences$DF_Scored ##Trial level data

scored_sentences$DF_Participant ##Participant level

scored_sentences$DF_Group ##Group level
