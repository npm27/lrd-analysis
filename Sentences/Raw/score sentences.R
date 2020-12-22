####Validate the sentence functions####
library(lrd)

dat = read.csv("AzBio_ScoreTemp_AH.csv")

#isolate the participant ids, sentences, and responses
dat2 = dat[ , c(1, 3, 4)]

#how many participants?
length(unique(dat2$Participant.Private.ID)) #101

#how many sentences?
length(unique(dat2$sentence)) #21

##score the things!
##get better column names
colnames(dat2)[1] = "Sub.id"

##Need to make a trial id
#first, remove the blank sentences
dat2 = subset(dat2,
              dat2$sentence != "")

##Recheck n
#how many participants?
length(unique(dat2$Sub.id)) #100

#how many sentences?
length(unique(dat2$sentence)) #20

##Okay, 100 participants each studied 20 sentences

#Make the trial id column
dat2$trial_id = rep(1:20)

#replace NAs with blanks

dat2[is.na(dat2)] = " "

scored_sentences = prop_correct_sentence(dat2, responses = "Response", key = "sentence",
                                         key.trial = "trial_id", id = "Sub.id", id.trial = "trial_id",
                                         cutoff = 5, token.split = " ")

scored_sentences$DF_Scored
scored_sentences$DF_Participant

#write to .csv
#write.csv(scored_sentences$DF_Scored, file = "sentences_5.csv", row.names = F)
#write.csv(scored_sentences$DF_Participant, file = "sentences_5_participant.csv", row.names = F)
