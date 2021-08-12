####Need to add back in the list items that participants did not recall and score them as 0
temp = read.csv("M3.csv") #swap out the input .csv here and make sure to change file name on last line!

library(RecordLinkage)
library(readxl)
library(lrd)
library(sjmisc)

#read in the answer key
keyA = readxl::read_excel("longkeys.xlsx", sheet = "Version A")
keyB = readxl::read_excel("longkeys.xlsx", sheet = "Version B")

table(keyA$Key_Item)
table(keyB$Key_Item)

##remove spaces
keyA = apply(keyA, 2, function(x)gsub(' ', '', x))
keyA = as.data.frame(keyA)

keyB = apply(keyB, 2, function(x)gsub(' ', '', x))
keyB = as.data.frame(keyB)

##AND LOWER CASE
keyA$Key_Item = tolower(keyA$Key_Item)
keyB$Key_Item = tolower(keyB$Key_Item)

##I think the best way to go about this would be to split on version
##start w/ M0
ma = subset(temp,
             temp$Version == "A" |temp$Version == "C" | temp$Version == "E")
mb = subset(temp,
             temp$Version == "B" | temp$Version == "D" | temp$Version == "F")


##Okay, lets give a try
length(unique(ma$Sub.ID)) ##We have the 60 participants \who completed this set of lists

l2 = na.omit(ma)

l1.key = keyA$Key_Item

ids = c()
items = c()
scores = c()

#x = str_contains("windmill", l1.key)
#length(table(x)) == 1

for (p in unique(l2$Sub.ID)){

  temp = subset(l2,
                l2$Sub.ID == p)

  for (i in l1.key){

    x = str_contains(i, temp$Response)

    print(i)

    print(table(x))

    if (length(table(x)) == 1){

      val = FALSE

      scores = c(scores, val)

    } else {

      val = TRUE

      scores = c(scores, val)

    }

    items = c(items, i)

    ids = c(ids, p)

  }

}

testoutput = data.frame(ids, items, scores)

output_A2 = testoutput[order(testoutput$items),]
output_A2 = output_A2[order(output_A2$ids),]
output_A2$list = rep("2")

##Now for version B
length(unique(mb$Sub.ID)) ##We have the 60 participants \who completed this set of lists

l2 = na.omit(mb)

l1.key = keyB$Key_Item

ids = c()
items = c()
scores = c()

#x = str_contains("windmill", l1.key)
#length(table(x)) == 1

for (p in unique(l2$Sub.ID)){

  temp = subset(l2,
                l2$Sub.ID == p)

  for (i in l1.key){

    x = str_contains(i, temp$Response)

    print(i)

    print(table(x))

    if (length(table(x)) == 1){

      val = FALSE

      scores = c(scores, val)

    } else {

      val = TRUE

      scores = c(scores, val)

    }

    items = c(items, i)

    ids = c(ids, p)

  }

}

testoutput = data.frame(ids, items, scores)

output_B2 = testoutput[order(testoutput$items),]
output_B2 = output_B2[order(output_B2$ids),]
output_B2$list = rep("1")

#Combine at write to file
#write.csv(rbind(output_A2, output_B2), file = "processed_m3.csv", row.names = F)
