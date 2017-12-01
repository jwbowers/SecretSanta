### This script randomly assigns people into pairs (one secret santa and one gift recipient) across families
### It also sends an email to each santa informing them about their gift recipient.

## Read in data from Google Sheets
install.packages("googledrive")
library("googledrive")
library(dplyr)
library(purrr)
library(readr)
library(gmailr)

drive_download("SecretSanta2017",type="csv",overwrite=TRUE)
origdat <- read.csv("SecretSanta2017.csv",as.is=TRUE)
## Remove non-participants
## the base R way: dat <- dat[dat$Participant=="Yes",]
## The ddplyr way
dat <- filter(origdat,Participant=="Yes")

# Using Mari's algorithm
## Pull out vectors for family and people just to make the code easier to read below
thefamily <- dat$Family
uniqname <- dat$UniqName

santas <- vector(length=length(uniqname))
## Start with the first person
set.seed(12345)
santas[1] <- sample( uniqname[ thefamily != thefamily[1] ],size=1 )
for(s in 2:length(uniqname)){
  santas[s] <- sample( uniqname[ ( thefamily != thefamily[s] ) & !(uniqname %in% santas) ], size=1 )
}

dat$santa <- santas

thebody <- "Dear %s,

You are the secret santa for %s. You can use the attached Google Sheet to learn more about your gift recipient (https://drive.google.com/open?id=1hM0HBpSu_KzOfF9kc75MvNvfgrVHnvngF1XJ_1Yg3T4).

We will be exchanging our gifts on December 23 at Leah, Brock, Lucy, Tyler, and Elle's house.

Love,

Jake and Mari
"

#testdat <- filter(dat,Family=="BowersWong")
#testdat <- droplevels(testdat[-3,])

edat<-dat %>% mutate(To=sprintf('%s <%s>',UniqName,Email),
                        From='Jake Bowers <jake@jakebowers.org>',
                        Subject='Secret Santa Assignment',
                        body=sprintf(body,UniqName,santa)) %>% select(To,From,Subject,body)

write_csv(edat, "composed-emails.csv")
emails <- edat %>% pmap(mime)
str(emails, max.level = 2, list.len = 2)

## This next line sends all of the emails.
sent_mail <- emails %>% map(send_message)

# famdist<-outer(family,family,function(x,y){ x==y })
# dimnames(famdist)<-list(people,people)
# pdist<-outer(people,people,function(x,y){ paste(x," to ",y,sep="") })
# dimnames(pdist)<-list(from=people, to=people)
#
# ## A slow ugly greedy algorithmn
# thesource<-1:length(people)
# res<-vector(length=length(people))
# for(i in 1:length(people)){
#   thesamp<-sample(thesource[!famdist[i,] & !is.na(thesource)],1)
#   res[i]<-pdist[i,thesamp]
#   thesource[thesource==thesamp]<-NA
# }
#
# write.csv(res,file="xmas2017.csv")
