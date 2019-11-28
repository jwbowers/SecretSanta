### This script randomly assigns people into pairs (one secret santa and one gift recipient) across families
### It also sends an email to each santa informing them about their gift recipient.
## Using guidance from https://jennybc.github.io/purrr-tutorial/ex20_bulk-gmail.html

## Read in data from Google Sheets
## install.packages("googledrive")
library("googledrive")
library(dplyr)
library(purrr)
library(readr)
library(gmailr)

##drive_download("SecretSanta2019",type="csv",overwrite=TRUE)
origdat <- read.csv("SecretSanta2019.csv",as.is=TRUE)
## Remove non-participants
## the base R way: dat <- dat[dat$Participant=="Yes",]
## The ddplyr way
dat <- filter(origdat,Participant=="Yes")

# Using Mari's algorithm
## Pull out vectors for family and people just to make the code easier to read below
thefamily <- dat$Family
uniqname <- dat$UniqName

santas <- vector(length=length(uniqname))
names(santas)<-uniqname
## Start with the first person
set.seed(123492019)
santas[1] <- sample( uniqname[ thefamily != thefamily[1] ],size=1 )
for(s in 2:length(uniqname)){
  santas[s] <- sample( uniqname[ ( thefamily != thefamily[s] ) & !(uniqname %in% santas) ], size=1 )
}

dat$santa <- santas

write.csv(dat,file="dat.csv")

thebody <- "Dear %s,

You are the secret santa for %s. You can use the attached Google Sheet to learn more about your gift recipient (https://docs.google.com/spreadsheets/d/10Db03keZZZQ1nfwurJaV9GY4weBU6jOv6_lhxXWnzfg/edit?usp=sharing).

Love,

Jake and Mari
"

testdat <- filter(dat,Family=="BowersWong")
testdat <- droplevels(testdat[-3,])

edat <- dat %>% mutate(to = sprintf('%s <%s>',UniqName,Email),
                        from = 'Jake Bowers <jake@jakebowers.org>',
                        subject = 'Secret Santa Assignment 2019',
                        body = sprintf(thebody,UniqName,santa)) %>% select(to,from,subject,body)

write_csv(edat, "composed-emails.csv")
library(googleAuthR)
options("googleAuthR.scopes.selected" = "email")


##edat <- read.csv("composed-emails.csv")
emails1 <- edat[1:15,] %>% pmap(mime)
emails2 <- edat[16:25,] %>% pmap(mime)
##str(emails, max.level = 2, list.len = 2)

## This next to avoid problems with single message failures
safe_send_message <- safely(gm_send_message)

## This next line sends all of the emails.
sent_mail1 <- emails1 %>% map(safe_send_message)
sent_mail2 <- emails2 %>% map(safe_send_message)


saveRDS(sent_mail1,"sent-emails1.rds")
saveRDS(sent_mail2,"sent-emails2.rds")

errors2<- sent_mail2 %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail2[errors2]


errors1<- sent_mail1 %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail1[errors1]




### Old Algorithm
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
