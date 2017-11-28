
##set.seed(20151225)
##people<-c("Fair","Brent","Cara","Jake","Mari","Cosmo","Ben","Alex","Lisa","Andy","Cole")
##family<-c(1,1,2,2,2,2,3,3,4,4,4)
## Read in data from Google Sheets 
library("googledrive")
sheetnm <-  drive_get("~/SecretSanta2007"))
dat <- drive_get(as_id(sheetnm$id))
## Remove non-participants
dat <- dat[dat$Participant=="Yes",]
## Assign the vectors for family and people
family <- dat$Family
people <- dat$Name

famdist<-outer(family,family,function(x,y){ x==y })
dimnames(famdist)<-list(people,people)
pdist<-outer(people,people,function(x,y){ paste(x," to ",y,sep="") })
dimnames(pdist)<-list(from=people, to=people)

## A slow ugly greedy algorithmn
thesource<-1:length(people)
res<-vector(length=length(people))
for(i in 1:length(people)){
  thesamp<-sample(thesource[!famdist[i,] & !is.na(thesource)],1)
  res[i]<-pdist[i,thesamp]
  thesource[thesource==thesamp]<-NA
}

write.csv(res,file="xmas2017.csv")



