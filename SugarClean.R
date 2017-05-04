
#Rum project: data cleaning script 2
#Fall 2016
#Peer Christensen

# This script takes a csv consisting of data from a list of rums and sugar contents
# cleans the data and creates an improved csv file

setwd("/Users/peerchristensen/Desktop/rum project")
rums = read.csv("rumlist.csv", header=T,stringsAsFactors = F,sep="\t")
rums=as.vector(t(rums))
rums = gsub("^....","",rums) #look up regular expressions if unclear
sugar = regmatches(rums,gregexpr(":...",rums))
sugar = as.numeric(gsub("[^0-9]","",sugar))
rums=sub("(.*?):.*", "\\1", rums)

df=as.data.frame(sugar)
df$label=rums
df=df[,c(2,1)]

#delete duplicate rows, remove NA and "ron, set =< 2 to 0, alph. order
df=na.omit(df)
df$label=gsub("Ron ","",df$label)
df= df[!duplicated(df), ]
df$sugar[df$sugar<=2] <- 0
df=df[order(df$label),]

hist(df$sugar,breaks=length(unique(df$sugar)),col="red")

#write csv
write.table(df,"rum_sugar.txt",sep="\t")
write.csv2(df,"rum_sugar.csv",sep=";")