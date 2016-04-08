install.packages("XML")
install.packages("plyr")
install.packages("ggplot2")
install.packages("gridExtra")

require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")

#First thing that we have to do is to clean up the data a bit 
#In particular, we have to strip out the ">=" and change to = 

txt <- gsub("type(speculation|notspeculation)>=", "type=", 
paste(readLines("C:/Users/jwillisc/PowerFolders/R Stuff/Stuff for Nicole/New_Learn_NB.xml"), collapse = "\n"), fixed=TRUE) #This gets rid of the >=

doc <- xmlParse(txt) #this puts it back into XML

xmltop = xmlRoot(doc) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltop) #give name of node, PubmedArticleSet
xmlSize(xmltop) #how many children in node, 19
xmlName(xmltop[[1]]) #name of root's children
xmlName(xmltop[[2]]) #name of root's children

xmltop[[1]]
xmltop[[2]]

xmlSize(xmltop[[2]]) #number of nodes in each child
xmlSApply(xmltop[[2]], xmlName) #name(s)
dat <- as.data.frame(xmlSApply(xmltop[[2]], xmlAttrs)) #attribute(s)
dat[1:2,] 

write.csv2(dat, "test_xml.csv") #If we want to save the data 

# Next step - Want to turn this into a datafile (Do you want me to put all of this into a function?)



#transpose data into three frames for Hedges, Words and Notes to better handle variables
dat_H <- as.data.frame(t(dat[colnames(dat)=="HEDGE"])[-186,],stringsAsFactors = F)##tmp:remove line 186 H185: non-increasing position of end point
dat_W <- as.data.frame(t(dat[colnames(dat)=="WORD"]),stringsAsFactors = F)
dat_N <- as.data.frame(t(dat[colnames(dat)=="NOTE"]),stringsAsFactors = F)

# Prepare Trigger Words 
#
#tmp:remove line 186 because it is not monotonically increasing in the sentence end-point
#create intervals for endpoints of HEDGES (note that findInterval() treats the first interval break as exclusive, therefore +1

v <- as.integer(dat_H$end)+1
#and WORDS in 
x <- as.integer(dat_W$end)

#is vector v monotonically increasing?
#all(v == cummax(v)) 
#at which position is the break?
#which.max( v < cummax(v) ) which

##create index indicating to which hedge each trigger word belongs (there is a HEDGE 0 therefore +1)
w_index <- findInterval(x,v)+1

#create empty list to hold lists of trigger words for each HEDGE
w<-rep(list(NA),nrow(dat_H))

#assign indexed Trigger Words to n (e.g. HEDGE 16 is non-existent, and some HEDGES do not have any trigger words)
w[unique(w_index)]<- tapply(dat_W$text,w_index,as.list)


# Prep. Notes - similar to words but simpler because there is only one note per sentence

v <- as.integer(dat_H$end)+1
x <- as.integer(dat_N$end)

##create index indicating to which hedge each trigger word belongs
n_index <- findInterval(x,v)+1

#create empty list to hold lists of trigger words for each HEDGE
n<-rep(NA,nrow(dat_H))

#assign indexed Trigger Words to n (e.g. HEDGE 16 is non-existent, and some HEDGES do not have any trigger words)
n[unique(n_index)]<- dat_N$type



# Create Data-Frame 
XML_DATA <- data.frame(
  ID=rep(1,nrow(dat_H)) # I don't know yet how you want to handle meeting ID's yet
  ,Meeting=rep(1,nrow(dat_H)) # I don't know yet how you want to handle this
  ,Sent_No = as.numeric(gsub("(H)(\\d)","\\2",dat_H$id)) # Take Digit After ID Var from HEDGE ID
  ,Hedge_ID= paste("[",dat_H$start,"-",dat_H$end,"]") # Create character vector indicating position of sentence
  ,Speculation= paste(dat_H$type) #Create Character vector indicating handcoded annotation
  ,Notes=n #add notes from above
  ,stringsAsFactors = F
  )
XML_DATA$Words <- w # to incorporate nested list structure this needs to go outside for some reason
