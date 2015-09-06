#SCRIPT for FETCHING Facebook Data

library(Rfacebook)
library(httr) 
library(plyr)
library(stringr)

token <- "CAACEdEose0cBALCejtInIvgSPUMbE0RWgfITidmOSaHRFlDqkxhpp2xfSz8RZCZB26EquaW2pIxPMX1eKzUmxLwdepFCsopN6iSIo5KIKNRKztvJo9ISMJ0ylRZCnBM7BWpG73iHUpkn8pdKjZAaZBHWFVeLmN2ixOQ3jPkm9eevCN8BsfAsJVqiT09H6NaTfxeVSNUyHQQZDZD"
d <- getFQL("SELECT  text FROM comment WHERE object_id='10151337184825667' AND parent_id='0'  ORDER BY time", token)
x1 <- length(d)
for(i in 1:x1)
{ 
 	x[i] = d[[i]]
}
x = x[1:x1]	
lapply(x, write, "DATA.txt", append=TRUE, ncolumns=1000)   

#read textfile and dictionary from  data 
Text <- read.delim(file='DATA.txt', header=FALSE, stringsAsFactors=FALSE)
Text <- Text$V1
Text <- unlist(lapply(Text, function(x) { str_split(x, "\n") }))
dictionary <- read.delim(file='worddictionary.txt', header=FALSE, stringsAsFactors=FALSE)
names(dictionary) <- c('word', 'score')
dictionary$word <- tolower(dictionary$word)    
 
#differentiate  words acording to score 
vNegTerms <- dictionary$word[dictionary$score==-5 | dictionary$score==-4]
negTerms <- c(dictionary$word[dictionary$score==-3 | dictionary$score==-2 | dictionary$score==-1])
posTerms <- c(dictionary$word[dictionary$score==3 | dictionary$score==2 | dictionary$score==1])
vPosTerms <- c(dictionary$word[dictionary$score==5 | dictionary$score==4])    
 
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
final_scores <- matrix('', 0, 5)
scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
initial_sentence <- sentence
    #remove unnecessary characters  
	sentence <- gsub('[[:punct:]]', '', sentence)
	sentence <- gsub('[[:cntrl:]]', '', sentence)
	sentence <- gsub('\\d+', '', sentence)
	sentence <- tolower(sentence)
	wordList <- str_split(sentence, '\\s+')
	words <- unlist(wordList)
	vPosMatches <- match(words, vPosTerms)
    	posMatches <- match(words, posTerms)
    	vNegMatches <- match(words, vNegTerms)
    	negMatches <- match(words, negTerms)
    	vPosMatches <- sum(!is.na(vPosMatches))
    	posMatches <- sum(!is.na(posMatches))
    	vNegMatches <- sum(!is.na(vNegMatches))
    	negMatches <- sum(!is.na(negMatches))
    	score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    	newrow <- c(initial_sentence, score)
    	final_scores <- rbind(final_scores, newrow)
    	return(final_scores)
  	}, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    
 
posResult <- as.data.frame(sentimentScore(Text, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
results <- rbind(posResult)
results$sentiment1 = 1
for(i in 1:length(results))
{
if(as.numeric(results[i,2]) + as.numeric(results[i,3]) > as.numeric(results[i,4]) + as.numeric(results[i,5]))
results[i,7] = 0
else
results[i,7] = 1
}
#results$sentiment = NULL
results$sentiment1 = gsub(1, "POSITIVE", results$sentiment1)
results$sentiment1 = gsub(0, "NEGATIVE", results$sentiment1)
results$sentiment = NULL
write.csv(results, "DATA1.csv", row.names = FALSE)









