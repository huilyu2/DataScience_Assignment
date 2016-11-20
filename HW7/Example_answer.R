# The data are available on the Web at 
# http://www.stat.berkeley.edu/users/nolan/stat133/data/stateoftheunion1790-2012.txt.zip

#### Person A: Preparation
### Download the file and unzip it.
### Use readLines() to read this plain text file into R and assign it to speeches

speeches = readLines(file("C:/Documents/GSLIS/490 Introduction to Data Science/Data and Code/stateoftheunion1790-2012.txt"))

### Use grep and regular expressions to determine which 
### lines of speeches have the three asterisks in them. 

breaks = grep("\\*\\*\\*", speeches)

### Use breaks to identify the elements in speeches (which correspond to lines in the file
###	that have the date of the speech.  
### Place these dates in a character vector called tempDates.

tempDates = as.character(speeches[breaks + 4])
tempDates = tempDates[c(1:222)]

### Use gregexpr to identify the location of the year in 
### each tempDates. Then use substr() to extract the year 
### Convert the year to numeric. Call it speechYr

yrLocs = as.numeric((gregexpr("[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", tempDates)))
speechYr = as.numeric(substr(tempDates, yrLocs, yrLocs + 4))

### Use gsub to extract the month of the speech from tempDates. 
### Convert the month to numeric. 
### Assign it to a vector called speechMo.

speechMo = gsub("[[:blank:]][[:digit:]]+[,][[:blank:]][[:digit:]]+", "", tempDates)

speechMo = as.numeric(sapply(speechMo, switch, "January" = 1, "February" = 2, "March" = 3, "April" = 4,
                             "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9,
                             "October" = 10, "November" = 11, "December" = 12))


###  Use breaks to extract the name of the president from speeches
###  Assign the names to the character vector presidents. 

presidents = speeches[breaks + 3]
presidents = presidents[c(1:222)]

### Cut up the speeches vector according to speech, 
### and place each speech into an element of the list.
### Call the list speechesL.
### Each element of the list should be a character vector
### Each element in the character vector should correspond
### to a sentence in the speech (not a line in the file).
### To create this character vector, you probably want to 
### first collapse all of the lines of text for one speech 
### into one long line and then split up this line according 
### to the appropriate literals that mark the end of a sentence. 
### Don't worry about the first sentence if it gets lumped in
### with the greeting.

#combine is a list of all the speeches
combine = list()
for (i in 1:221){
  combine[[i]] = paste(speeches[(breaks[i]+6):breaks[i+1]-1], sep=" ", collapse = " ")
}

#handle last speech 
combine[[222]] = paste(speeches[(breaks[222]+6):breaks[222+1]-1], sep=" ", collapse = " ")

#split each speech by separate sentences
speechesL= sapply(combine, function(x) strsplit(x, "[\\?|\\.|\\!]"))

#### Person B: Preparation
### You will work with the list called speechesL that person A created.
### If needed, you can begin your work with the speechesL list
### provided on bspace.

### For each speech, your will create a word vector that 
### counts the number of occurrences of each word used in the \
### speech.  


### The following steps will help you create a word vector for 
### each speech:

###  Complete the function speechToWords() shown below
library(Rstem)

speechToWords = function(sentences){
  # sentences is character vector of sentences for each speech
  s1=gsub("'","",sentences)
  s2=gsub("[[:digit:]]","",s1)
  s3=gsub("(Applause.)","",s2)
  # Use gsub to eliminate apostrophes and numbers 
  # Drop the phrase (Applause.)
  # Turn characters to lower case.
  s4=tolower(s3)
  
  # Use strsplit to split the text up by blanks and punctuation
  s5=strsplit(s4,"([[:blank:]]|[[:punct:]])")
  # Unlist the return value 
  s6=unlist(s5)
  
  # Drop any empty words 
  s7=s6[s6!=""]
  
  # Use wordStem() to stem the words
  s8=wordStem(s7)  
  # return a character vector of all words in the speech
  return(s8)
}

# Apply the speechToWords() to each speech in speechesL
# Assign it to the list speechWords

speechWords = lapply(speechesL, speechToWords)

### Unlist the return value and use unique() 
### to get the bag of words.  
### Alphabetize the bag of words, and call it uniqueWords

uniqueWords= sort(unique(unlist(speechWords)))

### For each speech create a word vector 
### Think about vector operations to do this. 
### Consider the table function, and 
### assignment/indexing vectors using names.  
### Ultimately, create a matrix with columns corresponding to
### speeches and rows to words 

### You might find the following code helpful

emptyVec = rep(0, length(uniqueWords))
names(emptyVec) = uniqueWords

wordVecs = lapply(speechWords, function(x){
  emptyVec[names(table(x))]=table(x)
  return(emptyVec)
})

### Convert the list from the lapply into a matrix
wordMat = matrix(unlist(wordVecs), 
                 ncol  = length(wordVecs), 
                 byrow = FALSE)

####################
### Person A: Explore

### Now create the final data structure that you will use 
### to analyze the data.
### I have provided you with a starting  data frame, 
### called speechesDF. It has two variables Pres, which 
### holds the president's name, and party,
### which provides the political party of the president.
### It is available in the rda file on bspace

###	To this data frame add the following variables:

# yr - year of the speech (numeric)
# month - month of the speech (numeric)
# words - number of words in the speech (you will use person B's work)
# chars - number of letters in the speech
# sent - number of sentences in the speech
# short - proportion of words that are four or fewer letters

load("IntermediateResults.Rda")

speechesDF$yr = speechYr

speechesDF$month = speechMo

speechesDF$words = sapply(speechWords, length)

speechesDF$chars = sapply(speechWords, function(x) sum(nchar(x)))

speechesDF$sent = sapply(speechesL, length)

speechesDF$short = sapply(speechWords, function(x) sum(nchar(x) <= 4)/length(x))

plot(speechesDF$yr, speechesDF$words, type = "l", main = "Number of Words in Presidential Speeches", xlab = "Year", ylab = "Number of Words")

### In this plot, a graph of the number of words in each presidential speech given from 1790 to 2012 is shown. One can see that from 1790
### to the 1900s, the number of words steadily increased. However, around 1930, the number of words in the speeches dereased. And recently,
### those numbers have remained pretty steady around 6000 words. 

###	Explore the speeches using this data frame.  
### Make one plot that provides an interesting comparison 
### of the speeches/presidents.  

##############
### Person B: Distances between speeches
### In this final stage of analysis, compute the 
### distance between presidents using the Shannon-Jensen (SJ) metric 
### Use computeSJDistance() to compute the distance between 
### presidents. 

### To do this, first add together the word vectors for 
### the speeches made by the same president.
### The resulting matrix should be called presMat

source("computeSJDistance.R")

pres=unique(speechesDF$Pres)

presMat=matrix(0, nrow=length(uniqueWords),ncol=length(pres))
for(i in 1:length(pres)){
  if(sum(speechesDF$Pres==pres[i])!=1){
    presMat[,i]=rowSums(wordMat[,speechesDF$Pres==pres[i]])
  }else{
    presMat[,i]=wordMat[,speechesDF$Pres==pres[i]]
  }
}

### Next compute the number of presidents that used each word
### This is our "document frequency". Call it df
df=list()
for(i in 1:length(uniqueWords)){
  df[i]=sum(presMat[i,]!=0)
}
df=unlist(df)

### Call computeSJDistance() , passing it the presMat, df,
### and the unique words
### The return value is a matrix, call it presDist

presDist = computeSJDistance(tf = presMat, df=df, terms=uniqueWords)
rownames(presDist) = unique(pres)
colnames(presDist) = unique(pres)
### Use multidimensional scaling to produce a visualization 
### of  the results.
### Use labels, lines, color, etc. to make the 
### plot as informative as possible. 
coordinates=cmdscale(presDist)
preWW2=coordinates[1:28,]
postWW2=coordinates[29:41,]
plot(preWW2,xlab="Dimension 1",ylab="Dimension 2",
     main="Multidimensional Scaling of Presidents' Speeches",
     pch=23,col="Blue",xlim=c(-0.04,0.07),ylim=c(-0.04,0.03))
points(postWW2, pch=23, col="Red")
for(i in 1:41){
  text(coordinates[i,1], coordinates[i,2]-0.002, pres[i], cex=0.5)
}
points(-0.035,-0.03,pch=23,col="Blue")
text(-0.035,-0.03-0.002,"Pre World War II",cex=0.5)
points(-0.035,-0.035,pch=23,col="Red")
text(-0.035,-0.035-0.002,"Post World War II",cex=0.5)
### From this graph, we can see that the presidents' speeches can be grouped by era. The pre World War II
### preidents' speeches form a specific cluster and are very close to one another.

### Use hierarchical clustering to produce a visualization 
### of  the results.
plot(hclust(as.dist(presDist)),sub="",xlab="",ylab="", main="State of the Union Cluster Dendrogram")
### From the Cluster Dendrogram, we can see that the presidents' speeches cluster by era as well.