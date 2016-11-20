# HW 7 - Due Tuesday October 25, 2016 in moodle and hardcopy in class. 
# Upload R file to Moodle with name: HW7_490IDS_YourClassID.R
# Do not remove any of the comments. These are marked by #

# Please ensure that no identifying information (other than your class ID) 
# is on your paper copy, including your name

# Q. 1
# (a)
insert.reg = "[[:alnum:]]+[[:punct:]][[:alnum:]]+"
word = c("h@te", "v|c0din")
gregexpr(insert.reg, word)

# (b)
ip.address = "[[:digit:]]{1,3}\\.[[:digit:]]{1,3}\\.[[:digit:]]{1,3}\\.[[:digit:]]{1,3}$"
address = c("100.12.162.0", "192.18.345.88")
gregexpr(ip.address,address)

# (c)
email.reg = "[[:alnum:]]+@[[:alnum:]]+\\.(com|edu|net|org|gov)$"
email = c("huilyu2@illinois.edu", "lvhui0122@126.com")
gregexpr(email.reg,email)

# Q. 2
# (a)
con = file("C:/Documents/GSLIS/490 Introduction to Data Science/Data and Code/stateoftheunion1790-2012.txt", open = "r")
allText = readLines(con = con)
class(allText)
length(allText)

# (b)
# Return the indices of lines.
asterisk.index = grep("^[*]{3}$",allText)

# (c)
# Each date appears four lines after the "***".
date.index = asterisk.index+4
date = allText[date.index]
head(date)

# (d)
year.index = regexpr("[[:digit:]]{4}", date)
year = substring(date, year.index, nchar(date))
head(year)

# (e)
month.index = regexpr(" [[:digit:]]{1,2}, ", date)
month = substring(date, 1, month.index-1)
head(month)

# (f)
# Each name of president appears 3 lines after the "***".
president.index = asterisk.index+3
president = allText[president.index]
head(president)

# (g)
numberOfSpeeches = length(grep("^[*]{3}$",allText))
unique(president)
numberOfPresidents = length(unique(president))

# (h)
listOfSpeeches = list()
for (i in 1:222) {
  listOfSpeeches[[i]] = character()
}
asterisk.index[223] = 169641
for (j in 1:222) {
  for (i in (asterisk.index[j]+2):(asterisk.index[j+1]-2)) {
    listOfSpeeches[[j]] = append(listOfSpeeches[[j]], allText[i])
  }
}

length(listOfSpeeches)

# (i) to (m)

install.packages("Rstem")
library("Rstem", lib.loc="C:/Program Files for operation/R-3.3.1/library")

speechToWords = function(lines){
  # lines is a character vector of lines for each speech
  # Use gsub to eliminate apostrophes and numbers 
  s1=gsub("'","",lines)
  s2=gsub("[[:digit:]]+","",s1)
  
  # Drop the phrase (Applause.)
  # I think the exact phrase with parentheses should be eliminated. But the word applause inside
  # the sentences should not be removed.
  s3=gsub("\\(Applause\\.\\)","",s2)

  # Turn characters to lower case.
  s4=tolower(s3)
  
  # Use strsplit to split the text up by blanks and punctuation
  s5=strsplit(s4,"[[:blank:]]|[[:punct:]]")
  # Unlist the return value 
  s6=unlist(s5)
  
  # Drop any empty words 
  s7=s6[s6!=""]
 
  # Use wordStem() to stem the words
  # s8=wordStem(s7)  
  # return a character vector of all words in the speech
  return(s7)
}

# Create a list of words. Each character vector of each speech consists of only one element.
# The element is a string of all the text of that speech.
listOfWords = list()
for (i in 1:222) {
  listOfWords[[i]] = character()
}
for (i in 1:222) {
  listOfWords[[i]] = paste(listOfSpeeches[[i]], sep=" ", collapse = " ")
}

# Create a word vector for each speech.
speechWords = lapply(listOfWords, speechToWords)
length(speechWords)

# (n)

# Unlist the return value and use unique() to get the bag of words.  
# Alphabetize the bag of words, and name it uniqueWords.

uniqueWords= sort(unique(unlist(speechWords)))
length(uniqueWords)

tempVector = rep(0, length(uniqueWords))
names(tempVector) = uniqueWords

wordVectors = lapply(speechWords, function(x){
  tempVector[names(table(x))]=table(x)
  return(tempVector)
})

# Convert the list from the lapply into a matrix
wordMatrix = matrix(unlist(wordVectors), ncol = length(wordVectors))
dim(wordMatrix)
rownames(wordMatrix) = uniqueWords

# So each column represents each speech. There are 23102 unique words in total.
# The value of each cell is the term frequency for that word in that speech.

# (o)

# I would like to pick Barack Obama and George Washington's speeches and compare 
# how their frequent terms differ.

which(president=="George Washington")
# [1] 1 2 3 4 5 6 7 8

which(president=="Barack Obama")
# [1] 219 220 221 222

washingtonWords = wordMatrix[,1:8]
washingtonWords = apply(washingtonWords, 1, sum)
washingtonTopwords = matrix(nrow = 50, ncol = 2)
washingtonTopwords[,2] = head(sort(washingtonWords, decreasing = TRUE), 50)
washingtonTopwords[,1] = names(head(sort(washingtonWords, decreasing = TRUE), 50))
washingtonTopwords

obamaWords = wordMatrix[,219:222]
obamaWords = apply(obamaWords, 1, sum)
obamaTopwords = matrix(nrow = 50, ncol = 2)
obamaTopwords[,2] = head(sort(obamaWords, decreasing = TRUE), 50)
obamaTopwords[,1] = names(head(sort(obamaWords, decreasing = TRUE), 50))
obamaTopwords

# From the two matrices of Washington and Obama top words, we can see that
# apart from the stop words there are some differences between them.
# Washington spoke more words like "united", "sates", "public", "government" and "citizens".
# But Obama spoke more words like "american", "america", "americans", "people", "jobs" and "years".
# We can find that the statement for the country related words has changed from "United States"
# to "American". Maybe "U.S." could not be reflected for this term frequency comparison.
# Also we can find during George Washington period, the public, government and citizens are
# usually been mentioned. But during Barack Obama period, things about people and jobs 
# have more concerns.

# I am interested in the term frequency of "jobs" and "job" over the time.
which(rownames(wordMatrix)=="job")
# [1] 11416
which(rownames(wordMatrix)=="jobs")
# [1] 11420
wordMatrix[11416,]
wordMatrix[11420,]
frequency.job = wordMatrix[11416,] + wordMatrix[11420,]
plot(year, frequency.job, type = "l", main = "Frequency of job & jobs", xlab = "year", ylab = "frequency (times)")

# From the graph we can see that the word "job" has never been mentioned in speeches before 1900.
# Right after 1900, "job" appeared in speeches of presidents. Then there was a peak around 1948.
# After that, there were fluctuations but generally the frequency was rising. Most recently,
# the word "job" was quite frequently used in the speech. The graph of the frequency generally
# reflects that job demands are increasing and the employment is getting more concerns.
