library(stringr)
tweets <- read.csv("data/text-emotion.csv")
content <- as.character(tweets$content)

counts <- sapply(content, nchar)
summary(counts)
hist(counts, lwd = 5)


mentions <- str_extract_all(content, "\\@[[:alnum:]]*", simplify = TRUE)
countmentions <- apply(mentions, 1, function(x){sum(x!="")})
hist(countmentions)

zero <- sum(countmentions == 0)
one <- sum(countmentions == 1)
two <- sum(countmentions == 2)
three <- sum(countmentions == 3)
four <- sum(countmentions == 4)
five <- sum(countmentions == 5)
six <- sum(countmentions == 6)
seven <- sum(countmentions == 7)
eight <- sum(countmentions == 8)
nine <- sum(countmentions == 9)
ten <- sum(countmentions == 10)
mentioncounts <- data.frame(number = c(seq(1,10,1)),counts = c(one,two,three,four,five,six,seven,eight,nine,ten))

barplot(mentioncounts$counts, )

content[countmentions==10]

mentions <- str_extract_all(content, "\\#[[:alnum:] ]*", simplify = TRUE)
countmentions <- apply(mentions, 1, function(x){sum(x!="")})