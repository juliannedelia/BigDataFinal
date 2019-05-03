install.packages("syuzhet")
library(syuzhet)

setwd("/Users/juliannedelia/Desktop/BigData/final_project")
list.files()

# first I'm going to look at the dataset from one week before the season finale
one_week_before = read.csv("3.6.csv")
head(one_week_before)

one_week_desc = one_week_before$description
head(one_week_desc)

# data conversion and cleaning
one_week_desc = (iconv(one_week_desc, "latin1", "ASCII", sub=""))
one_week_desc = gsub("&amp", " ", one_week_desc)
one_week_desc = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", one_week_desc)
one_week_desc = gsub("@\\w+", " ", one_week_desc)
one_week_desc = gsub("t.co", " ", one_week_desc)
one_week_desc = gsub("[[:punct:]]", " ", one_week_desc)
one_week_desc = gsub("[[:digit:]]", " ", one_week_desc)
one_week_desc = gsub("http\\w+", " ", one_week_desc)
one_week_desc = gsub("[ \t]{2,}", " ", one_week_desc)
one_week_desc = gsub("^\\s+|\\s+$", " ", one_week_desc)

head(one_week_desc)

one_week_sentiment = get_nrc_sentiment(one_week_desc)
one_week_sentiment
write.csv(one_week_sentiment, "one_week_sentiment.csv", fileEncoding = "UTF-8")

summary(one_week_sentiment)
colSums(one_week_sentiment)
barplot(colSums(one_week_sentiment))


# now I'm going to look at the first night of the 2-night season finale
# this dataset has 42,000 tweets

night_one = read.csv("3.11.csv")
head(night_one)

night_one_desc = night_one$description
head(night_one_desc)

# data conversion and cleaning
night_one_desc = (iconv(night_one_desc, "latin1", "ASCII", sub=""))
night_one_desc = gsub("&amp", " ", night_one_desc)
night_one_desc = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", night_one_desc)
night_one_desc = gsub("@\\w+", " ", night_one_desc)
night_one_desc = gsub("t.co", " ", night_one_desc)
night_one_desc = gsub("[[:punct:]]", " ", night_one_desc)
night_one_desc = gsub("[[:digit:]]", " ", night_one_desc)
night_one_desc = gsub("http\\w+", " ", night_one_desc)
night_one_desc = gsub("[ \t]{2,}", " ", night_one_desc)
night_one_desc = gsub("^\\s+|\\s+$", " ", night_one_desc)

head(night_one_desc)

night_one_sentiment = get_nrc_sentiment(night_one_desc)
night_one_sentiment
write.csv(night_one_sentiment, "night_one_sentiment.csv", fileEncoding = "UTF-8")

summary(night_one_sentiment)
colSums(night_one_sentiment)
barplot(colSums(night_one_sentiment))


# now I'm going to look at the second night of the 2-night season finale
# this dataset has 45,000 tweets

night_two = read.csv("3.12.csv")
head(night_two)

night_two_desc = night_two$description
head(night_two_desc)

# data conversion and cleaning
night_two_desc = (iconv(night_two_desc, "latin1", "ASCII", sub=""))
night_two_desc = gsub("&amp", " ", night_two_desc)
night_two_desc = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", night_two_desc)
night_two_desc = gsub("@\\w+", " ", night_two_desc)
night_two_desc = gsub("t.co", " ", night_two_desc)
night_two_desc = gsub("[[:punct:]]", " ", night_two_desc)
night_two_desc = gsub("[[:digit:]]", " ", night_two_desc)
night_two_desc = gsub("http\\w+", " ", night_two_desc)
night_two_desc = gsub("[ \t]{2,}", " ", night_two_desc)
night_two_desc = gsub("^\\s+|\\s+$", " ", night_two_desc)

head(night_two_desc)

night_two_sentiment = get_nrc_sentiment(night_two_desc)
night_two_sentiment
write.csv(night_two_sentiment, "night_two_sentiment.csv", fileEncoding = "UTF-8")

summary(night_two_sentiment)
colSums(night_two_sentiment)
barplot(colSums(night_two_sentiment))


# now I'm going to look at data from one week after the finale
after = read.csv("3.20.csv")
head(after)

after_desc = after$description
head(after_desc)

# data conversion and cleaning
after_desc = (iconv(after_desc, "latin1", "ASCII", sub=""))
after_desc = gsub("&amp", " ", after_desc)
after_desc = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", after_desc)
after_desc = gsub("@\\w+", " ", after_desc)
after_desc = gsub("t.co", " ", after_desc)
after_desc = gsub("[[:punct:]]", " ", after_desc)
after_desc = gsub("[[:digit:]]", " ", after_desc)
after_desc = gsub("http\\w+", " ", after_desc)
after_desc = gsub("[ \t]{2,}", " ", after_desc)
after_desc = gsub("^\\s+|\\s+$", " ", after_desc)

head(after_desc)

after_sentiment = get_nrc_sentiment(after_desc)
after_sentiment
write.csv(after_sentiment, "after_sentiment.csv", fileEncoding = "UTF-8")

summary(after_sentiment)
colSums(after_sentiment)
barplot(colSums(after_sentiment))

# comparing night one with night two

sum(c(colSums(night_one_sentiment)))
n1 = (colSums(night_one_sentiment/104673)*100)
n1

sum(c(colSums(night_two_sentiment)))
n2 = (colSums(night_two_sentiment/119066)*100)
n2

# bind percentages
n1n2 = rbind(Night_1= n1, Night_2 = n2)
n1n2

# plot percentages to compare side by side

barplot(n1n2, 
        beside=T, 
        las=1, 
        col=c("pink","purple"), 
        border=NA, 
        xlab = "Sentiments", 
        ylab = "Sentiment Scores (%)", 
        main = "Finale Night One v. Night Two")
legend("topleft",  
       legend = c("Night One", "Night Two"),
       fill = c("pink", "purple"),
       border=NA,
       box.lty = 0,
       cex = 0.5)

# comparing one week before and one week after the finale

sum(c(colSums(one_week_sentiment)))
before = (colSums(one_week_sentiment/2743)*100)
before

sum(c(colSums(after_sentiment)))
after_ = (colSums(after_sentiment/2957)*100)
after_

# bind percentages
b_and_a = rbind(Before= before, After = after_)
b_and_a

# plot percentages to compare side by side

barplot(b_and_a, 
        beside=T, 
        las=1, 
        col=c("seagreen3","royalblue4"), 
        border=NA, 
        xlab = "Sentiments", 
        ylab = "Sentiment Scores (%)", 
        main = "One Week Before v. One Week After")
legend("topleft",  
       legend = c("Before", "After"),
       fill = c("seagreen3", "royalblue4"),
       border=NA,
       box.lty = 0,
       cex = 0.5)





