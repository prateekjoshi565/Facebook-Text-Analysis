# Download the data 'details' and put it in your working directory...
# ...if you wish to run this code in your system.

install.packages("Rfacebook")
install.packages("RCurl")
library(Rfacebook)
library(RCurl)

# visit http://www.listendata.com/2017/03/facebook-data-mining-using-r.html to understand authentication process
#fb_oauth = fbOAuth(app_id="2*****************1", app_secret="5*******************************b", extended_permissions = TRUE)
#details = getPost(post = "10155510177141103", token=fb_oauth, comments = T, reactions = T, n.likes = 5000)

load("details.RDa")

table(details$reactions$from_type)

details$post$likes_count
details$post$comments_count
details$post$shares_count


library(tm)
library(stringr)
library(wordcloud)
library(ggplot2)
library(SentimentAnalysis)

# Load the comments in a corpus
comments = Corpus(VectorSource(details$comments$message))


######################### Text Transformation #########################

# replace symbols with space
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# used str_replace_all() from 'stringr' package
toSpace2 = content_transformer(function (x, pattern) str_replace_all(x, pattern, " "))

# remove trailing spaces
cut_trail = content_transformer(function(x, pattern) gsub(pattern, "", x))

# remove leading spaces
cut_leading = content_transformer(function(x, pattern) gsub(pattern, "", x))

comments = tm_map(comments, toSpace, "/")
comments = tm_map(comments, toSpace, "\n")
comments = tm_map(comments, toSpace, "@")
comments = tm_map(comments, toSpace, "\\|")
comments = tm_map(comments, toSpace2, "[[:punct:]]") # removes punctuation
comments = tm_map(comments, cut_trail, "[[:space:]]*$")  # removes trailing spaces
comments = tm_map(comments, cut_leading, "^[[:space:]]")  # removes leading spaces

# Convert all text to lower case
comments = tm_map(comments, content_transformer(tolower))

# Remove numbers
comments = tm_map(comments, removeNumbers)

# Remove english common stopwords
comments = tm_map(comments, removeWords, stopwords("english"))

# Document matrix is a table containing the frequency of the words
dtm = TermDocumentMatrix(comments)

m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v), count = v)

# Generate a word cloud
wordcloud(words = d$word, freq = d$count, min.freq = 30,
          max.words=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# plotting 20 most frequent terms
ggplot(d[1:20,], aes(reorder(word, -count), count, fill = factor(word))) + 
  geom_bar(stat = "identity") + xlab("word") +
  geom_text(aes(label = count), vjust = -0.3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.5, hjust = 1, size = 12),
        panel.background = element_rect(fill = "transparent"),
        legend.position = "none")

# Sentiment Analysis
comments_sentiment = analyzeSentiment(comments)

comments_sentiment = convertToDirection(comments_sentiment)
comments_sentiment$actual_comment = details$comments$message

View(comments_sentiment[,c(2,15)])