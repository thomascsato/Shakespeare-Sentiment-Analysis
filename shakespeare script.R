library(tidyverse)
library(tidytext)
library(gutenbergr)

# Random data exploration.

shakespeare <- gutenberg_works(author == "Shakespeare, William")
shakespeare
IDs = shakespeare[shakespeare$title == "Measure for Measure" |
                  shakespeare$title == "The Taming of the Shrew",]$gutenberg_id

shakespeare %>%
  filter(gutenberg_id %in% IDs)

MeasureForMeasure <- gutenberg_download(1530)
Taming <- gutenberg_download(1508)

?get_sentiments
View(get_sentiments("nrc"))

afinn <- get_sentiments("afinn")
range(afinn$value)

nrc <- get_sentiments("nrc")
unique(nrc$sentiment)

unique(get_sentiments("loughran")$sentiment)

View(MeasureForMeasure)

# Be able to group by paragraph, in other words, by which character is talking.
index <- 0
for (i in 1:length(MeasureForMeasure$text)) {
  if (MeasureForMeasure$text[i] == "") {
    index <- index + 1
  }
  MeasureForMeasure[i, 3] <- index
}
names(MeasureForMeasure)[3] <- "paragraph"

MeasureForMeasuret <- t(pivot_wider(MeasureForMeasure[, -1], names_from = paragraph, values_from = text))
View(MeasureForMeasuret)
str_c(MeasureForMeasuret[10])
stri_join_list(MeasureForMeasuret[100], sep = " ")
MeasureForMeasure_text <- lapply(list(MeasureForMeasuret), stri_join_list, sep = " ")
View(data.frame(MeasureForMeasure_text))



c1 <- M4M_sentiment %>%
  group_by(word, character) %>%
  summarize(n = n()) %>%
  group_by(character) %>%
  summarize(n = n())
c2 <- count(M4M_tokens, character)
c3 <- inner_join(c1, c2, by = c("character" = "character"))

c3 <- mutate(c3, ratio = n.x / n.y)
View(c3 %>% arrange(desc(ratio)))
# Juliet has 80% rate of sentiment words to non-sentiment words

J2 <- filter(M4M_sentiment, character == "JULIET")

