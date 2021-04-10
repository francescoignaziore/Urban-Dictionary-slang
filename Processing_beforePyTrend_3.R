#In this file we process the words after being enriched with new information fields 
#such as 'posting date' and 'likes'. Specifically, there are many words with the same word_id but with many entries,
#meaning that the same word may have been inserted by multiple users at different times. Hence we
#decide to refer take an average of the inserted dates for all terms that are equal, and take the value of maximum like.
#(We won't be using likes in the analysis as we noticed sometimes they're not related to how famous the word is, but rather
#to how funny the user description)



library(tidyverse)

#We do it first for the words with a geolocalization tag.


defs <- read.csv("WordsInfo.csv")


View(defs)

defs$written_on <- as.Date(defs$written_on)
#We drop duplicates keeping the average date, and if there were words
#inserted in the same month, we get the one with the highest likes.

defs %>% group_by( word_id_start) %>%
  summarise(date = mean(written_on),
            word_id_start = word_id_start) -> mean_dates

def2 <- merge(mean_dates, defs, by = c("word_id_start"))


def2 <- unique(def2)



def3 <- def2[c("word", "word_id_start", "date", "thumbs_up", "final_countries", "written_on")]
def3 %>% group_by( word_id_start) %>%
  summarise(thumbs_up = max(thumbs_up),
            word_id_start = word_id_start) -> max_likes
max_likes <- unique(max_likes)
def4 <- merge(max_likes, def3, by = c("word_id_start","thumbs_up"))
def4 <- unique(def4)
View(def4)

write.csv(def4,"WordsToPytrend")


#We then do it for the ones for which there is no geotag


#################American words
american_defs <- read.csv('American_ud.csv')


american_defs$written_on <- as.Date(american_defs$written_on)
american_defs %>% group_by( word_id) %>%
  summarise(date = mean(written_on),
            word_id = word_id) -> mean_dates


def2american <- merge(mean_dates, american_defs, by = c("word_id"))


def2american <- unique(def2american)



def3american <- def2american[c("word_id", "word", "date", "thumbs_up")]


def3american %>% group_by( word_id) %>%
  summarise(thumbs_up = max(thumbs_up),
            word_id= word_id) -> max_likes

max_likes <- unique(max_likes)


def4america <- merge(max_likes, def3american, by = c("word_id","thumbs_up"))


def4america <- unique(def4america)

american_definitions <- read.csv('american.csv')

american_definitions <- american_definitions[c(2,3)]

final_us_names <-merge(def4america, american_definitions, by = 'word_id')

write.csv(final_us_names,"WordsToPytrend_nongeo")



