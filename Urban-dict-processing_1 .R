

library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)
library(RSQLite)

# We first read the 'definition' table
#with all the words and definitions present 
#on urban dictionary up to 15 March 2021
filename <- "urban-dict.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
dbListTables(db)
mytable <- dbReadTable(db,"definition")

#total number of definitions, considering many
#words were defined multiple times by different users.
dim(mytable)[1]
names(mytable)

#We set each definition string to lower cases.
df_set1  <- mytable %>% mutate(definition  = tolower(definition))
#The words in the dictionary range from celebrities, to common words
#being defined sarcastically, to numbers and symbols. After all, the entire
#english vocabulary together, considering different countries variants,
#don't seem to exceed 1m words. Hence, we decide to filter words that contain in their
#definition the word 'slang'.
df_set1 <- dplyr::filter(df_set, grepl("slang",definition))

names(df_set1)

names(def2)


american <- anti_join(x = df_set1,y = def2, by = c("word_id"))
dim(american)

dim(def2)

View(american)

write_csv(american, "american.csv")

help(anti_join)

View(df_set1)
#Urban dictionary was born in America. Users can sign up from all over the world
#and can insert new words belonging to any language.
#Hence we decide also to filter words containing any geographic adjective that could
#link them to a specific location.
#To do that, we downloaded a list of all possible 'demonyms' in english.
#A demonym is a word used for the inhabitant of a place.
#American is a demonym, but also Bostonian is a demonym, as much 
#as Acheian (ancient word population).
#Our list is comprised of 2144 demonyms, where every demonym
#has the corresponding noun linked to it.

countries <- read.csv("demonyms.csv", header = F)

names(countries)
#We further filter by the presence of these adjectives.
to_filter <- paste(as.character(countries$V1),collapse = "|")
def2 <- filter(df_set1,grepl(paste(as.character(countries$V1),
                                   collapse = "|"), df_set1$definition))


Denomym <- str_extract(def2$definition, (to_filter))
def2 <- cbind(def2,Denomym)


View(def2 %>% filter(word_id == "Goen"))

d3 %>% filter(word_id == "Goen")


View(def2 %>% group_by(Denomym) %>% summarise(count =n()))
#Now that we have for each definition, the denomym it comes from, we want to group all 
#the denomym by country: that is American -> American -> U.S., Bostonian -> American -> U.S.

#We can do that having the nouns corresponding to the denomyms, and using a dataset
#of cities, regions and states of the world.
names(countries) <- c("adj", "name")

d3 <- merge(x = def2,y = countries,by.x = c("Denomym"),by.y =c("adj"),all.x =T)
cities <- read.csv("city.csv")

country_1 <- data.frame( name = cities$country, COUNTRY = cities$country)
subcountry_1 <- data.frame( name = cities$subcountry, COUNTRY = cities$country)
city_1 <- data.frame( name = cities$name, COUNTRY = cities$country)

geoloc <- rbind(country_1, subcountry_1, city_1)

geoloc <- unique(geoloc)


d4 <- merge(x = d3, y = geoloc,  by.x = c("name"), by.y = c("name"), all.x =T)

View(d4)

d5 <- merge(x = d3, y = geoloc,  by.x = c("name"), by.y = c("name"), all.x =T)
#general data cleaning
d5 <- d5[d5$Denomym != "Tab",]

check<-d5 %>% select(name,id) %>% group_by(name,id) %>% summarise(count = n()) %>% filter(count > 1)

d6_toadd <- anti_join(d5,check, by = "id")

list1 <- d5 %>% filter(id %in% check$id)

#There are ambiguities that need to be manually solve. EX: boston existing both in the UK and US.
#We pick the one that seems more plausible to us.
name = c("Aberdeen", 
         "Armenia", 
         "Birmingham",
         "Boston",
         "Bristol",
         "Cambridge"
         ,"Colombia"
         ,"Derry",
         "Dublin",
         "Dunedin",
         "Florida",
         "Georgia",
         "Goa",
         "Hyderabad",
         "Jamaica",
         "Lancaster",
         "Lebanon",
         "Liberia",
         "Liverpool",
         "London",
         "Manchester",
         "Maryland",
         "Mexico",
         "New Brunswick",
         "Newcastle",
         "Ontario",
         "Perth",
         "Plymouth",
         "Puerto Rico",
         "Punjab",
         "Rome",
         "Sidney",
         "Tonga",
         "Venezuela",
         "Venice",
         "Victoria",
         "Virginia",
         "Wellington"
)

length(name)

COUNTRY = c("United Kingdom", 
            "Armenia",
            "United Kingdom",
            "United States",
            "United Kingdom",
            "United Kingdowm",
            "Colombia","Ireland",
            "Ireland","United States",
            "United States",
            "United States",
            "India",
            "India",
            "Jamaica",
            "United Kingdom",
            "Lebanon",
            "Liberia",
            "United Kingdom",
            "United Kingdom",
            "United Kingdom",
            "United States",
            "Mexico",
            "Canada",
            "Australia",
            "Canada",
            "United Kingdom",
            "United Kingdom",
            "Puerto Rico",
            "India",
            "Italy",
            "Australia",
            "Tonga",
            "Venezuela",
            "Italy",
            "Canada",
            "United States",
            "New Zeland"
)



d_rem <- data.frame(name =name, COUNTRY =  COUNTRY)
names(list1)
m1 <- merge(list1, d_rem, by = c("name", "COUNTRY"))
d6 <- rbind(d6_toadd, m1)
d6 <- d6 %>% distinct()
check2<-d6 %>% select(id) %>% group_by(id) %>% summarise(count = n()) %>% filter(count > 1)
list2 <- d6 %>% filter(id %in% check2$id)

d7 <- d6 %>% mutate(final_countries = ifelse(is.na(COUNTRY),name, COUNTRY))

#Further refinements
d7$final_countries[d7$final_countries == "Philadelphia"] <- "United States"
d7$final_countries[d7$final_countries == "Galway"] <- "Ireland"
d7$final_countries[d7$final_countries == "Yorkshire"] <- "United Kingdom"
d7$final_countries[d7$final_countries == "Vatican City State"] <- "Vatican City"
d7$final_countries[d7$final_countries == "Hispania"] <- "Spain"
d7$final_countries[d7$final_countries == "Glasgow"] <- "United Kingdom"

#We get the countries with more than 100 words
d7 %>% group_by(final_countries) %>% summarise(n()) -> counts


names(d7)


countries <- counts[counts$`n()` > 100,1]

d7 <- d7[d7$final_countries != 'Africa',] #Africa is not a country

d7 %>% select(word_id,final_countries) %>% filter(final_countries %in% as.matrix(countries) ) -> search1

length(search1[,1])
View(search1)

View(d7)




write_csv(search1,"WordsToProbe.csv")
#We probe all the other fields of the word through the urban dictionary API in python