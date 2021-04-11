#Here we test for correlation > 0, carry out the permutation test and draw conclusions



library(MASS)
library(gginference)
library(tidyverse)
library(qdapDictionaries)library(ggraph)
library(igraph)
library(RColorBrewer)
library('circlize')
library(ggridges)
#devtools::install_github("nanxstats/ggsci")
library("ggsci")
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag) 
library(data.table)


#We load R analysis to get also the objects computed in the previous R scripts.
load("R_analysis.RData")

#pytr <- read.csv(".\\ubscrape-master\\WordsToPytrend.csv")


#pars = read.csv(".\\ubscrape-master\\Pytrends_final.csv")


#WordsToPytrend are the words after being processed and ready to be feed in Pytrend. They also contain definitions. Hence we
#join with Pytreds_final, that is the words with google trends scores, so that for each word we still have a definition.


pars <- pars[,!(colnames(pars) %in% c("X"))]

pars1 <- merge(pytr[c('word','word_id_start')], pars, by.x ='word')


#d7 needs to be computed in the 'Urban dict processing 1.R'
pars2 <- merge(d7[c('word_id','definition')], pars1, by.x ='word_id', by.y = "word_id_start")

pars2 <- (unique(pars2))

#We filter out common words in English. This is a bit controversial because common words may still be used as slangs in particular
#contexts. But,
#especially in the english language as it's spoken world wide, they may be searched for other reasons that their slang variation.
is.word  <- function(x) x %in% GradyAugmented # 

pars4 <- pars2[which(!is.word(pars2$word_id)),]

#We decide to consider only the words whose country of origin was the one with the maximum google trend value. We do that because:
#1) For interpretation purposes, every country's value is relative to the maximum country value. SO if the maximum country value
#IS the origin country, we can think as countries scores as the proportion of search in relation to the origin country. Hence
#the if the total score is 1200 and the origin country of the word is US, with value 100, than the word was spread for a volume of 
#12 times the origin country. We couldn't do that if the maximum country did not coincide with the origin country. We could still
#apply a further normalization to normalize the origin country to 100, but we decided instead to just drop words whose maximum country was
#not the origin country. That's also because
#2) It appeared logical to us that the country where the word originated would also be the one where the word thrived more. 
#Since slang words can have many meanings, and can be used in differetn contexts, words that appear to be more famous in other countries
#rather than their own, may as well be that they are just used according to a different meaning. 

pars2 %>% filter(value_country == 100) -> pars3

pars3 %>% group_by(country) %>% 
  mutate(correlation = cor(value_influence,as.numeric(as.Date(date))), 
         count =n()) %>%
  dplyr::select(country,correlation, count)  %>% distinct()-> parsi


#In order to apply a permutation test at alpha level 0.05, we need to have at least 20 relabellings. Hence n > 3.

Country_perm <- parsi$country[parsi$count > 3]

pars3$date <- as.Date(pars3$date)

Country_perm <- Country_perm[!(Country_perm %in% ("United States"))]


#h8 <- merge(h7,WordsN[c("word","country")],by ="word")

#h8 %>% group_by(country) %>% summarise(correlation = cor(as.numeric(as.Date(date)),value_influence)) ->corr1


#Country_perm <-unique( h8$country)

#We are ready to apply permutation tests to each of our countries!
correlaz <- matrix(NA,length(Country_perm),10000)
k = 1

for(j in Country_perm){
  
pars3 %>% filter(value_influence > 0) %>% filter(country == j) -> temp

n <- dim(temp)[1]

print(j)
y <- as.numeric(as.Date(temp$date))

for(i in 1:10000){
  smp <- sample(1:n)
  
  x <- temp$value_influence[smp]
  
  correlaz[k,i] <- cor(x,y)
  
  if(i %% 1000 == 0){
    
    print(i)
  }
  
  
}
k = k +1

}


plot_data <- as.vector(as.matrix(as.data.frame(t(correlaz))))
countrycol <- integer(0)
for(i in 1:length(Country_perm)){countrycol <- c(countrycol , rep(Country_perm[i], 10000))}

histograms <- data.frame(Country =countrycol, corr = plot_data )

#The data from the US still miss the entries coming from the all the data that implicetely referred to the US but that didn't have
#an American label. We read the non localized data and we deem the expressions as American if the strongest country on Google Trend was the US.
nongeo <- read.csv("trends_set_nongeo.csv")

nongeo %>% filter(country == 'United States') -> USwords

pars3 %>% filter(country == "United States") -> US

USwords <- rbind(US[3:7],USwords[-1])

USwords <- unique(USwords)

is.word  <- function(x) x %in% GradyAugmented # or use any dataset from package

USwords1 <- USwords[which(!is.word(USwords$word)),]



USwords1 %>% filter(value_country == 100) -> USwords2


temp <- USwords2

n <- dim(temp)[1]



correlaz <- rep(NA,10000)
for(i in 1:10000){
  smp <- sample(1:n)
  
  y <- as.numeric(as.Date(temp$date))
  x <- temp$value_influence[smp]
  
  correlaz[i] <- cor(x,y)
  
  
}

#We prepare to plot the data

parseView <- pars3[c(3,4,5,6,7)]

names(parseView) <- c("Word","Date","Country","GT Country Value","GT influence Value")

parseView %>% filter(`GT influence Value` >= 0) ->parseView


US_1 <- data.frame( Country=rep("United States",10000), corr = correlaz)


parsi1 <- parsi %>% filter(count > 3)

histograms <- rbind(histograms, US_1)

parsi1[1,] <- list("United States",cor(USwords2$value_influence, as.double(as.Date(USwords2$date))), dim(USwords2)[1] )


countries <- parsi1$country[order(parsi1$count, decreasing  =T)] 


histograms$Country <- factor(histograms$Country, levels = countries)

corr_toplot = parsi1$correlation[order(parsi1$count, decreasing = T)]

corr_toplot <- data.frame(Country = countries, observed = corr_toplot)

corr_toplot$Country = factor(corr_toplot$Country, levels = countries)


histograms <- merge(histograms, corr_toplot, by = "Country")

# Use smoke as the faceting variable
ggplot(histograms, aes(x = corr)) +
  geom_histogram(  colour = "black",bins= 80, fill = "lightblue") +
  facet_wrap(Country ~ .) + xlab("Correlation") +
  geom_vline(aes(xintercept = observed), colour = "red",  corr_toplot) +
  labs(x="Correlation",
       y = "Frequence",
       title="Permutation test on correlation between Date and GT Infuence Value",
       subtitle="A grid where each plot displays the distribution according to the hyphotesis where correlation is 0\n and the red lines are the  observed correlations.")


pValues = data.frame(Country = corr_toplot$Country, 
                     greater = rep(NA,length(corr_toplot$Country)),
                     bilateral = rep(NA,length(corr_toplot$Country)),
                     lower = rep(NA,length(corr_toplot$Country)),
                     observedValues = corr_toplot$observed)

for(i in 1:length(pValues[,1])){
  
  pValues$greater[i] <- (sum((histograms[histograms$Country == as.character(pValues$Country[i]),2] ) > (corr_toplot$observed[i]) )+1) / 10000
  
  pValues$bilateral[i] <- (sum(abs(histograms[histograms$Country == as.character(pValues$Country[i]),2] ) > abs(corr_toplot$observed[i]) )+1) / 10000
  pValues$lower[i] <- (sum(abs(histograms[histograms$Country == as.character(pValues$Country[i]),2] ) < abs(corr_toplot$observed[i]) )+1) / 10000
  
  
  
}

View(pValues)

#######################################



#Let's Build a network!

wordsMatrix <- readr::read_csv(".\\ubscrape-master\\words_country.csv")

wordsNonGeoMatrix <- readr::read_csv(".\\ubscrape-master\\words_nongeo.csv")


#We comment out the procudere for taking the 20 most famous words for each country.
#If those plots want to be showed, you can run the below commands
#WordsN <- unique(rbind(pars3[c(3,4,5,6,7)],USwords1))

#vi <- WordsN[order(WordsN$value_influence), ]


#df1 <- Reduce(rbind,by(vi, vi["country"], tail, n=20))


wordsMatrix1 <- merge(wordsMatrix, pars3[c('word','country')], by.x = c('index'), by.y = c("word"))


merge(wordsNonGeoMatrix, USwords1[c('word','country')], by.x = c('index'), by.y = c("word")) -> wordsNonGeoMatrix1


Matrix <- rbind(wordsMatrix1[-2], wordsNonGeoMatrix1[-2])

#Matrix %>% filter(index %in% df1$word) -> Matrix


Matrix[-1] %>% group_by(country) %>% summarize_all(sum) -> Matrix1

countries<- names(Matrix1)[-1][names(Matrix1)[-1] %in% as.matrix(Matrix1[,1])]

#not more than 20
#countries <- countries[ !(countries %in% c("Japan","New Zealand","Jamaica","Norway","China","Russia"))] 

other_countries <-  names(Matrix1)[-1][!(names(Matrix1)[-1] %in% as.matrix(Matrix1[,1]))]


Matrix1 <- Matrix1 %>% filter(country %in% countries)

Matrix1 <- Matrix1[-1]

mat <- as.matrix(Matrix1[c(countries)])





#we remove the diagonal values
diag(mat) <- 0

#Matrix2 <- as_tibble(mat)

#Matrix2 <- cbind(Matrix2,Matrix1[c(other_countries)])

#Matrix2$overall_values <- apply(Matrix2,1,sum)

Matrix[-1] %>% group_by(country) %>% summarize_all(sum) -> Matrix0

#Matrix2$country <- Matrix0$country

#adjacency <-  as.matrix(Matrix2[c(countries)])


rownames(mat) <- colnames(mat)





adjacency <- mat


E(g)$weight

# Basic usual argument
#install.packages("chorddiag")






g <- igraph::graph_from_adjacency_matrix(adjacency, mode = "directed", weighted =  T)

df <- igraph::as_data_frame(g)



library(ggsci)

cols <- pal_jco(alpha = 0.6)(10)

cols1 <- pal_npg(alpha = 0.6)(10)
cols3 <- pal_locuszoom(alpha = 0.6)(7)

cols2 <- pal_simpsons(alpha = 0.6)(1)

colors <- c(cols,cols1,cols2,cols3)

circos.clear()

circos.par(start.degree = 160, 
           clock.wise = FALSE,
           gap.degree = 2, track.margin = c(-0.01, 0.01), points.overflow.warning = FALSE)

colss <- colors[sample(1:21, replace =F)]

par(mfrow = c(1,2))


chordDiagram(df, grid.col = colss[c(1,3:13,19,20,21)], annotationTrack = "grid", 
             link.arr.type = "big.arrow",
             directional = 1,
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04,
             link.sor= T,
             link.largest.ontop = FALSE,
             preAllocateTracks = list(track.height = 0.3))

title(main = "Chord Diagram of the influence of \n the most 20 famous words per country")

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(-0.1, 0.5))
}, bg.border = NA) 



chordDiagram(df, grid.col = colss[c(1,3:13,19,20,21)], annotationTrack = "grid", 
             link.arr.type = "big.arrow",
             directional = 1,
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04,
             link.sor= T,
             link.largest.ontop = FALSE,
             preAllocateTracks = list(track.height = 0.3), 
             scale= T)



circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(-0.1, 0.5))
}, bg.border = NA) 

title(main = "Normalized Chord Diagram of the influence of \n the most 20 famous words per country")




#write_csv(WordsN, "WordsN.csv")

#Eventually we carry out a quick t-test for the mean of the US words
#that occurred before 2011 and those that occured after. 

USwords2 %>% mutate(dateBin = ifelse(as.numeric(as.Date(date)) >= as.numeric(as.Date("2011-01-01")),1,0 )) -> US3


pop0 <- US3[US3$dateBin == 0,"value_influence"]
pop1 <- US3[US3$dateBin == 1,"value_influence"]

pop0d <- as.numeric(US3$date[US3$dateBin == 0])
pop1d <-as.numeric(US3$date[US3$dateBin == 1])


test <- t.test(x = US3[US3$dateBin == 0,"value_influence"], y = US3[US3$dateBin == 1,"value_influence"], "less")



t<- t.test(x = US3[US3$dateBin == 0,"value_influence"] ,y =US3[US3$dateBin == 1,"value_influence"])
t
ggttest(t)

length(pop0)

length(pop1)


mean(US3[US3$dateBin == 0,"value_influence"])

mean(US3[US3$dateBin == 1,"value_influence"])

View(WordsN)
dim(WordsN)

# 
# 
# hist(as.Date(WordsN$date))
# 
# WordsN$binary <- ifelse(WordsN$country == "United States",1,0)
# 
# null <- WordsN$value_influence[WordsN$binary == 0]
# hist1 <- hist(null, nclass = 30)
# 
# hyp <- WordsN$value_influence[WordsN$binary == 1]
# 
# hist2 <- hist(hyp, nclass = 30)
# 
# plot(hist1, col ="blue", freq  =F)
# plot(hist2, add = T, col =  mycol <- t_col("red", perc = 50, name = "lt.pink"), freq = F)
# 
# mean(null)
# mean(hyp)
# 
# mean()
# mean()
# 
# 
# # create different vectors for each group
# 
# (n1 <- length(hyp))
# (n <- length(null))
# 
# ######
# 
# # Let's do a Wilcoxon test:
# 
# wilcox.test(null, hyp, alternative="less")
# 
# help("wilcox.test")
# 
# 
# wilcox.test(null[order(null, decreasing =T)][1:100],hyp[order(hyp, decreasing = T)][1:100], alternative="less")
# 
# mean(null[order(null, decreasing =T)][1:100])
# mean(hyp[order(hyp, decreasing =T)][1:100])
# 
# # We do not reject H0 in favor of Ha at alpha=0.05
# # There is not enough evidence in the data to conclude that 
# #   that the compound increases asat levels. 
# 
# 
# #######################################################
# 
# # We now show that the Wilcoxon test can be 
# #   viewed as a permutation test:
# ranks <- rank(c(hyp,null))
# (observed.sum.ranks <- sum(ranks[1:n1]))
# 
# # nr of different group assignments:
# choose(n1,n)
# 
# # we will randomly sample nrep permutations:
# nrep <- 100000
# 
# wilcox.one.rep <- function(y, n1, n){
#   ynew <- sample(y, n, replace=F)
#   ranks.new <- rank(ynew)
#   return(sum(ranks.new[1:n1]))
# }
# 
# res.wilcox <- replicate(nrep, wilcox.one.rep(c(hyp,null), n1, n))
# 
# hist(res.wilcox)
# abline(v=observed.sum.ranks)
# (pval <- (sum(res.wilcox<=observed.sum.ranks)+1)/(nrep+1))
# 
# #########################################################
# library(scales)
# 
# 
# dates_p <- as.numeric((lubridate::year(as.Date(WordsN$date))))
# dates_p <- as.data.frame(dates_p)
# ggplot(dates_p, aes(x = dates_p)) + geom_histogram(bins = 18,color="black", fill="lightblue") + 
#     xlab("Years") + ggtitle("Words count distributed over years")
# 
# View(pars3)
# 
