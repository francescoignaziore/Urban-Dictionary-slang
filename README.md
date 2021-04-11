# Urban dictionary slang and its influence across the world


## Data Retrieval

#### 1) For the retrieval of Urban Dictionary words and definitions, we used the following python scripts: https://github.com/samuelstevens/ubscrape

#### 2) The Urban Dictionary Database updated to March 21, 2021, containing 5 714 237 entries, (word_id, definition),  can be downloaded here: https://www.dropbox.com/s/tvhy5otred13e71/urban-dict.db?dl=0

#### 3) The following .csv files, used in the intermediate steps of the analysis can be found here: https://www.dropbox.com/sh/z32es6xjqhz18ad/AADfp1vZ_XrX3ivcWdDGaEaHa?dl=0
##### Their description is:

#### i) From the database, we filter all the words through demonyms and we assign each word to a country.
city.csv = dataset of cities/regions/countries in the world. \
demonyms.csv = dataset with all English demonyms.

#### ii) We save all the word_id and definition of the words we are interested in in two files, according to whether the word has a geo-label or it doesn't. These words now have to be looked up on Urban Dictionary to retrieve the date when they were inserted.
WordsToProbe.csv = all words that contained the word slang in their definition and a denomyn. \
american.csv = all words that contained the word slang in their definition, but not a denomyn. With care, we'll argue that some of these words may be American.

#### iii) The enriched dataframes with new fields. They now have be re-processed. 
American_ud.csv = words that were non-localized and that are supposedly american, after being enriched with fields thumbs_up and written_on (date).\
WordsInfo.csv = words that were localize, after being enriched with fields thumbs_up and written_on (date). 

#### iv) We process the data and, given that many equal words were posted by many users,  we take the mean posting date for each group. They are now able to be fed into pytrend.
WordsToPytrend_nongeo.csv = non localized group\
WordsToPytrend.csv = Localized group

#### v.i) The results with columns (date, word_id, GT Influence Score, Definition) for inferential analysis.
Pytrends_final.csv = Google trends score for words with a country label.\
trends_set_nongeo.csv = Google trends score for words without a country labe.

#### v.ii) The results in matrix format for network visualization.
words_country.csv = Matrix with single where i = word, j = country, x_ij search values of word_i in country_j, related to words that have a country label.\
words_nongeo.csv = Matrix with single where i = word, j = country, x_ij search values of word_i in country_j, related to words that don't have a country label.


#### All other information can be found in the .pdf report.
