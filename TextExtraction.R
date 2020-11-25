## The data from Vidgen et al.(2020) is being used

### Functions ###############################################################
## Clean Tweets removes unwanted aspects of the tweet
clean_tweet <- function(x){
  x %>% 
    rm_twitter_url() %>% ## removes twitter links
    str_replace_all("&amp;", "and") %>% ## replaces &amp with and 
    str_remove_all("^RT:? ") %>%  ## removes all retweets
    str_replace_all("\\\n", " ") %>% ## replaces newlines with space
    rm_title_name(pattern = "@[[:alnum:]]+") %>% ## removes userName
    rm_between(left = "<", right = ">") %>%  ## removes emojis/unicode character
    rm_title_name(pattern = "#[[:alnum:]]+")%>%  ## removes all hashtags
    str_replace_all("[[:punct:]]", " ") %>%  ## removes all punctuation
    str_to_lower() %>%  ## makes all words lower case 
    str_trim("both") ## removes the whitespace at the start and end of the tweets
}
#### Loading Data ###########################################################
## Data contains 20k tweets that have been classified manually by experts 
## if they contain hate or not (Vidgen et al., 2020)

df <- read.table(paste0(getwd(),"/TwitterData/20Kdata.tsv"), sep= "\t",
                 fill = T, header = T)
##### Extracting the relevant parts of the data #################################
### The data has 2 people decide if there is hate speech or not if they disagree 
## then a more senior adjudicator is used to determine the outcome 
## For this task then I need the tweet and the outcome from the adjudicator 
data <- df %>% 
  select(text, expert)

## They have split the data into 5 categories neutral, counter hate, discussion
## of prejudice, criticism, and hostility 
## Adapting our data for the Fortuna and Nunes definition of hate speech we will
## use hostility and criticism to mean hate speech and all others to be neutral 

levels(data$expert)[1] <- "Neutral"
levels(data$expert)[2] <- "Neutral"
levels(data$expert)[2] <- "Hate"
levels(data$expert)[3] <- "Hate"
levels(data$expert)[3] <- "Neutral"
names(data)[names(data) == "expert"] <- "Classification"


### Cleaning up the tweets ####################################################

## Using the clean_tweet function described above
data$text <- clean_tweet(data$text)
data$text <-  gsub("[^\x01-\x7F]", "",data$text) ##removes any other text that is
                                      			           ##is not in ASCII range Hx20 to Hx7E
### Stemming Words
## This is so words that have different tenses will be counted as the same word
data$text <- stemDocument(data$text)
## remove any duplicated tweets now links and usernames have been removed 
length(data$text)## 19,993 tweets
data <- data[!duplicated(data$text),]
length(data$text) ## 19,093 tweets 900 tweets have been removed 
### selecting the appropriate number of tweets for the problem #################

## We are trying to develop a model that accurately  labels if a tweets is hate 
## or not whenever it starts to be a trending topic, this means that fewer a low
## number of tweets is needed to do this initial automatic detection 
## we have 19,093 tweets, a trend can start with 5000 tweets so this should be
## the number of tweets that we use
## giving tweets an ID
data$ID <- c(1:nrow(data))
## need to randomly pull 5000 tweets from this dataset 
## Use a stratified sampling method to limit bias i.e. the proportion of hate tweets 
## will be the same in the sample of tweets as it was in the entire population 
set.seed(40126429) ## student number as seed
propH <-as.double( count(data,Classification)[2,2]/nrow(data)) 
## as.double to force it to be a vector value instead of a data frame
##  0.2702037 <- The proportion of hateful tweets 
NumH <- ceiling( 5000*propH) ## Number of hate tweets in the sample  ##1352
NumN <- floor(5000*(1-propH)) ## Number of Neutral tweets in sample  ##3648 
Hdf <- data %>% ## Gathering all Hate tweets to take a sample of ID's
            filter(Classification == "Hate")%>%
            select(ID)  

Ndf <- data %>% 
            filter(Classification == "Neutral") %>% 
            select(ID) 
HsamID <- sample(Hdf$ID, size = NumH) ## List of hate tweet IDS          
NsamID <- sample(Ndf$ID, size = NumN) ## List of Neutral tweet IDS
TweetIDs <- c(HsamID, NsamID)
DataSample <- data %>% ## This can be used to build the model 
                    filter(ID %in% TweetIDs) %>% 
                    select(text, Classification)
DataRemain <- data %>% ## This can be used for blind testing the model 
                    filter(!ID %in%  TweetIDs) %>% 
                    select(text, Classification)
### Saving data for future work ################################################
write.csv(DataRemain, paste0(getwd(),"/twitterdata/RemainingdData.csv"), row.names = F)
write.csv(DataSample, paste0(getwd(),"/twitterdata/SampleData.csv"), row.names = F)
