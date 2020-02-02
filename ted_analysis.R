Sys.setenv(TZ="US/Eastern")
Sys.getenv("TZ")
library(dplyr)
library(anytime)
library(ggplot2)
TED <- read.csv("ted_main.csv", header = TRUE)

#DATA CLEANING

# Check if there is any missing value
any(is.na(TED))

# Change number of speaker to factor
TED <- TED %>% mutate(num_speaker = as.factor(num_speaker))

# Convert unix time to standard time 
TED <- TED %>% mutate(film_date = anydate(film_date, tz = "US/Eastern"),
                      published_date = anydate(published_date, tz = "US/Eastern"))

# Split date into three columns -> currently just use year.
TED <- TED %>% dplyr::mutate(film_year = lubridate::year(film_date), 
                             film_month = lubridate::month(film_date), 
                             film_day = lubridate::day(film_date))
TED <- TED %>% dplyr::mutate(published_year = lubridate::year(published_date), 
                             published_month = lubridate::month(published_date), 
                             published_day = lubridate::day(published_date))

# Fix the inconsistent use of ; / 'and' ,  in speaker_occupation 
TED$speaker_occupation <- gsub(';', ',', TED$speaker_occupation)
TED$speaker_occupation <- gsub('and', ',', TED$speaker_occupation)
TED$speaker_occupation <- gsub('/', ',', TED$speaker_occupation)
TED$speaker_occupation <- gsub(' ,', ',', TED$speaker_occupation)

#EXPLORATORY DATA ANALYSIS

TED_group <- TED %>% group_by(published_year)%>%select(views, published_year)%>%summarise(Views=sum(views))
ggplot(TED_group, aes(x=published_year, y=Views)) + geom_line(color="#a70000") +
  geom_point(color="red") +labs(title="Graph 1 - Ted Talk Views Trend from 2006 to 2017",x= "Year") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
## Overall, TED Talk's views decreases from 2006 to 2017


# Find top 100 most-vieId videos 
# -> Group by year, count observations and calculate mean for each year.
TED_sort <- TED %>% arrange(desc(views))
TED_sort <- TED_sort[1:100,]
top100 <- TED_sort %>% group_by(published_year) %>% summarise(observations = n(), Average_Views = mean(views), Average_Comments = mean(comments))
top100

#########
## Process the "ratings" column in the orginial data so that it creates new columns of types of ratings 
## with the data is the count of corresponding rating
rate = TED[,"ratings"]
# Create a function that split a row of rating in the original dataset 
# into categories of ratings with their frequency.
rate_split <- function(list) {
  list <- toString(list)
  list = as.list(scan(text=list,what=","))
  ratings_type = vector()
  k = 0
  for (i in seq(5,123,by = 9))
  {
    k = k + 1
    ratings_type[k] = list[i]
  }
  ratings_count = vector()
  k = 0
  for (i in seq(9,126,by = 9))
  {
    k = k + 1
    ratings_count[k] = list[i]
  }
  ratings_count = toString(ratings_count)
  ratings_count = gsub('},,', '', ratings_count)
  ratings_count = gsub('}]', '', ratings_count)
  ratings_count = as.list(scan(text=ratings_count, what=""))
  ratings_count = as.numeric(ratings_count)
  ratings_type = unlist(ratings_type)
  A <- rbind(ratings_type,ratings_count)
  A <- t(A)
  A <- as.data.frame(A)
  A <- A[order(A["ratings_type"]),]
  A <- A %>% arrange(ratings_type)
  A <- t(A)
  A <- A[-1,] #remove the overlapping rownames of categories
  return (A)
}

# After running through the entire dataset, I figure out that
# there are 14 types of ratings that are the same for every row in ratings column
# So went back to the funciton and remove the overlapping rows of ratings categories
# The 14 categories of ratings are defined below:
rate_types = c("Beautiful","Confusing","Courageous","Fascinating",
               "Funny","Informative","Ingenious","Inspiring",
               "Jaw-dropping","Longwinded","Obnoxious","OK",
               "Persuasive","Unconvincing")

#Create a table with the first row is the 14 types of categories as 14 columns
rate_table <- rate_split(rate[1])
#Create the rest of the table from the data set
for(i in c(2:2550))
{
  rate_data <- rate_split(rate[i])
  rate_table <- rbind(rate_table, rate_data)
}
rate_table <- as.data.frame(rate_table)
# Add column names (14 types of ratings)
colnames(rate_table) <- rate_types
# Remove row names
rownames(rate_table) <- c()

# The count of each rating category is of type factor.
lapply(rate_table,class)
# So I decided to save it as a csv file and then import that file into R, which automatically convert them to numeric.
write.csv(rate_table, file = "Rate_Table.csv", row.names = TRUE)

# Import the file again
TED_rate <- read.csv("Rate_Table.csv", header = TRUE)
head(TED_rate)
# Add the rate table to the original dataset
TED_2 <- cbind(TED,TED_rate)
# Add all the counts of ratings of a video together
TED_2 <- TED_2 %>% mutate(Sum_ratings = Beautiful + Funny + Inspiring + 
                            Fascinating + Informative + Persuasive + 
                            Ingenious + Courageous + Obnoxious + Longwinded + 
                            Jaw.dropping + Unconvincing + Confusing + OK)
# Place 14 types of ratings into either positive or negative ratings
# Add all the counts of positive ratings of a video together
TED_2 <- TED_2 %>% mutate(Sum_pos_ratings = Beautiful + Funny + Inspiring + Fascinating + Informative + Persuasive + Ingenious + Courageous)
# Add all the counts of negative ratings of a video together
TED_2 <- TED_2 %>% mutate(Sum_neg_ratings = Obnoxious + Longwinded + Jaw.dropping + Unconvincing + Confusing)
# I leave "OK" out in the last two columns because it is neutral

# Create a table that gather information views, comments, and 3 types of Sum ratings
# Read my observations of this table in the report!
data_correlation <- TED_2 %>% dplyr::select(views,comments,Sum_pos_ratings,Sum_neg_ratings,Sum_ratings)
corr_matrix = cor(data_correlation)
corr_matrix 
# Draw a correlation plot to make observation
library(corrplot)
col_template<- colorRampPalette(c("#a70000", "white", "red"))(20)
corrplot(corr_matrix, type="upper",  tl.col="black")

# I want to put the published_year in three broader categories:
# 1: 2006 - 2009, 2: 2010 - 2013, 3: 2014 - 2017
# In order to do this, I create a function that helps determine whether a published year 
# belongs to group 1, 2 or, 3. Then I can execute the function for the entire dataset!
year_group <- function(year) 
{
  if (year <= 2009)
  {
    return ("2006-2009")
  }
  else if (year > 2009 & year <= 2013)
  {
    return ("2010-2013")
  }
  else
  {
    return ("2014-2017")
  }
}
#Extract the published_year column from the dataset 
# to run the function in a for loop
years <- TED_2$published_year

# Create a table that detemines if published_year belongs to group 1, 2, or 3
# The element of this table is corresponding to the order of the original dataset
year_table = vector()
for (i in c(1:2550))
{
  year_table[i] = year_group(years[i])
}

#Add the year_table column into the orginal dataset and name it Year_group
TED_2 <- TED_2 %>% mutate(Year_group = year_table) %>% mutate(Year_group = as.factor(Year_group))
#Create a scatter of representing duration on the x_axis, 
#views on the y-axis and with Year_group as grouping variable
title1 = "Graph 2 - Scatter plot of duration and views grouped by Year_group" 
ggplot(TED_2, aes(x=duration, y=views, color=Year_group)) + 
  geom_point() + labs(title = title1, x = "Duration (seconds)", 
                      y = "Number of views", color = "Year Group")

# I want to see if "Long-winded" means long speeechs. 
# Add a column representing the percentage of Long_winded out of Sum of all ratings
TED_2 <- TED_2 %>% mutate(Longwinded_perc = Longwinded/Sum_ratings)
title2 = "Scatter plot of duration and percentage of Longwinded" 
ggplot(TED_2, aes(x=duration, y=Longwinded_perc)) + 
  geom_point(color = "red") + labs(title = title2, x = "Duration (seconds)", 
                      y = "Percentage of Longwinded Rating")


#########
# I want to detemine if a video's number of views belongs to one of the three categories:
# 1: Low (below 25th percentile), 2: Medium (25th to 75 percentile), 3: High (above 75th percentile)  
perc25 <- quantile(TED_2$views, .25)
perc75 <- quantile(TED_2$views, .75)
# Create a function that detemines if a videos' number of views belongs to group 1,2, or 3
view_group <- function(num) 
{
  if (num < perc25)
  {
    return ("Low")
  }
  else if (num >= perc25 & num <= perc75)
  {
    return ("Medium")
  }
  else
  {
    return ("High")
  }
}
#Extract the languages column from the dataset 
views <- TED_2$views


# Create a table that detemines if a video's number of views is low, average or high
# The element of this table is corresponding to the order of the original dataset
views_table = vector()
for (i in c(1:2550))
{
  views_table[i] = view_group(views[i])
}
# Add it to the dataset
TED_2 <- TED_2 %>% mutate(Views_group = views_table) %>% mutate(Views_group = as.factor(Views_group))
# Create a scatter plot representing languages on the y-axis,
# published_date on the x_axis and grouped by Views_group
ggplot(TED_2, aes(x=published_date, y=languages, color= Views_group, shape = Views_group))+
  geom_point() + geom_smooth(method=lm, se=FALSE) +  
  labs(title = "Graph 3 - Number of languages for a TED talk from 2006 to 2017",
       x = "Published Date", y = "Number of languages", color = "Views Group", shape = "Views Group")


# Find top 10 speakers that deliver the most 
Top_Speakers <- TED_2 %>% group_by(main_speaker) %>% summarise (Number_of_Appearance = n()) %>% arrange(desc(Number_of_Appearance))
Top_Speakers <- Top_Speakers[1:10,]
Top_Speakers
write.csv(Top_Speakers, file = "TopSpeakers.csv", row.names = TRUE)

# Filter the data of top 10 speakers
Top_speakers_data <- TED_2 %>% filter(main_speaker ==  "Hans Rosling" | main_speaker == "Juan Enriquez" |main_speaker == "Marco Tempest"|main_speaker == "Rives"|main_speaker == "Bill Gates")
Top_speakers_data <- TED_2 %>% filter(main_speaker ==  "Hans Rosling" | main_speaker == "Juan Enriquez" |main_speaker == "Marco Tempest"|main_speaker == "Rives"|main_speaker == "Bill Gates"|main_speaker == "Clay Shirky"|main_speaker == "Dan Ariely"|main_speaker == "Jacqueline Novogratz"|main_speaker == "Julian Treasure"|main_speaker == "Nicholas Negroponte")
ggplot(Top_speakers_data, aes(x=published_date, y=views, color = main_speaker)) +
  geom_line() +geom_point() + geom_hline(yintercept=perc75,linetype="dashed")+ 
  geom_hline(yintercept=perc25,linetype="dashed") + 
  labs(title ="Graph 4 - The trend of views throughout the years \n for speakers making the most speeches", 
       x = "Published Date", y = "Number of Views", color = "Speaker") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank())

#Create a function that splits the tags string for a video
tag_split <- function(list) {
  list <- toString(list)
  list <- substr(list, 2, nchar(list)-1)
  tag_list <- as.list(strsplit(list, ",")[[1]])
  return (tag_list)
}

#Split the tags for all videos and add to a list containing all tags
tag.list <- tag_split(tags[1])
tag.list
for(i in c(2:2550))
{
  tag.list.current <- tag_split(tags[i])
  tag.list <- c(tag.list,tag.list.current)
}
tag_df <- data.frame(matrix(unlist(tag.list), nrow=length(tag.list), byrow=T))
head(tag_df)
colnames(tag_df)[colnames(tag_df)=="matrix.unlist.tag.list...nrow...length.tag.list...byrow...T."] <- "tag_types"

#Find the top 5 tags that appeared the most in TED talks
tags_top <- tag_df %>% group_by(tag_types) %>% summarise(observations = n()) %>% arrange(desc(observations)) %>% slice(1:5)
tags_top
#technology ranked top 1!
write.csv(tags_top, file = "top_tags.csv", row.names = TRUE)

#Create 5 columns that determine if a video's tags contain the top 5 tags
TED_2 <- TED_2 %>% mutate(tech_tag=ifelse(grepl("technology", tags),"1","0"), tech_tag=factor(tech_tag))
TED_2 <- TED_2 %>% mutate(sci_tag=ifelse(grepl("science", tags),"1","0"), sci_tag=factor(sci_tag))
TED_2 <- TED_2 %>% mutate(globe_tag=ifelse(grepl("global issues", tags),"1","0"), globe_tag=factor(globe_tag))
TED_2 <- TED_2 %>% mutate(design_tag=ifelse(grepl("design", tags),"1","0"), design_tag=factor(design_tag))
TED_2 <- TED_2 %>% mutate(culture_tag=ifelse(grepl("culture", tags),"1","0"), culture_tag=factor(culture_tag))

#Get the number of views for each tag in top 5
tech_data <- TED_2 %>% filter(tech_tag == "1") %>% dplyr::select(views)
sci_data <- TED_2 %>% filter(sci_tag == "1") %>% dplyr::select(views) 
globe_data <- TED_2 %>% filter(globe_tag == "1") %>% dplyr::select(views)
design_data <- TED_2 %>% filter(design_tag == "1") %>% dplyr::select(views)
culture_data <- TED_2 %>% filter(culture_tag == "1") %>% dplyr::select(views)

#Create a graph of boxplots that show the number of views
# for each tag in top 5.
par(mfrow=c(1,5))
a <- boxplot(tech_data$views, xlab="technology", ylab="views",ylim = c(0, 4000000), outline=FALSE,col="#b7ded2")
b <- boxplot(sci_data$views, xlab="science", ylab="views",ylim =c(0, 4000000),outline=FALSE, col ="#f6a6b2")
c <- boxplot(globe_data$views, xlab="global issues", ylab="views", ylim = c(0, 4000000),outline=FALSE, col = "#f7c297")
d <- boxplot(design_data$views, xlab="design", ylab="views", ylim = c(0, 4000000),outline=FALSE, col ="#ffecb8")
e <- boxplot(culture_data$views, xlab="culture", ylab="views",ylim = c(0, 4000000),outline=FALSE, col = "#90d2d8")
# technology does not bring about the highest number of views

#FITTING MODELS

#Select the variables
dataForModel <- TED_2 %>% dplyr::select(views,duration,languages,Sum_pos_ratings) 
head(dataForModel)
write.csv(dataForModel, file = "moldeling_data.csv", row.names = TRUE)


#Draw graphs to make observation
library(gridExtra)
language=ggplot(dataForModel, aes(x=languages, y=views))+geom_point()
duration=ggplot(dataForModel, aes(x=duration, y=views))+geom_point()
pos_ratings=ggplot(dataForModel, aes(x=Sum_pos_ratings, y=views))+geom_point()
grid.arrange(duration,language,pos_ratings, nrow=1)

#Fit model. See my observation on this model in the report!
m1=lm(views~languages+duration+Sum_pos_ratings, data=dataForModel)
summary(m1)


#Perform backward selection to find the best model
library(MASS)
m1_subsets= stepAIC(m1, direction = "backward", trace=FALSE)
summary(m1_subsets)
m1_subsets$anova
#The last model contain languages and Sum_pos_ratings only.

#SEE MY REPORT FILE FOR FULL ANALYSIS.

