##Code Courtesy: "An introduction to Facebook ad analysis using R" Kaggle kernel by ChrisBow
getwd()
setwd("C:/Users/EE User/OneDrive/Gopika/OMSA/MGT-6203/module 11")
#DataExplorer package for exploratory data analysis 
#Useful Documentation- https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html
if(!require(DataExplorer))install.packages("DataExplorer")
library(DataExplorer)
library(dplyr)
data<- read.csv("KAG_conversion_data.csv",stringsAsFactors = FALSE)
head(data)

#Dataset manipulation
#Drop fb_campaign_id
data<-data%>%
  select(-fb_campaign_id)
head(data)
#rename xyz_campaign_id
data<- data%>%
  rename(campaign_id=xyz_campaign_id)
head(data)
#Check the data dictionary provided with the code for meanings of various columns

#Columns: 
#
#1.) ad_id: unique ID for each ad. Acts as primray key/sole identifier for that ad 
#
#2.) campaign_id: an ID associated with each ad campaign of a company.
#
#3.) age: age of the person to whom the ad is shown.
#
#4.) gender: gender of the person to whom the add is shown
#
#5.) interest: a code specifying the category to which the person's interest belongs (interests are as mentioned in the person's Facebook public profile).
#
#6.) Impressions: the number of times the ad was shown.
#
#7.) Clicks: number of clicks on for that ad.
#
#8.) Spent: Amount paid by a company to Facebook, to show that ad.
#
#9.) Total conversion: Total number of people who enquired about the product after seeing the ad.
#
#10.) Approved conversion: Total number of people who bought the product after seeing the ad.


##Quickly exploring the dataset

#How many facebook ads do we have?
length(data$ad_id)
#1143

## How many unique facebook ad campaigns are here
length(unique(data$campaign_id))
#3

#what age groups were ads targeted at?
unique(data$age)
#"30-34" "35-39" "40-44" "45-49"

#Geners
unique(data$gender)
#M F

#Qiuck glimpse at the data and data types
glimpse(data)

# Gender and Age are Factors. let us change them into integer data as:
# replace character string age ranges with number(average age of the range)
#(it is necessary to convert them into categorical / numerical variables for the groupby nalysis)

data$age[data$age=='30-34']<-32
data$age[data$age=='35-39']<- 37
data$age[data$age=='40-44']<- 42
data$age[data$age=='45-49']<-47

#convert variable to integer
data$age<- as.integer(data$age)

#convert the gender variable to integer
data$gender[data$gender=='M']<-0
data$gender[data$gender=='F']<-1
data$gender<-as.integer(data$gender)


##checking for missing data
sapply(data,function(x) sum(is.na(x)))
#what does above command do?
#Above command simultaneously checks no of NA values in all columns of datasets"data"
#for eg: running um(is.na(data$gender)) gives number of NA values in"gendercolumn of data
#we want to do this for all columns without running it manually sapply() helps apply a function
#to each column of a dataset.

#Easier Alternative way to check for missing data(using dataExplorer Package)
plot_missing(data)


#descriptive statistics of the dataset
summary(data)


#######creating additional useful features

#1) click- through-rate (CTR). this is the percentage of how many of our impressions became clicks.
#A high CTR is often seen as a sign of good creative being presented to arelevant audiences.
#A low click through rate is suggestive of less-than-engaging adverts (design and / or messaging) and / or 
#presentation of adverts to an inappropriate audience. What is seen as a good CTR will depend on the type of 
#advert (website banner, Google Shopping ad, search network test ad etc.) and can vary across sectors, but 2% would be a reasonable benchmark.

#2) Cost Per Click (CPC). Self-explanatory this one: how much (on average) did each click cost. 
#While it can often be seen as desirable to reduce the cost per click, the CPC needs to be considered along with 
#other variables. For example, a campaign with an average CPC of £0.5 and a CR of 5% is 
#likely achieving more with its budget than one with a CPC of £0.2 and a CR of 1% (assuming the conversion value is the same.


#3) Cost per Conversion (CostPerConv_Total). This is the cost per 'conversion'. What a conversion is will be
#determined by the objectives of the campaign. It could be a partial sale, someone completing a contact form on a landing page,
#downloading an e-book, watching a video, or simply spending more than a particular amount of time or 
#viewing over a target number of pages on a website.

#4) Cost per Approved Conversion (CostPerConv_Approved). This is the cost per approved conversion (guaranteed sale).

#5) Cost per mille: Cost Per Mille (CPM). This number is the cost of one thousand impressions. 
#If your objective is ad exposure to increase brand awareness, this might be an important KPI for you to measure.


data<- data%>%mutate(CTR = round(((Clicks/Impressions)*100),4),
                     CPC=ifelse(Clicks!=0,round(Spent/Clicks,4),Spent),
                     CostPerConv_Total=ifelse(Total_Conversion !=0,round(Spent/Total_Conversion,4),Spent),
                     CostPerConv_Approved=ifelse(Approved_Conversion!=0,round(Spent/Approved_Conversion,4),Spent),
                     CPM=round((Spent/Impressions)*1000,2))
write.csv(data,"KAG_conversion_data_wrangled.csv")
head(data)



#########Analysis of dataset through some questions-

#How many ads belong to campaign(campaign_id)1178?
data%>%filter(campaign_id==1178)%>%nrow()
#625

#Which is the smallest and the largest campaign?
data%>%group_by(campaign_id)%>%summarise(n_ads=length(ad_id))
#Smallest-916, largest-1178


#How many ads which have never caused any approved conversion were targeted towards female audiences?
data%>%filter(Approved_Conversion==0)%>%group_by(gender)%>% summarise(count=length(ad_id))
#281


#What percentage of total ads(use ad_id for calculation) have never caused any aproved conversion?
(data%>%filter(Approved_Conversion==0)%>%nrow())/(data%>%nrow())*100
(nrow(data[data$Approved_Conversion==0,]))/(nrow(data))*100#traditional R syntax-also works
#48.9064%

##which campaign had the best brand awareness wrt the impression on an average?
data%>%group_by(campaign_id)%>% summarise(n_ads=length(ad_id),campaign_Impr=mean(Impressions))
#1178 -mean impression of 327718 per ad


#which campaign had spent most efficiently on brand awareness on an average (least cost per mile)?
data%>%group_by(campaign_id)%>% summarise(n_ads=length(ad_id),campaign_CPM=mean(CPM))%>%arrange(campaign_CPM)
#936 -0.224 CPM


#which ad was the most successful wrt approved conversions/
data%>%filter(Approved_Conversion==max(Approved_Conversion))
#1121104 - Approved Conversion:40

#Alternative way to do this -Arrange in descending order of approved conversion and check first row
data %>% arrange(desc(Approved_Conversion))


#How many ads which have not spent any money led to aproved conversions?
data%>% filter(Spent==0&Approved_Conversion!=0)%>%nrow()
#71

#which campaign did mot of such cost effective ads as described previously?
data%>% filter(Spent==0&Approved_Conversion!=0) %>%group_by(campaign_id)%>% summarise(n_ads=length(ad_id))%>%arrange(desc(n_ads))
#936 campaign id - 60 ads


#which ad generted least impressions?
data%>% filter(Impressions==min(Impressions))
#951641

#to get only ad_id above:
data%>%filter(Impressions==min(Impressions))%>% select(ad_id)


#which ad that costed the least based on CPC led to least impressions?
data%>% filter(CPC==min(CPC))%>% filter(Impressions==min(Impressions))%>% select(ad_id)
#951641
