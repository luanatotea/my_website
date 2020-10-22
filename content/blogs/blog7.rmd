---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: blog7
title: Ipsum
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(leaflet)
library(car)
library(huxtable)
```

# Introduction
Stockholm

Whoever said that beauty lies in the eye of the beholder most probably never visited the capital city of Scandinavia, Stockholm. It is the natural center and the largest city in the Scandinavian area built where lake greets the sea, at the junction of Lake Mälar (Mälaren) and Salt Bay (Saltsjön). With eight centuries of history and culture, Stockholm is regarded as the innovative trendsetter across all the creative industries ranging from fashion, technology, music film, design to the game industry. 

It is therefore no surprise that Stockholm is the most popular Swedish city for international travelers. Despite it being a compact city with an estimated metro area population of 1,63 million, over 15.3 million tourists were registered by the Swedish Agency for Economic and Regional Growth (Statistics Sweden) in 2019 alone. This represents a 5% increase in excess of 705 thousand since 2018 and 3.5 million more than five years ago.

##Executive Summary 
Looking at such an excess increase, this report aims to analyze data on the online apartment rental marketplace listings in Stockholm with the objective of fitting an optimal prediction model for the cost of two people staying a total of four nights in Stockholm. The data is from insideairbnb.com. It was originally scraped from the hospitality heavyweight Airbnb, the multi-sided marketplace that connects travelers with host and experience providers.

##Purpose 
To achieve the objective of this report, our group will interpret the set deliverable across four consequent facets-

1.	Perform a cohesive Exploratory Data Analysis (EDA) to investigate and develop an understanding of our data;

2.	Overlay all data coordinates in a geospatial format to get an overview of the spatial distribution of Airbnb rentals in Stockholm. Additionally, in order to get the statistical distributions of the data, our group will chart the data in plots to recognize patterns and select explanatory variables to perform a regression analysis;

3.	A comprehensive regression analysis will then be performed with selected variables. Models will then be built and evaluated. Furthermore, our group will determine should there be any variables demonstrating collinearity by calculating the “Variance Inflation Factor” of the best fit for purpose model. The variables used for the optimal model will then be summarized;

4.	Finally, from the aforementioned analysis, our group will evaluate and employ the optimal regression model to in order to predict the total cost of a four-night stay in a Stockholm Airbnb for two people.


##Methodology 
Through an overarching inspection carried out through functions glimpse() and skim() in order to get a better feel of the data, it was concluded that the raw dataset from insideairbnb.com shaped both quantitative as well as qualitative data from Airbnb in Stockholm. Subsequently, data manipulation and data wrangling were conducted to solve for duplicity and to homogenize the data for further exploration. 

Under a scrutinized look at the newly created dataset, it was concluded that further ostracization should be carried out in order to solve for the missing, incorrect and null values and ridy the data to be fit for the purpose. 

The resultant robust data set was then further explored through visualizations in the form of mappings. Regressions analysis and tabular summary of the data were conducted for analytical purposes.

##Limitations
Visualizations are a key component of understanding a data set and although R can be leveraged for geospatial visualizations, our analysis only reported back two maps of the city and an overview of the spatial distribution of Stockholm Airbnb rentals because of time constraints.

The regression analysis faced a number of limitations too on top of the time-sensitive nature of the report. We encountered duplicated or lackluster data entries as well as some which were redundant or incomprehensible because of misleading notations. These were eventually taken out of the analysis. 

Furthermore, on top of sub setting the data to create dataframes that are fit for our objective, our group had to make decisions and assumptions for the N/As and missing values and change these entries so to enhance the data’s usefulness and comprehension.



# Explanatory Data Analysis (EDA)
First, we need to import the datas from Airbnb databse

```{r load-data}
#Import data
listings <- vroom::vroom("http://data.insideairbnb.com/sweden/stockholms-l%C3%A4n/stockholm/2020-06-26/data/listings.csv.gz") %>% 
  clean_names()
```

We will now start exploring the data.

## How many variables/columns? How many rows/observations?
```{r EDA}
glimpse(listings)
```

There are 106 columns and 7635 rows(listed houses)

## Which variables are numbers?
```{r numbers}
skim(listings)
```
At first glance we discover the following -

- Character: 46      
- Date: 5       
- Logical: 16      
- Numeric: 39 

But some numeric variables are defined as character, such as price, weekly_price and so on. Therefore, we need to change the columns to transform then into numeric variables.
```{r changing_to_numeric}
Right_listing <- listings %>% 
        mutate(price=parse_number(price),
               weekly_price = parse_number(weekly_price),
               monthly_price = parse_number(monthly_price),
               security_deposit = parse_number(security_deposit),
               cleaning_fee = parse_number(cleaning_fee),
               host_response_rate = parse_number(host_response_rate),
               host_acceptance_rate = parse_number(host_acceptance_rate),
               extra_people = parse_number(extra_people),
               zipcode = parse_number(zipcode))
```

```{r numbers_right}
skim(Right_listing)
```
As we can see, now there are: 

- Character: 37
- Date: 5   
- Logical: 16     
- Numeric: 48

## Which are categorical or factor variables (numeric or character variables with variables that have a fixed and known set of possible values)?
To conduct this analysis we firstly need to define a range of distinct values to define it either as categorical or not. As a result, we defined that less than 41 unique values and more than 2 are considered categorical variables
```{r categorical}
Categorical <- Right_listing %>%
            summarise_each(funs(n_distinct)) %>%  #We calculate distinct values for each column
            t() %>% 
            as_tibble(rownames = "Variables") %>% #Convert back to tibble
            filter(V1>2,V1<41) %>% 
            arrange(V1)
```

Checking factors
```{r factors}
Right_listing %>% 
  select(Categorical$Variables) %>% 
  head()
```
These variable will be considered categorical: cancellation_policy, room_type, neighbourhood_cleansed , bed_type, property_type and neighbourhood. So, we need to turn them into factors.

```{r turnIntoFactor}
Right_listing <- Right_listing %>% 
        mutate(cancellation_policy = as.factor(cancellation_policy),
               room_type = as.factor(room_type),
               neighbourhood_cleansed = as.factor(neighbourhood_cleansed),
               bed_type = as.factor(bed_type),
               property_type = as.factor(property_type),
               neighbourhood = as.factor(neighbourhood)
               )
```

## Are there any entry that is not in Stockholm?

Further down in Section 3, after plotting the Map of all Stockholm districts, we realized that a lot of entries are surprisingly not located in Stockholm. Therefore our group reiterated the in the EDA section to treat the Stockholm data so that the only entries are located in Stockholm.
```{r cities}

#Distinct location values
Right_listing %>% distinct(smart_location, city)

```

At first glance, the number of entries that seem to be outside Stockholm are worryingly high, so after extensive research our group realized that a vast majority of these cities are in fact located in Stockholm because the city is built on a multitude of districts and islands. Even some vague values are in Stockholm, like the accommodations defined by Zipcodes (e.g. 114 24, 12573, 112 31 ), and even the one that is Korean (스톡홀름), Chinese (斯德哥尔摩) or Japanese (ストックホルム).

Admittedly, there are also locations that are not in Stockholm, such as Uppsala (a different Swedish city), Crown Point - Trinidad and Tobago(another country).  There is also a mistyped value "S, Sweden" and it was unclear whether it was actually Stockholm. All these value will be ostracized from our database.

```{r}
#Filtering our dataset
cleaned_listing <- 
  Right_listing %>%
  filter(city != "Uppsala", city != "Crown Point", city != "S")
```


```{r}
#Filtering our dataset
cleaned_listing <- 
  Right_listing %>%
  filter(city != "Uppsala", city != "Crown Point", city != "S")
```

## Districts of Stockholm - Classification

```{r}
cleaned_listing %>% 
  group_by(neighbourhood_cleansed) %>% 
  count()  %>% 
  arrange(n)
```
The dataset correctly displays all 14 districts of Stockholm. These are further grouped into three main areas as listed below  - Center, Söderort (South Stockholm) and Västerort (West Stockholm).

Center:
Kungsholmen
Norrmalm
Södermalm
Östermalm

Söderort (South Stockholm)
Enskede-Årsta-Vantör
Farsta
Hägersten-Liljeholmen
Skarpnäck
Skärholmen
Älvsjö


Västerort (West Stockholm)
Bromma
Hässelby-Vällingby
Rinkeby-Kista
Spånga-Tensta

```{r}
#creating a list with the correspondent values
neighbourhood_region <- 
  list("Spånga-Tensta"= "Västerort",
     "Skärholmens"= "Söderort",
     "Rinkeby-Tensta"= "Västerort",
     "Älvsjö"= "Söderort", 
     "Hässelby-Vällingby"= "Västerort",
     "Farsta"= "Söderort",
     "Skarpnäcks"= "Söderort",
     "Bromma"= "Västerort",
     "Enskede-Årsta-Vantörs"= "Söderort",
     "Östermalms" = "Center", 
     "Hägersten-Liljeholmens" = "Söderort", 
     "Kungsholmens" = "Center",
     "Norrmalms" = "Center" , 
     "Södermalms" = "Center") 


#The newly defined list will then be converted into a data frame
neighbourhood_region_list <- 
  data.frame(I(neighbourhood_region))


#Row levels are then converted to columns
neighbourhood_region_list$neighbourhood_cleansed <- 
  rownames(neighbourhood_region_list)

#deleting current row headers (names)
rownames(neighbourhood_region_list) <- 
  c()

#A Left_join is used to join the new distributed neighborhoods to the primary dataframe cleaned_listing

cleaned_listing <- left_join(cleaned_listing,neighbourhood_region_list, by = "neighbourhood_cleansed")

#cThe joined column is firstly converted to a chr and then  transformed in to a factor c

cleaned_listing$neighbourhood_region = as.character(cleaned_listing$neighbourhood_region)
cleaned_listing$neighbourhood_region = as.factor(cleaned_listing$neighbourhood_region)

glimpse(cleaned_listing)

```


A new column will be added to the dataframe under “neighbourhood_region” classification

## Any NAs?
To produce a relevant work based on a database, we need to know if there is any NA value and if yes, if we need to change them or they are just part of our database. But before that, we will create a dataframe with fewer column because we do not need all of them.

```{r cleaned_listing}
cleaned_listing <- 
  cleaned_listing %>%
      select(id,
             listing_url,
             price,
             cleaning_fee,
             bed_type,
             cancellation_policy,
             last_review,
             last_scraped,
             latitude,
             longitude,
             bedrooms,
             beds,
             accommodates,
             security_deposit,
             monthly_price,
             extra_people,
             minimum_nights,
             guests_included,
             bathrooms,
             availability_365,
             availability_60,
             number_of_reviews,
             review_scores_rating,
             review_scores_cleanliness,
             host_is_superhost,
             reviews_per_month,
             instant_bookable,
             property_type,
             room_type,
             is_location_exact,
             neighbourhood_cleansed,
             neighbourhood,
             neighbourhood_region)
 

```

Now, we can check NAs
```{r NAs}
skim(cleaned_listing)

```
As we can see, some variables have missing values. For cleaning_fee, we are going to suppose that those host do not charge that fee and we will change to 0. For variables such as bedrooms and bed we are going to exclude them. For last_review, review_score variables and reviews_per_month we believe that these offers were not reviewed yet so we will keep them. Also, for monthly_price we suppose that missing values are because some offers do not have a special price for month rents and for security_deposit we assume that some hosts do not charge them.

```{r excludeNA}
cleaned_listing <- cleaned_listing %>% 
          drop_na(bedrooms, beds)
cleaned_listing <- cleaned_listing %>% 
          mutate(cleaning_fee = case_when(is.na(cleaning_fee) ~ 0,
                                TRUE ~ cleaning_fee))
cleaned_listing <- cleaned_listing %>% 
          mutate(bathrooms = case_when(is.na(bathrooms) ~ 0,
                                TRUE ~ bathrooms))
```

## Property Types
We will analyze "Property Types" column

```{r Property Types}

PropType_Per <- 
  cleaned_listing %>% 
  group_by(property_type) %>% 
  summarize(n = n()) %>%
  mutate(Frequence = n/sum(n)) %>%
  arrange(desc(n))
PropType_Per

  #The percentage for the top 4 categories is summed up
  PropType_Per %>% 
    top_n(4, Frequence) %>% 
    summarize(sum(Frequence))

```

The top 4 type of property are: Apartment, House, Townhouse and Loft and they represent 93,7% of all accommodations.

As requested, we will now create a simplified variable for property types.
```{r roperty Types}
#First, we need to transform Property Type in Character or the code below will fail
cleaned_listing <- 
  cleaned_listing %>%
  mutate(property_type = as.character(property_type))

#Now we can create the new variable
cleaned_listing <- cleaned_listing %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("Apartment","House", "Townhouse","Loft") ~ property_type, 
    TRUE ~ "Other"
  ))
  
#Now we transform the into factor and them see if it worked

cleaned_listing <- 
  cleaned_listing %>%
  mutate(property_type = as.factor(property_type),
         prop_type_simplified = as.factor(prop_type_simplified))
  
cleaned_listing %>%
  count(property_type, prop_type_simplified) %>%
  arrange(desc(n))  
```
It is working!

## Histograms: Price

```{r Price}
# Calculating the favstats for price
fav_stats(cleaned_listing$price)
```

Price range is really high, but we can try to plot it. The graphic below shows that prices are mainly concentrated under 2500 Eur/day, whilst other values are spread out across the x axis. There is a high concentration on price 13867. To solve this we created a table that shows the apartments under this price. Analyzing these apartments and theirs URLs, we realized that they do not even exist, also the have almost the same summary and the same latitude and longitude, so we decided to ostracize them from the data.

Examples of these accommodation reduntant to our analysis are displayed below -

```{r Price Plot}
#Histogram for Price
ggplot(cleaned_listing, aes(x = price)) + geom_histogram(binwidth = 50)

#Analysing anomalie at Price = 13867.
Filter_table <- cleaned_listing %>%
        filter(price == 13867)
head(Filter_table)

#Excluding offers with Price = 13867.
cleaned_listing <- cleaned_listing %>% 
          filter(price != 13867)

#Now we can check the histogram again
ggplot(cleaned_listing, aes(x = price)) + geom_histogram(binwidth = 50)

```

## Histograms: Reviews Score

To understand the satisfaction of customers with AirBNB's locations in Stockholm, we will analyse the histogram and the favstats for that variable.

```{r Review}
# Calculating the favstats for price
fav_stats(cleaned_listing$review_scores_rating)
```

As we can see, reviews are really concentrated above 90, which is really good because the standard deviation is small and the mean is considerably high. 

```{r Review Plot}
#Histogram for Price
ggplot(cleaned_listing, aes(x = review_scores_rating)) + 
    geom_histogram(binwidth = 1)+
    labs(title = "Reviews are left-skewed",
    subtitle =  "but because the are highly concentraded at 100 and cannot go beyond that.")

```

## Minimum Nights
Repeating the Property Type's process with minimum nights, we will analyze minimum nights for Airbnb's accomodations in Stockholm.

```{r MinNights}
Min_Nights <- cleaned_listing %>%
               group_by(minimum_nights) %>% 
               summarize(n = n()) %>%
               mutate(Frequence = n/sum(n)) %>% 
               arrange(desc(n))
Min_Nights
#Let's sum the percentage for the top 4 categories
Min_Nights %>% 
  top_n(4, Frequence) %>% 
  summarize(sum(Frequence))

```
1,2,3 and 4 are most common values, 30 and 14 appear relevant, but together they represent 2,7% of the total of accommodations. The data is then filtered for accommodations of only 4 days. The reason for this data manipulation is because, further down in Section 4 we will use the price of 4 nights as our dependent variable to run regression analysis.

```{r Min Nights}
cleaned_listing <- 
  cleaned_listing %>% 
  filter(minimum_nights <= 4)

cleaned_listing %>%
  count(minimum_nights) %>%
  arrange(desc(n))  
```

## What are the correlations between variables? Does each scatterplot support a linear relationship between variables? Do any of the correlations appear to be conditional on the value of a categorical variable?

Based on the skim that we did before, we can the correlation between numerical variables.
```{r correlation}
cleaned_listing %>% 
ggpairs(columns = c("price",
                  "bedrooms",
                  "beds",
                  "accommodates",
                  "bathrooms",
                  "cleaning_fee",
                  "review_scores_rating",
                  "number_of_reviews",
                  "guests_included",
                  "extra_people"))
```
We can now analyze the correlations between crucial data:

- Price: Weak correlation (<0,4) with all other variables, therefore, we cannot use any numeric variable to express price and the opposite is also true.
- Bedrooms: Strong correlation(>0,7) with number of beds and accommodations and a moderate(>0,4) with number of bathroom. Due to house proportionality it makes sense and was already expected.
- Bed: A strange negative correlation(<0) with number_of_reviews, it may show to us that larger houses tend to have fewer customers than small accommodations.
- Guest Included: Moderate(>0,4) with number of extra people.

Other correlations are not relevant to our analysis because they do not provide us any relevant information.


# Mapping
```{r MAP}
leaflet(data = filter(cleaned_listing, minimum_nights <= 15)) %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillColor = "blue", 
                   fillOpacity = 0.4, 
                   popup = ~listing_url,
                   label = ~property_type)
```
## Clustering Stockholm's Airbnb Properties
```{r}
map_cluster <-leaflet(data = cleaned_listing) %>% 
  
  # Map with OpenStreetMap.Mapnik
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  
  # Centered on the coordinate of Stockholm
  setView(lat = 59.334591, lng = 18.063240, zoom = 11) %>% 

  # Add circle markers with popups and labels
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillOpacity=0.6,
                   popup = ~listing_url,
                   label = ~property_type,
                   
                   # cluster 
                   clusterOptions = markerClusterOptions())
#print map_cluster
map_cluster
```


# Regression Analysis

The objective of this report is to predict the price for a 4-night stay in a Stockholm Airbnb for a total of four nights. In order to make this prediction, our group will run a linear regression with the price for four nights as the Y variable. We hope to achieve a equation that represents the variability of price for 4 nights.

## Price for 4 nights
To start our regression, we need to create the variable that expresses that price of 4 nights for each accommodation.
```{r create 4_night}
cleaned_listing <- 
  cleaned_listing %>% 
  mutate(price_4_nights = price * 4 +
           cleaning_fee +
           ifelse(guests_included >= 2,0, extra_people))

#As usual, let's check to see if it's working
cleaned_listing %>% group_by(price_4_nights) %>% count()
```
Now, we will plot on a histogram to analyze the distribution of prices. Already supposing that the range is high, we will also plot a histogram in a log scale.
```{r plot hist}

ggplot(cleaned_listing, aes(x=price_4_nights)) + geom_histogram(binwidth = 50)

```

As we can see, there are 2 outliers with price over 50.000,00 we access the URL (<https://www.airbnb.com/rooms/4634166> and <https://www.airbnb.com/rooms/38602945>) and the price on the dataframe is wrong, so we are going to delete them and run the histogram again.

```{r filteroutlier}
cleaned_listing <- 
  cleaned_listing %>% 
  filter(price_4_nights <= 50000)

ggplot(cleaned_listing, aes(x=price_4_nights)) + geom_histogram(binwidth = 100)
```

The distribution is right-skewed as expected, since price can not be negative. Let’s see if the log function will help us interpret our results better.

```{r plotlog}
ggplot(cleaned_listing, aes(x=log(price_4_nights))) + geom_histogram()
```
This plot above shows a normal distribution, therefore, we will use the log of the price for 4 night as our Y variable.

```{r transftolog}

cleaned_listing <- cleaned_listing %>% mutate(price_4_nights = log(price_4_nights))

```
## Basic Models
### Model I 

The first model consider solely prop_type_simplified, number_of_reviews, and review_scores_rating and the second adding room_type to that. Both with the goal of explaining the price of a 4-night stay.

```{r model1}
model1 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating, 
             data = cleaned_listing)

model1 %>% tidy(conf.int=TRUE) 
```

```{r glance 1}
glance(model1)
```

By looking at the numerical variables (number_of_reviews and review_scores_rating), we see that the p values and statistics (>│2│) for all the variables are significant, so we should include them in our model. The estimate means that for each additional review score rating, the price would increase by 0.00832% as for the number_of_reviews for each additional number of reviews the price would drop by 0.00125%.

The adjusted R square of this model is 0.5, which means that 5% of the variance in price_4_nights can be explained by this model. This is not a sufficient value. Thus, we continue our regression analysis by adding additional variables. 

N.B. The category "apartment" was used as reference to calculate price changes and it is why it does not appear. Looking to prop_type_simplified, it is evident that Townhouse and Other are not significant to the model. The model shows that if a tourist changes from an apartment to a house, the price would increase by 0,419%, on log scale, as (exp(0,419)-1)100 = 52,04% on a "normal" scale. Similarly, when a tourist shifts from an apartment to a loft, the price would increase by 0,419%, on log scale, as (exp(0,54)-1)100 = 71,6% on a "normal" scale.

Now we will check if room_type is a significant predictor of the variable correlated to the price.

### Model II

```{r model2}

model2 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type,
             data = cleaned_listing)

model2 %>% tidy(conf.int=TRUE) 

```

```{r}
model2 %>% glance()

```

Based on above findings, Entire home/apt was used as reference to the model for the room type. All room types are statistically relevant. If a tourist shifts from an “Entire home/apt” to "Hotel room", “private room” or “shared room” there is a decrease of 0,272%, 0.699% and 1.03%, respectively, in the final price.

The adjusted r-squared is only 0,299, which means that only 29,9% of our values is covered by this model.

## Further models to explore
The Stockholm dataset has many more variables to further explore in the analysis. We will proceed by adding the following 6 models -

### Model III

To further investigate the significance of the property capacity to any changes in price our group we will add the bathrooms, beds and bedrooms variables to Model 2. Additionally, variable accommodates storing the number of people each property has capacity for will be included. 

```{r}
model3 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type +
               bathrooms +
               bedrooms +
               beds +
               accommodates,
             data = cleaned_listing)

model3 %>% tidy(conf.int=TRUE) 


```
```{r}
model3 %>% glance()
```

#### Model III - extended analysis 
Further exploration of Model 3 for a more scrutinous look

```{r}
model3_1 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating +
               room_type +
                bedrooms +
                 bathrooms +
               accommodates,
             data = cleaned_listing)

model3_1 %>% tidy(conf.int=TRUE) 


```
```{r}
model3_1 %>% glance()
```


We firstly notice that by adding the explanatory variables bedrooms, and beds and accommodates that they have statistical significance by looking at the “statistics” and the “p-value”. It would imply that each of the variables is a statistically significant predictor. Admittedly, through further exploration in the section above, we notice that the beds variable is not a very optimal predictor of the changes in price for a 4-night stay.

When adding bedrooms and bathrooms to our model we obtained an R squared of 0.42. We notice that the R squared does not show a significant decrease after ostracizing the beds variable.

Ceteris paribus, an average of 10.18%, 16.27%  and respectively 8%  higher price per stay is associated for each additional bedroom, bathroom and accommodates (representing each property's capacity). This further analysed Model III is optimal and it adds robustness to Model II.

Model III Performance

It is concluded that  the adjusted R-squared has significantly increased from the addition of these variables. The model now explains approximately 43% of variation in the price of travelers' stay. Despite requiring further analysis for accuracy, is is a refined version of the basic representation of Model 2. 

Consequently, we will continue to explore should there be any additional variables that could aid our best fit for purpose model.

### Model IV

We continue to explore if Superhosts command a pricing premium are relevnt to our analysis. We will build on Model II. 

```{r}
model4 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               host_is_superhost,
             data = cleaned_listing)

model4 %>% tidy(conf.int=TRUE) 
```
```{r}
model4 %>% glance()
```

We firstly notice that by adding the explanatory variable host_is_superhost that it weighs statistical significance by looking at the “statistics” and the “p-value”. Admittedly, we notice that  price is negatively correlated by 0.097%.

Performance
This model not only explains only 5.5% of the Stockholm variables but it substantially decreases the coverage from model II. Conclusively, we will not include it in the subsequent regressions models.

### Model V
We continue to explore whether the exact deployment of a location could serve as a significant predictor for the price of a 4-night stay.

```{r}
model5 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type +
               is_location_exact,
             data = cleaned_listing)

model5 %>% tidy(conf.int=TRUE) 
```


```{r}
model5 %>% glance()
```

By looking at variables is_location_exactTRUE, we see that the p values and statistics (>│2│) for is significant, so we should include them in our model. The estimate means that when the exact location is displayed, the price would increase by 0.02760%.

Performance
Admittedly, this model only explains 28.9% of our variables. Therefore, it did not increase/decrease the coverage from Model II so it will be left out from our group's further regression analysis.


### Model VI

We continue to explore whether the neighborhood is significant to determine the price. 

For this analysis we will use the categorical variable “neighborhood_region”. Earlier in the Exploratory Analysis section, our group sectioned the neighborhoods into 3 main areas of Stockholm  i.e.  Center, Söderort (South Stockholm) and Västerort (West Stockholm). This model is a build up from Model II.

```{r}
model6 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type +
               neighbourhood_region,
             data = cleaned_listing)

model6 %>% tidy(conf.int=TRUE) 
```
```{r}
model6 %>% glance()
```

We firstly notice that by adding this explanatory variable that it weighs statistical significance by looking at the “statistics” and the “p-value”.

Looking at the above output, we notice that the “Center” is the reference variable used to calculate the price difference and this is the reason why it does not appear. The model shows that if we change either from the Center to Söderort (South Stockholm) or from the Center to Västerort (West Stockholm) the price in rentals will significantly drop. This is justifiable since Stockholm holds in the center its extremely affluent areas with the most upmarket shops, restaurants and bars. 

Performance 
Adding the neighbourhood variable, it is an evident improvement in the adjusted r-squared from 28% to 39%. We will continue to explore to determine our best fit for purpose model.


### Model VII

For this analysis we will use the categorical variable “cancellation_policy” to explore further whether offers which include a cancellation policy have any effect on the price of a 4-night Airbnb stay for two people. This model is a build up from Model II.


```{r}
model7 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type +
               cancellation_policy,
             data = cleaned_listing)

model7 %>% tidy(conf.int=TRUE) 
```
```{r}
model7 %>% glance()
```

We firstly notice that by adding this explanatory variable that it weighs statistical significance by looking at the “statistics” and the “p-value”.

Performance
We can see that our adjusted r-squared has increased from 28% to 30%. We conclude that this is an enhancement to our regression model.

### Model VIII
Having explored all these variables, it is now time to join the ones we concluded as optimal for our regression model to get the most significant adjusted R sq. 

```{r}
model8 <- lm(price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type +
               bedrooms +
               bathrooms +
               accommodates+
               neighbourhood_region +
               cancellation_policy,
             data = cleaned_listing)

model8 %>% tidy(conf.int=TRUE) 
```
```{r}
model8 %>% glance()
```
Through our regression analysis it was concluded that most of these variables we explored are significant for the model and affect the change in price as we noticed the evident increase in the adjusted r-squared to 55%. Hence, it was concluded that Model VIII including all the significant variables discussed above is the optimal fit for the purpose of this report. 
 
A diagnosis of our selected model will be carried out to check for residuals and collinearity in Model VIII.

## Diagnostics, collinearity, summary tables

### Check the residuals
```{r}
library(ggfortify)

autoplot(model8)+
  theme_bw()
```
Our interpretation:

Normal Q-Q: The residuals follow a nomal distribution as they are mainly found on the straight line. Admittedly, there are some outliers in the price as there is a small but visible deviation from the line. 

Residuals vs Fitted: There is no recognizable pattern in the residuals with a focus on y=0, this means that there is no pattern that is currently unexplained. 

Residuals vs. Leverage: The number of points in the plot is not significant suggesting that the relationship between leverage and residuals is not fulsome, and therefore there is no strong influence in between the model parameter estimates.

Scale-Location: The fitted line showcases a decipherable negative slope. 



#Check for Collinearity
```{r}
vif(model8)
```
```{r}
huxreg(list("Model_8" = model8))
```

# Price Prediction

Based on our last model, we are going to use it to predict the price of 2 people staying 4 nights in an Airbnb in Stockholm with a few restrictions: Apartment with a private room, have at least 10 reviews, and an average rating of at least 90. On our first prediction, we will create a fake apartment and use Model VIII to estimate the price of this fake apartment.

```{r Preditction}
library(lubridate)

#We will first create the parameters of our "imaginary_airbnb"
Created_Airbnb <- tibble(prop_type_simplified = "Apartment",
                           number_of_reviews = 10,
                           review_scores_rating = 90,                                                
                           room_type = "Private room",
                           beds = 1,
                           bathrooms = 1,
                           bedrooms = 1,
                           accommodates = 2,
                           cancellation_policy = "flexible",
                           neighbourhood_region = "Center")


#We will use the broom::argument() to predict the price of our airbnb, it will also add the SEs.
model_predictions <- broom::augment(model8, 
                                    newdata = Created_Airbnb, 
                                    se_fit=TRUE)

#Now we will calculate the lower and upper confidence intervals to reach 95%
model_predictions <- model_predictions %>% 
  mutate(
    lower = .fitted - 1.96 * .se.fit,
    upper = .fitted + 1.96 * .se.fit
  )

#save lower, fiited, upper and SE values into a variable
results <- model_predictions %>% select(lower,.fitted,upper, .se.fit) 

#we have now to transform our prices from log(euro) to euro
results <- results %>% mutate(lower = exp(lower),
                              .fitted = exp(.fitted),
                              upper = exp(upper),
                              .se.fit = exp(.se.fit))

results

Night_4P <- cleaned_listing %>% 
            filter(prop_type_simplified == "Apartment" & 
                     room_type  == "Private room" &  
                     number_of_reviews >= 10 & 
                     review_scores_rating >= 90,
                     neighbourhood_region == "Center") %>% 
            mutate(price_4_nights = exp(price_4_nights))
mean(Night_4P$price_4_nights)
median(Night_4P$price_4_nights)
```

As expected and described in the last section, our model represents the reality with an accuracy of 55% and we did not expect that our model would provide a precise estimate for the price of 4 nights. The mean price of all apartments that fitted our restrictions is 2852 and the median is 2542, since there are some extreme values and, as shown in the Exploratory Data section, some expensive properties may not even exist. Therefore, the median is a better value to make the comparison, since the Mean is influenced by some extreme values.

Our model predicts a price between €2339 and €2540, so as expected, our model will not have a relevant accuracy, however, it can provide at least a reasonable estimate.


Now, for our second attempt, we will use the characteristics of properties that already exist and use our model to predict the price.
```{r}
#Using the same filter that we use on the first prediction
filter_of_listings <- cleaned_listing %>% 
  filter(property_type == 'Apartment',
         room_type == 'Private room',
         review_scores_rating >= 90,
         number_of_reviews >= 10,
         neighbourhood_region == "Center") %>% 
#Create a new column to add the price of our prediction
         mutate(prediction = exp(predict(model8, .))) 
```

```{r}
#Calculating stats for our estimation
Upper <- quantile(filter_of_listings$prediction, 0.975)
Lower <- quantile(filter_of_listings$prediction, 0.025)
Mean <- mean(filter_of_listings$prediction)

#Plotting the data to a histogram to see how predicted price flutuate
ggplot(filter_of_listings, aes(x = prediction)) +
  geom_histogram() +
  geom_vline(xintercept = Mean, size = 0.7, color = 'blue') + 
  geom_vline(xintercept = Upper, size = 0.7, color = 'green') +
  geom_vline(xintercept = Lower, size = 0.7, color = 'red') +
  annotate(geom = 'text', label = paste("Point Prediction:", round(Mean)), x = Mean + 50, y = 40, colour="black", angle=300) +
  annotate(geom = 'text', label = paste("Upper 95% CI:", round(Upper)), x = Upper + 50, y = 40, colour="black", angle=300) +
  annotate(geom = 'text', label = paste("Lower 95% CI:", round(Lower)), x = Lower+ 50, y = 40, colour="black", angle=300) +
  theme_fivethirtyeight() +
  labs(y = "count", x = "Price: 2 Guests for 4 Nights", 
       title = 'Confidence Interval for a prediction of the Price of a Stay in a Stockholm',
       subtitle = 'Histogram of estimated price')

```
Based on the database that we imported from Airbnb, the confidence interval to predict how much it would cost 2 people to stay 4 nights in Stockholm, sleeping at an apartment with a private room that has at least 10 reviews, and at least 90 as average rating goes from 2117 to 3496 When we compare the mean of 2648 from our model to the mean of 2852, we see that our model would provide a reasonable estimate. Also, the median of our model is below the mean, the same phenomenon that happens with the real price. So, it enhances even more that our model provides, at least, a reasonable estimate.

The values from the two estimates are a little bit different and the reason is because the second prediction uses real values for all variables, while our first prediction uses the same values.

# Conclusion

Our model covers 55% of the price of Airbnb in Stockholm. It is not a high value, but as shown by our analysis, it can reasonably predict  the price. Our model predicted, with a 95% confidence that the cost of 4 night stay for 2 people in the Stockholm would range from US$2117 to US$3496.