---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: Risk return of DJIA stocks
draft: false
image: pic11.jpg
keywords: ""
slug: blog5
title: Risk return of DJIA stocks
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


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library (dplyr)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(scales)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest)    
library(purrr)  
library(lubridate)
library(patchwork)
library(gt)
library(tidytext)
library(ggrepel)

```


Next, let's choose the [Dow Jones Industrial Aveareg (DJIA)](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) stocks and their ticker symbols and download some data. Besides the thirty stocks that make up the DJIA, we will also add `SPY` which is an SP500 ETF (Exchange Traded Fund).


```{r, tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


table1 <- djia[[2]] %>%
  mutate(date_added = ymd(date_added),
         
    
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>%
  c("SPY")

```




```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) 
```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

Create a dataframe and assign it to a new object, where you summarise monthly returns since 2017-01-01 for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}

after_20170101 <- myStocks_returns_monthly %>%
    filter(date >= "2016-12-30")

after_20170101 %>%
  group_by(symbol) %>%
  summarise(Minimum_Perc = min(monthly_returns*100), Maximum_Perc = max(monthly_returns*100), Median_Perc = median(monthly_returns*100), Mean_Perc = mean(monthly_returns*100), Standard_Deviation_Perc = sd(monthly_returns*100))
```


Plot a density plot, using `geom_density()`, for each of the stocks
```{r density_monthly_returns}

# YOUR CODE GOES HERE
 ggplot(data = after_20170101, mapping = aes(x = monthly_returns)) + 
     geom_density() +
     facet_wrap(~ symbol)+
     labs(y= "Density", x = "Monthly returns", title = "Density of Monthly Returns by Company")+
  NULL


```

What can you infer from this plot? Which stock is the riskiest? The least risky? 

> TYPE YOUR ANSWER AFTER (AND OUTSIDE!) THIS BLOCKQUOTE.

The riskest should be AAPL since it has the most spreaded monthly returns, while the least risky is KO since most of its monthly returns accumulate within a limited range. Due to the inate diversification of SPY, it is less "risky" than any of the individual companies.

Finally, produce a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock with its ticker symbol

```{r risk_return_plot}

 sd <- after_20170101 %>%
   group_by(symbol) %>%
  summarise(mean = mean(monthly_returns), SD = sd(monthly_returns))

ggplot(data = sd, mapping = aes(x = SD, y = mean, label = symbol)) +
  geom_point() +
  geom_text_repel()+
  labs(x = "Standard Deviation", y = "Mean", title = "Expected Monthly Return vs Risk")+
  NULL
```

What can you infer from this plot? Are there any stocks which, while being riskier, do not have a higher expected return?

> TYPE YOUR ANSWER AFTER (AND OUTSIDE!) THIS BLOCKQUOTE.
Yes there are, Boeing for example has a significantly higher risk (as measured by SD) vs its mean monthly return. Cisco for example has a similar return profile whilst exhibiting lower risk


