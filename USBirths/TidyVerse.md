---
title: "Tidyverse - US Births"
output:
  html_document:
    keep_md: yes
    self_contained: no
  pdf_document: default
---

#### Author 

- Joby John


#### Summary

In this  exercise, I am using US_births_2000-2014_SSA.csv dataset. I am using the dplyr functions to sort, group, mutate and summarize data.  

#### Load Libraries

```r

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

options(knitr.table.format = "html")
```




Following steps are done : 

- Read file into a dataframe

- Used dplyr function create summary and display in a kable

- USed dplyr to sort, group and mutate data

- Ploted number of births using ggplot




```r
data_url <-'https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv'
births_data <- read.csv(data_url)

births_data_summary_by_year <-    births_data %>%
                                  group_by( year )  %>% 
                                  group_by(year, total_births = sum(births)) %>%
                                  distinct(year,total_births)%>%
                                  select(year, total_births)%>%
                                  arrange(desc(total_births))
```

#### Number of births per year


```r
births_data_summary_by_year %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))  
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:right;"> total_births </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:right;"> 4380784 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:right;"> 4335154 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:right;"> 4310737 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:right;"> 4211941 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:right;"> 4190991 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:right;"> 4186863 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:right;"> 4163060 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2000 </td>
   <td style="text-align:right;"> 4149598 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 4110963 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> 4099313 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:right;"> 4055975 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 4010532 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:right;"> 4006908 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:right;"> 4000868 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:right;"> 3973337 </td>
  </tr>
</tbody>
</table>

```r

#Births peaked in 2007. Any correlation to great recession?

ggplot(births_data_summary_by_year, aes(x= year, y= total_births)) + geom_line()
```

<img src="README_figs/README-unnamed-chunk-2-1.png" width="672" />

#### Number of births per month

```r
births_data_summary_by_month <-   births_data %>%
                                  mutate(month = month.abb[month])%>%
                                  group_by( month )  %>% 
                                  group_by(month, total_births = sum(births)) %>%
                                  distinct(month,total_births)%>%
                                  select(month, total_births)%>%
                                  arrange(desc(total_births))

births_data_summary_by_month %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))  
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> month </th>
   <th style="text-align:right;"> total_births </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Aug </td>
   <td style="text-align:right;"> 5540170 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jul </td>
   <td style="text-align:right;"> 5450418 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 5399592 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oct </td>
   <td style="text-align:right;"> 5302865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> May </td>
   <td style="text-align:right;"> 5195445 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dec </td>
   <td style="text-align:right;"> 5194432 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mar </td>
   <td style="text-align:right;"> 5172961 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jun </td>
   <td style="text-align:right;"> 5163360 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jan </td>
   <td style="text-align:right;"> 5072588 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nov </td>
   <td style="text-align:right;"> 5008750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Apr </td>
   <td style="text-align:right;"> 4960750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Feb </td>
   <td style="text-align:right;"> 4725693 </td>
  </tr>
</tbody>
</table>

#### Top 10 days 

```r
top_10_birth_days<- births_data %>% 
            top_n(10, births) %>%
            select(year, month,date_of_month,births)%>%
            mutate(month = month.abb[month])%>%
            arrange(desc(births))

top_10_birth_days %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))  
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> month </th>
   <th style="text-align:right;"> date_of_month </th>
   <th style="text-align:right;"> births </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 16081 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Dec </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 15645 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Dec </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 15590 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Dec </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 15555 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 15454 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 15440 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 15391 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:left;"> Aug </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 15374 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:left;"> Dec </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 15214 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:left;"> Sep </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 15205 </td>
  </tr>
</tbody>
</table>

#### Births per day

```r
births_data_summary_by_day_of_week <-    births_data %>%
                                  group_by(day_of_week )  %>% 
                                  group_by(day_of_week, total_births = sum(births)) %>%
                                  distinct(day_of_week,total_births)%>%
                                  select(day_of_week, total_births) %>%
                                  arrange(desc(total_births))
births_data_summary_by_day_of_week %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))  
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> day_of_week </th>
   <th style="text-align:right;"> total_births </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 10274874 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 10109130 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 10045436 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 9850199 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 9316001 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6704495 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 5886889 </td>
  </tr>
</tbody>
</table>

#### Improvements

- Are Super Bowl Babies A Real Thing? We could Use this dataset to see if that claim is valid or not. 

- Convert numerical day of the week to actual day of the week. For example, 1 to sunday.

- Further analysis could be done. For example, how does economy affects the number of births.


#### References

 https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv

 https://www.bustle.com/articles/140278-are-super-bowl-babies-a-real-thing-one-commercial-makes-an-interesting-claim-video
 
