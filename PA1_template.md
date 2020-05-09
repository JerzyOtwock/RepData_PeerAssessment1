---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

 
 ```r
 pacman::p_load(tidyverse,lubridate)
 
 df <- readr::read_csv("activity.csv")
 ```
 
 ```
 ## Parsed with column specification:
 ## cols(
 ##   steps = col_double(),
 ##   date = col_date(format = ""),
 ##   interval = col_double()
 ## )
 ```

## What is mean total number of steps taken per day?
 
 ```r
 step_by_day <- df %>% 
    group_by(date) %>% 
    summarise( sum_day_step = sum(steps, na.rm = F))
 mean(step_by_day$sum_day_step, na.rm = T)
 ```
 
 ```
 ## [1] 10766.19
 ```
 
 ```r
 options(scipen = 10)
 ggplot(step_by_day, aes(x = sum_day_step)) +
    geom_histogram(fill = "cyan", col = "dark blue", binwidth = 2000) +  # kolor wykresu i liczba słupków
    ggtitle("Number of steps") +  # tytuł
    labs(x = "Steps", y = "# days")   
 ```
 
 ![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
 
 ```r
 mean(step_by_day$sum_day_step, na.rm = T)
 ```
 
 ```
 ## [1] 10766.19
 ```
 
 ```r
 median(step_by_day$sum_day_step, na.rm = T)
 ```
 
 ```
 ## [1] 10765
 ```

## What is the average daily activity pattern?
 
 ```r
 df1 <- df %>% 
    group_by(interval) %>% 
    summarise( interval_mean = mean(steps, na.rm = T))
 
 ggplot(df1, aes(x = interval, y = interval_mean)) +
    geom_line()+
    theme_minimal() 
 ```
 
 ![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
 
 ```r
 df1 %>% filter(interval_mean == max(interval_mean))
 ```
 
 ```
 ## # A tibble: 1 x 2
 ##   interval interval_mean
 ##      <dbl>         <dbl>
 ## 1      835          206.
 ```

## Imputing missing values
 
 ```r
 df4 <- df %>% filter(is.na(steps))  
 df4 <- left_join(df4, df1, by = "interval" )
 df4 <- df4 %>% mutate(steps = interval_mean) %>% 
    select(-interval_mean)
 df5 <- df %>% filter(!is.na(steps))
 df5 <- rbind(df4, df5)
 
 df5 <- df5 %>% 
    group_by(date) %>% 
    mutate( sum_day_step = sum(steps, na.rm = F))
 
 options(scipen = 10)
 ggplot(df5, aes(x = sum_day_step)) +
    geom_histogram(fill = "cyan", col = "dark blue", binwidth = 2000) +  # kolor wykresu i liczba słupków
    ggtitle("Number of steps") +  # tytuł
    labs(x = "Steps", y = "# days")   
 ```
 
 ![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
 
 ```r
 mean(df5$sum_day_step, na.rm = T)
 ```
 
 ```
 ## [1] 10766.19
 ```
 
 ```r
 median(df5$sum_day_step, na.rm = T)
 ```
 
 ```
 ## [1] 10766.19
 ```
We filled up missing data using average number of steps for in given interval.
Both mean and median went up.


## Are there differences in activity patterns between weekdays and weekends?
 
 ```r
 df6 <- df5 %>% 
        mutate( weekend = wday(date) %in% c(1, 7) ) %>%
        mutate(weekend = factor(weekend, levels = c("FALSE", "TRUE"), labels = c("Weekday" , "Weekend" ))) %>% 
        group_by(weekend , interval) %>%
        summarise( mean_day_step = mean(steps, na.rm = F))
 
 
    ggplot(df6)+
    geom_line( aes(x = interval , y = mean_day_step))+
    theme_minimal() + facet_wrap(~ weekend , nrow = 2)
 ```
 
 ![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
