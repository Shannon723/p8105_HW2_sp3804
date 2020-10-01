HW2\_sp3804\_Shannon (Seonyoung Park)
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Problem 1

``` r
trashwheel_df = 
  read_xlsx(
    "./Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "Mr. Trash Wheel",
    range = cell_cols("A:N")
    ) %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>%
  mutate(
    sports_balls = round(sports_balls),
    sports_balls = as.integer(sports_balls)
  )
```

Read precipitation data\!

``` r
precip_2018 = 
  read_excel(
    "./Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2018 Precipitation",
    skip = 1
  ) %>%
  janitor::clean_names() %>%
  drop_na(month) %>%
  mutate(year = 2018) %>%
  relocate(year)

precip_2017 = 
  read_excel(
    "./Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2017 Precipitation",
    skip = 1
  ) %>%
  janitor::clean_names() %>%
  drop_na(month) %>%
  mutate(year = 2017) %>%
  relocate(year)
```

Now combine annual precipitation.

``` r
month_df = 
  tibble(
    month = 1:12,
    month_name = month.name
  )

precip_df = 
  bind_rows(precip_2018, precip_2017) 

left_join(precip_df, month_df, by = "month")
```

    ## # A tibble: 24 x 4
    ##     year month total month_name
    ##    <dbl> <dbl> <dbl> <chr>     
    ##  1  2018     1  0.94 January   
    ##  2  2018     2  4.8  February  
    ##  3  2018     3  2.69 March     
    ##  4  2018     4  4.69 April     
    ##  5  2018     5  9.27 May       
    ##  6  2018     6  4.77 June      
    ##  7  2018     7 10.2  July      
    ##  8  2018     8  6.45 August    
    ##  9  2018     9 10.5  September 
    ## 10  2018    10  2.12 October   
    ## # … with 14 more rows

This dataset contains information from the Mr. Trashwhell trash
collector in Baltimore, Maryland. As trash enters the innner harbor, the
trashwhell collects that trash, and stores it in a dumpster. The dataset
contains information on year, month, and trash collected, include some
specific kinds of trash. There are a total of 344 rows in our final
dataset. Additional data sheets include month precipitation data.

## Problem 2

### Data import and clean up

``` r
subway_df = read.csv("./NYC_Transit_Subway.csv") %>%
  janitor::clean_names()

subway_df = subway_df %>%
  select(line,station_name, station_latitude,station_longitude,
         route1, route2, route3, route4, route5, route6, route7,
         route8, route9, route10, route11,entry, entrance_type,
         vending, ada)

subway_df %>%
  group_by(entry)%>%
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   entry [2]
    ##   entry     n
    ##   <fct> <int>
    ## 1 NO      115
    ## 2 YES    1753

``` r
# there are two types; entry exist or not

subway_df$entry = as.logical(as.numeric(pull(subway_df, entry)))
glimpse(subway_df)
```

    ## Rows: 1,868
    ## Columns: 19
    ## $ line              <fct> 4 Avenue, 4 Avenue, 4 Avenue, 4 Avenue, 4 Avenue, 4…
    ## $ station_name      <fct> 25th St, 25th St, 36th St, 36th St, 36th St, 45th S…
    ## $ station_latitude  <dbl> 40.66040, 40.66040, 40.65514, 40.65514, 40.65514, 4…
    ## $ station_longitude <dbl> -73.99809, -73.99809, -74.00355, -74.00355, -74.003…
    ## $ route1            <fct> R, R, N, N, N, R, R, R, R, R, R, R, R, R, N, N, N, …
    ## $ route2            <fct> , , R, R, R, , , , , , , , , , R, R, R, R, R, R, , …
    ## $ route3            <fct> , , , , , , , , , , , , , , , , , , , , , , , , , 
    ## $ route4            <fct> , , , , , , , , , , , , , , , , , , , , , , , , , 
    ## $ route5            <fct> , , , , , , , , , , , , , , , , , , , , , , , , , 
    ## $ route6            <fct> , , , , , , , , , , , , , , , , , , , , , , , , , 
    ## $ route7            <fct> , , , , , , , , , , , , , , , , , , , , , , , , , 
    ## $ route8            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ route9            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ route10           <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ route11           <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ entry             <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    ## $ entrance_type     <fct> Stair, Stair, Stair, Stair, Stair, Stair, Stair, St…
    ## $ vending           <fct> YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, Y…
    ## $ ada               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

``` r
# entry variable has been changed to logical form 
```

This dataset is NYC Transit data, especially, it contains information
related to each entrance and exit for each subway station in NYC. In the
final dataset, the variables included are line, station name, station
latitude, station longitude, route, entry, entrance type, vending and
ADA compliance. To clean up the data, I firstly used janitor to clean
the name. Then, selected the variables of interest. Lastly, the format
of ‘entry’ variable has been changed to logical form. So far, the data
looks tidy.

### Answering sub questions

``` r
distinct(subway_df, station_name,line, .keep_all=FALSE) %>%
  count()
```

    ##     n
    ## 1 465

``` r
# There are 465 distinct stations.

subway_df %>%
  group_by(ada)%>%
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   ada [2]
    ##   ada       n
    ##   <lgl> <int>
    ## 1 FALSE  1400
    ## 2 TRUE    468

``` r
#There are 468 ADA compliant stations

subway_df %>%
  filter(vending=="NO") %>%
  group_by(entry) %>%
  count() %>%
  mutate(n/sum(n))
```

    ## # A tibble: 1 x 3
    ## # Groups:   entry [1]
    ##   entry     n `n/sum(n)`
    ##   <lgl> <int>      <dbl>
    ## 1 TRUE    183          1

Q: How many distinct stations are there? Note that stations are
identified both by name and by line (e.g. 125th St A/B/C/D; 125st 1;
125st 4/5); the distinct function may be useful here. A: There are 465
distinct stations.

Q: How many stations are ADA compliant? A: There are 468 ADA compliant
stations.

Q: What proportion of station entrances / exits without vending allow
entrance? A: 100% of stations without vending allow entrance

``` r
subway_df$route8 = as.factor(subway_df$route8)
subway_df$route9 = as.factor(subway_df$route9)
subway_df$route10 = as.factor(subway_df$route10)
subway_df$route11 = as.factor(subway_df$route11)

subway_tidy = subway_df %>%
pivot_longer(
    route1:route11,
    names_to = "route_number",
    values_to = "route_name")


subway_distinct = distinct(subway_tidy, station_name,line, .keep_all=TRUE) 

subway_distinct %>%
  group_by(route_name, ada) %>%
  count()
```

    ## # A tibble: 43 x 3
    ## # Groups:   route_name, ada [43]
    ##    route_name ada       n
    ##    <fct>      <lgl> <int>
    ##  1 1          FALSE    26
    ##  2 1          TRUE      6
    ##  3 2          FALSE    31
    ##  4 2          TRUE      7
    ##  5 3          FALSE    11
    ##  6 3          TRUE      1
    ##  7 4          FALSE    13
    ##  8 4          TRUE      3
    ##  9 5          FALSE     5
    ## 10 6          FALSE    28
    ## # … with 33 more rows

Q: Reformat data so that route number and route name are distinct
variables. How many distinct stations serve the A train? A: After
reformatting the data, there are 465 distinct stations. Among distinct
stations, 65 stations serve the A train.

Q: Of the stations that serve the A train, how many are ADA compliant?
A: Among 65 distinct stations serving the A train, 17 stations are ADA
compliant.

## Question 3.
