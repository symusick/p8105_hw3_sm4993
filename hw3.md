Data Science I: HW3
================
Sydney Musick

# Problem 1

### Load packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

### Load data

``` r
library(p8105.datasets)
data("instacart")
```

The `instacart` dataset has 1384617 observations (1384617 rows and 15
columns). It includes variables order\_id, product\_id,
add\_to\_cart\_order, reordered, user\_id, eval\_set, order\_number,
order\_dow, order\_hour\_of\_day, days\_since\_prior\_order,
product\_name, aisle\_id, department\_id, aisle, department. The first
observation in the dataset, for example, is product\_id 49302, Bulgarian
Yogurt, in the yogurt aisle (aisle\_id 120).

### How many aisles are there? Which is most ordered from?

``` r
max(instacart$aisle_id)
```

    ## [1] 134

``` r
instacart %>%
  count(aisle) %>%
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

There are 134 aisles. The most commonly ordered from aisles are fresh
vegetables, fresh fruits, and packaged vegetables fruits.

### Plot that shows the number of items ordered in each aisle

``` r
instacart_10000 = instacart %>%
  select(aisle, department) %>%
  count(aisle, department) %>% 
  arrange(desc(n)) %>%
  filter(n >= 10000)
ggplot(instacart_10000, aes(x = aisle, y = n)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw3_files/figure-gfm/items/aisle%20plot-1.png)<!-- -->

``` r
ggsave("aisle_scatter_plot.pdf", height = 4, width = 10)
```

Most aisles have less than 40,000 orders. The exceptions are fresh
vegetables, fresh fruits, packaged cheese, packaged vegetables fruits,
and yogurt. Fresh vegetables and fresh fruits are extreme outliers,
nearing 160,000 orders.

### Table showing the three most popular items in each of the aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”

``` r
instacart_aisles = instacart %>%
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>%
  count(product_name, aisle) %>% 
  arrange(desc(n)) %>%
  filter(product_name %in% c("Organic Baby Spinach", "Organic Raspberries", "Organic Blueberries", "Light Brown Sugar", "Pure Baking Soda", "Cane Sugar", "Snack Sticks Chicken & Rice Recipe Dog Treats", "Organix Chicken & Brown Rice Recipe", "Small Dog Biscuits")) 
summarise(instacart_aisles, aisle, product_name, n)
```

    ## # A tibble: 9 × 3
    ##   aisle                      product_name                                      n
    ##   <chr>                      <chr>                                         <int>
    ## 1 packaged vegetables fruits Organic Baby Spinach                           9784
    ## 2 packaged vegetables fruits Organic Raspberries                            5546
    ## 3 packaged vegetables fruits Organic Blueberries                            4966
    ## 4 baking ingredients         Light Brown Sugar                               499
    ## 5 baking ingredients         Pure Baking Soda                                387
    ## 6 baking ingredients         Cane Sugar                                      336
    ## 7 dog food care              Snack Sticks Chicken & Rice Recipe Dog Treats    30
    ## 8 dog food care              Organix Chicken & Brown Rice Recipe              28
    ## 9 dog food care              Small Dog Biscuits                               26

The most popular items in the packaged vegetables fruits aisle are
Organic Baby Spinach (9784 orders), Organic Raspberries (5546 orders),
and Organic Blueberries (4966 orders). The most popular items in the
baking ingredients aisle are Light Brown Sugar (499 orders), Pure Baking
Soda (387 orders), and Cane Sugar (336 orders). The most popular items
in the dog food care aisle are Snack Sticks Chicken & Rice Recipe Dog
Treats (30 orders), Organix Chicken & Brown Rice Recipe (28 orders), and
Small Dog Biscuits (26 orders).

### Mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered

``` r
instacart_mean = instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "0", "Sunday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "1", "Monday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "2", "Tuesday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "3", "Wednesday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "4", "Thursday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "5", "Friday")) %>%
  mutate(order_dow = replace(order_dow, order_dow == "6", "Saturday")) %>%
  select(order_dow, order_hour_of_day) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarize(n_obs = n()) %>%
  summarize(mean_hour = mean(order_hour_of_day))
```

    ## `summarise()` has grouped output by 'order_dow'. You can override using the `.groups` argument.

``` r
day <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

instacart_mean %>%
  mutate(order_dow = factor(order_dow, levels = day)) %>%
  arrange(order_dow) 
```

    ## # A tibble: 7 × 2
    ##   order_dow mean_hour
    ##   <fct>         <dbl>
    ## 1 Sunday         14  
    ## 2 Monday         13.8
    ## 3 Tuesday        13.6
    ## 4 Wednesday      15.5
    ## 5 Thursday       12.3
    ## 6 Friday         12.4
    ## 7 Saturday       13.3

The mean time of day to get Pink Lady Apples or Coffee Ice Cream is
around mid-day to early afternoon most day.

# Problem 2

### Load data

``` r
data("brfss_smart2010")
```

### Clean data

``` r
brfss = brfss_smart2010 %>%
  janitor::clean_names() %>%
  rename(state = locationabbr, state_county = locationdesc, general_topic = class, specific_topic = topic) %>%
  filter(specific_topic %in% c("Overall Health")) %>%
  filter(response %in% c("Excellent", "Very good", "Good", "Fair", "Poor")) %>%
  group_by(response) %>%
  mutate(ordered(response, levels = c("Excellent", "Very good", "Good", "Fair", "Poor")))
```

### Which states were observed at 7 or more locations in 2002?

``` r
brfss_2002 = brfss %>%
  filter(year == 2002) %>%
  select(state, geo_location) %>%
  group_by(state, geo_location) %>%
  summarize(n_obs = n())
```

    ## Adding missing grouping variables: `response`

    ## `summarise()` has grouped output by 'state'. You can override using the `.groups` argument.

In 2002, CT, FL, MA, NC, NJ, and PA were observed at 7 or more
locations.

### Which states were observed at 7 or more locations in 2010?

``` r
brfss_2010 = brfss %>%
  filter(year == 2010) %>%
  select(state, geo_location) %>%
  group_by(state, geo_location) %>%
  summarize(n_obs = n())
```

    ## Adding missing grouping variables: `response`

    ## `summarise()` has grouped output by 'state'. You can override using the `.groups` argument.

In 2010, CA, CO, FL, MA, MD, NC, NE, NJ, NY, OH, PA, SC, TX, and WA were
observed at 7 or more locations.

### Dataset and spaghetti plot construction

``` r
brfss_new = brfss %>%
  filter(response == "Excellent") %>%
  group_by(state, year) %>%
  mutate(data_value_avg = mean(data_value)) %>%
  select(year, state, data_value_avg)
ggplot(brfss_new, aes(x = year, y = data_value_avg, group = state, color = state)) + geom_line() + theme(legend.position = "right")
```

![](hw3_files/figure-gfm/new%20dataset-1.png)<!-- -->

``` r
ggsave("brfss_spaghetti_plot.pdf", height = 4, width = 10)
```

### Two pannel plot construction for 2006 and 2010

``` r
brfss_2panel = brfss %>%
  filter(year %in% c("2006", "2010")) %>%
  filter(state == "NY") %>%
  group_by(year, state_county)
ggplot(brfss_2panel, aes(x = response, y = data_value, group = state_county, color = state_county)) + geom_line() + facet_grid(. ~ year)
```

![](hw3_files/figure-gfm/two%20panel%20plot-1.png)<!-- -->

``` r
ggsave("brfss_2panel_plot.pdf", height = 4, width = 10)
```
