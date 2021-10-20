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
  filter(n >= 10000) %>%
  arrange(department) %>%
  ggplot(aes(x = aisle, y = n)) + geom_point()
  ggsave("aisle_scatter_plot.pdf", height = 4, width = 6)
```

Make a plot that shows the number of items ordered in each aisle Arrange
aisles sensibly, and organize your plot so others can read it.
