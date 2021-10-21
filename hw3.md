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

# Problem 3

``` r
accel =
  janitor::clean_names(
    read_csv("data_hw3/accel_data.csv", col_names = TRUE)) %>%
  mutate(weekday_weekend = day) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Sunday", "weekend")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Monday", "weekday")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Tuesday", "weekday")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Wednesday", "weekday")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Thursday", "weekday")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Friday", "weekday")) %>%
  mutate(weekday_weekend = replace(weekday_weekend, weekday_weekend == "Saturday", "weekend"))
```

    ## Rows: 35 Columns: 1443

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (1): day
    ## dbl (1442): week, day_id, activity.1, activity.2, activity.3, activity.4, ac...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The `accel` dataset has 35 observations. It includes variables week,
day\_id, day, activity\_1, activity\_2, activity\_3, activity\_4,
activity\_5, activity\_6, activity\_7, activity\_8, activity\_9,
activity\_10, activity\_11, activity\_12, activity\_13, activity\_14,
activity\_15, activity\_16, activity\_17, activity\_18, activity\_19,
activity\_20, activity\_21, activity\_22, activity\_23, activity\_24,
activity\_25, activity\_26, activity\_27, activity\_28, activity\_29,
activity\_30, activity\_31, activity\_32, activity\_33, activity\_34,
activity\_35, activity\_36, activity\_37, activity\_38, activity\_39,
activity\_40, activity\_41, activity\_42, activity\_43, activity\_44,
activity\_45, activity\_46, activity\_47, activity\_48, activity\_49,
activity\_50, activity\_51, activity\_52, activity\_53, activity\_54,
activity\_55, activity\_56, activity\_57, activity\_58, activity\_59,
activity\_60, activity\_61, activity\_62, activity\_63, activity\_64,
activity\_65, activity\_66, activity\_67, activity\_68, activity\_69,
activity\_70, activity\_71, activity\_72, activity\_73, activity\_74,
activity\_75, activity\_76, activity\_77, activity\_78, activity\_79,
activity\_80, activity\_81, activity\_82, activity\_83, activity\_84,
activity\_85, activity\_86, activity\_87, activity\_88, activity\_89,
activity\_90, activity\_91, activity\_92, activity\_93, activity\_94,
activity\_95, activity\_96, activity\_97, activity\_98, activity\_99,
activity\_100, activity\_101, activity\_102, activity\_103,
activity\_104, activity\_105, activity\_106, activity\_107,
activity\_108, activity\_109, activity\_110, activity\_111,
activity\_112, activity\_113, activity\_114, activity\_115,
activity\_116, activity\_117, activity\_118, activity\_119,
activity\_120, activity\_121, activity\_122, activity\_123,
activity\_124, activity\_125, activity\_126, activity\_127,
activity\_128, activity\_129, activity\_130, activity\_131,
activity\_132, activity\_133, activity\_134, activity\_135,
activity\_136, activity\_137, activity\_138, activity\_139,
activity\_140, activity\_141, activity\_142, activity\_143,
activity\_144, activity\_145, activity\_146, activity\_147,
activity\_148, activity\_149, activity\_150, activity\_151,
activity\_152, activity\_153, activity\_154, activity\_155,
activity\_156, activity\_157, activity\_158, activity\_159,
activity\_160, activity\_161, activity\_162, activity\_163,
activity\_164, activity\_165, activity\_166, activity\_167,
activity\_168, activity\_169, activity\_170, activity\_171,
activity\_172, activity\_173, activity\_174, activity\_175,
activity\_176, activity\_177, activity\_178, activity\_179,
activity\_180, activity\_181, activity\_182, activity\_183,
activity\_184, activity\_185, activity\_186, activity\_187,
activity\_188, activity\_189, activity\_190, activity\_191,
activity\_192, activity\_193, activity\_194, activity\_195,
activity\_196, activity\_197, activity\_198, activity\_199,
activity\_200, activity\_201, activity\_202, activity\_203,
activity\_204, activity\_205, activity\_206, activity\_207,
activity\_208, activity\_209, activity\_210, activity\_211,
activity\_212, activity\_213, activity\_214, activity\_215,
activity\_216, activity\_217, activity\_218, activity\_219,
activity\_220, activity\_221, activity\_222, activity\_223,
activity\_224, activity\_225, activity\_226, activity\_227,
activity\_228, activity\_229, activity\_230, activity\_231,
activity\_232, activity\_233, activity\_234, activity\_235,
activity\_236, activity\_237, activity\_238, activity\_239,
activity\_240, activity\_241, activity\_242, activity\_243,
activity\_244, activity\_245, activity\_246, activity\_247,
activity\_248, activity\_249, activity\_250, activity\_251,
activity\_252, activity\_253, activity\_254, activity\_255,
activity\_256, activity\_257, activity\_258, activity\_259,
activity\_260, activity\_261, activity\_262, activity\_263,
activity\_264, activity\_265, activity\_266, activity\_267,
activity\_268, activity\_269, activity\_270, activity\_271,
activity\_272, activity\_273, activity\_274, activity\_275,
activity\_276, activity\_277, activity\_278, activity\_279,
activity\_280, activity\_281, activity\_282, activity\_283,
activity\_284, activity\_285, activity\_286, activity\_287,
activity\_288, activity\_289, activity\_290, activity\_291,
activity\_292, activity\_293, activity\_294, activity\_295,
activity\_296, activity\_297, activity\_298, activity\_299,
activity\_300, activity\_301, activity\_302, activity\_303,
activity\_304, activity\_305, activity\_306, activity\_307,
activity\_308, activity\_309, activity\_310, activity\_311,
activity\_312, activity\_313, activity\_314, activity\_315,
activity\_316, activity\_317, activity\_318, activity\_319,
activity\_320, activity\_321, activity\_322, activity\_323,
activity\_324, activity\_325, activity\_326, activity\_327,
activity\_328, activity\_329, activity\_330, activity\_331,
activity\_332, activity\_333, activity\_334, activity\_335,
activity\_336, activity\_337, activity\_338, activity\_339,
activity\_340, activity\_341, activity\_342, activity\_343,
activity\_344, activity\_345, activity\_346, activity\_347,
activity\_348, activity\_349, activity\_350, activity\_351,
activity\_352, activity\_353, activity\_354, activity\_355,
activity\_356, activity\_357, activity\_358, activity\_359,
activity\_360, activity\_361, activity\_362, activity\_363,
activity\_364, activity\_365, activity\_366, activity\_367,
activity\_368, activity\_369, activity\_370, activity\_371,
activity\_372, activity\_373, activity\_374, activity\_375,
activity\_376, activity\_377, activity\_378, activity\_379,
activity\_380, activity\_381, activity\_382, activity\_383,
activity\_384, activity\_385, activity\_386, activity\_387,
activity\_388, activity\_389, activity\_390, activity\_391,
activity\_392, activity\_393, activity\_394, activity\_395,
activity\_396, activity\_397, activity\_398, activity\_399,
activity\_400, activity\_401, activity\_402, activity\_403,
activity\_404, activity\_405, activity\_406, activity\_407,
activity\_408, activity\_409, activity\_410, activity\_411,
activity\_412, activity\_413, activity\_414, activity\_415,
activity\_416, activity\_417, activity\_418, activity\_419,
activity\_420, activity\_421, activity\_422, activity\_423,
activity\_424, activity\_425, activity\_426, activity\_427,
activity\_428, activity\_429, activity\_430, activity\_431,
activity\_432, activity\_433, activity\_434, activity\_435,
activity\_436, activity\_437, activity\_438, activity\_439,
activity\_440, activity\_441, activity\_442, activity\_443,
activity\_444, activity\_445, activity\_446, activity\_447,
activity\_448, activity\_449, activity\_450, activity\_451,
activity\_452, activity\_453, activity\_454, activity\_455,
activity\_456, activity\_457, activity\_458, activity\_459,
activity\_460, activity\_461, activity\_462, activity\_463,
activity\_464, activity\_465, activity\_466, activity\_467,
activity\_468, activity\_469, activity\_470, activity\_471,
activity\_472, activity\_473, activity\_474, activity\_475,
activity\_476, activity\_477, activity\_478, activity\_479,
activity\_480, activity\_481, activity\_482, activity\_483,
activity\_484, activity\_485, activity\_486, activity\_487,
activity\_488, activity\_489, activity\_490, activity\_491,
activity\_492, activity\_493, activity\_494, activity\_495,
activity\_496, activity\_497, activity\_498, activity\_499,
activity\_500, activity\_501, activity\_502, activity\_503,
activity\_504, activity\_505, activity\_506, activity\_507,
activity\_508, activity\_509, activity\_510, activity\_511,
activity\_512, activity\_513, activity\_514, activity\_515,
activity\_516, activity\_517, activity\_518, activity\_519,
activity\_520, activity\_521, activity\_522, activity\_523,
activity\_524, activity\_525, activity\_526, activity\_527,
activity\_528, activity\_529, activity\_530, activity\_531,
activity\_532, activity\_533, activity\_534, activity\_535,
activity\_536, activity\_537, activity\_538, activity\_539,
activity\_540, activity\_541, activity\_542, activity\_543,
activity\_544, activity\_545, activity\_546, activity\_547,
activity\_548, activity\_549, activity\_550, activity\_551,
activity\_552, activity\_553, activity\_554, activity\_555,
activity\_556, activity\_557, activity\_558, activity\_559,
activity\_560, activity\_561, activity\_562, activity\_563,
activity\_564, activity\_565, activity\_566, activity\_567,
activity\_568, activity\_569, activity\_570, activity\_571,
activity\_572, activity\_573, activity\_574, activity\_575,
activity\_576, activity\_577, activity\_578, activity\_579,
activity\_580, activity\_581, activity\_582, activity\_583,
activity\_584, activity\_585, activity\_586, activity\_587,
activity\_588, activity\_589, activity\_590, activity\_591,
activity\_592, activity\_593, activity\_594, activity\_595,
activity\_596, activity\_597, activity\_598, activity\_599,
activity\_600, activity\_601, activity\_602, activity\_603,
activity\_604, activity\_605, activity\_606, activity\_607,
activity\_608, activity\_609, activity\_610, activity\_611,
activity\_612, activity\_613, activity\_614, activity\_615,
activity\_616, activity\_617, activity\_618, activity\_619,
activity\_620, activity\_621, activity\_622, activity\_623,
activity\_624, activity\_625, activity\_626, activity\_627,
activity\_628, activity\_629, activity\_630, activity\_631,
activity\_632, activity\_633, activity\_634, activity\_635,
activity\_636, activity\_637, activity\_638, activity\_639,
activity\_640, activity\_641, activity\_642, activity\_643,
activity\_644, activity\_645, activity\_646, activity\_647,
activity\_648, activity\_649, activity\_650, activity\_651,
activity\_652, activity\_653, activity\_654, activity\_655,
activity\_656, activity\_657, activity\_658, activity\_659,
activity\_660, activity\_661, activity\_662, activity\_663,
activity\_664, activity\_665, activity\_666, activity\_667,
activity\_668, activity\_669, activity\_670, activity\_671,
activity\_672, activity\_673, activity\_674, activity\_675,
activity\_676, activity\_677, activity\_678, activity\_679,
activity\_680, activity\_681, activity\_682, activity\_683,
activity\_684, activity\_685, activity\_686, activity\_687,
activity\_688, activity\_689, activity\_690, activity\_691,
activity\_692, activity\_693, activity\_694, activity\_695,
activity\_696, activity\_697, activity\_698, activity\_699,
activity\_700, activity\_701, activity\_702, activity\_703,
activity\_704, activity\_705, activity\_706, activity\_707,
activity\_708, activity\_709, activity\_710, activity\_711,
activity\_712, activity\_713, activity\_714, activity\_715,
activity\_716, activity\_717, activity\_718, activity\_719,
activity\_720, activity\_721, activity\_722, activity\_723,
activity\_724, activity\_725, activity\_726, activity\_727,
activity\_728, activity\_729, activity\_730, activity\_731,
activity\_732, activity\_733, activity\_734, activity\_735,
activity\_736, activity\_737, activity\_738, activity\_739,
activity\_740, activity\_741, activity\_742, activity\_743,
activity\_744, activity\_745, activity\_746, activity\_747,
activity\_748, activity\_749, activity\_750, activity\_751,
activity\_752, activity\_753, activity\_754, activity\_755,
activity\_756, activity\_757, activity\_758, activity\_759,
activity\_760, activity\_761, activity\_762, activity\_763,
activity\_764, activity\_765, activity\_766, activity\_767,
activity\_768, activity\_769, activity\_770, activity\_771,
activity\_772, activity\_773, activity\_774, activity\_775,
activity\_776, activity\_777, activity\_778, activity\_779,
activity\_780, activity\_781, activity\_782, activity\_783,
activity\_784, activity\_785, activity\_786, activity\_787,
activity\_788, activity\_789, activity\_790, activity\_791,
activity\_792, activity\_793, activity\_794, activity\_795,
activity\_796, activity\_797, activity\_798, activity\_799,
activity\_800, activity\_801, activity\_802, activity\_803,
activity\_804, activity\_805, activity\_806, activity\_807,
activity\_808, activity\_809, activity\_810, activity\_811,
activity\_812, activity\_813, activity\_814, activity\_815,
activity\_816, activity\_817, activity\_818, activity\_819,
activity\_820, activity\_821, activity\_822, activity\_823,
activity\_824, activity\_825, activity\_826, activity\_827,
activity\_828, activity\_829, activity\_830, activity\_831,
activity\_832, activity\_833, activity\_834, activity\_835,
activity\_836, activity\_837, activity\_838, activity\_839,
activity\_840, activity\_841, activity\_842, activity\_843,
activity\_844, activity\_845, activity\_846, activity\_847,
activity\_848, activity\_849, activity\_850, activity\_851,
activity\_852, activity\_853, activity\_854, activity\_855,
activity\_856, activity\_857, activity\_858, activity\_859,
activity\_860, activity\_861, activity\_862, activity\_863,
activity\_864, activity\_865, activity\_866, activity\_867,
activity\_868, activity\_869, activity\_870, activity\_871,
activity\_872, activity\_873, activity\_874, activity\_875,
activity\_876, activity\_877, activity\_878, activity\_879,
activity\_880, activity\_881, activity\_882, activity\_883,
activity\_884, activity\_885, activity\_886, activity\_887,
activity\_888, activity\_889, activity\_890, activity\_891,
activity\_892, activity\_893, activity\_894, activity\_895,
activity\_896, activity\_897, activity\_898, activity\_899,
activity\_900, activity\_901, activity\_902, activity\_903,
activity\_904, activity\_905, activity\_906, activity\_907,
activity\_908, activity\_909, activity\_910, activity\_911,
activity\_912, activity\_913, activity\_914, activity\_915,
activity\_916, activity\_917, activity\_918, activity\_919,
activity\_920, activity\_921, activity\_922, activity\_923,
activity\_924, activity\_925, activity\_926, activity\_927,
activity\_928, activity\_929, activity\_930, activity\_931,
activity\_932, activity\_933, activity\_934, activity\_935,
activity\_936, activity\_937, activity\_938, activity\_939,
activity\_940, activity\_941, activity\_942, activity\_943,
activity\_944, activity\_945, activity\_946, activity\_947,
activity\_948, activity\_949, activity\_950, activity\_951,
activity\_952, activity\_953, activity\_954, activity\_955,
activity\_956, activity\_957, activity\_958, activity\_959,
activity\_960, activity\_961, activity\_962, activity\_963,
activity\_964, activity\_965, activity\_966, activity\_967,
activity\_968, activity\_969, activity\_970, activity\_971,
activity\_972, activity\_973, activity\_974, activity\_975,
activity\_976, activity\_977, activity\_978, activity\_979,
activity\_980, activity\_981, activity\_982, activity\_983,
activity\_984, activity\_985, activity\_986, activity\_987,
activity\_988, activity\_989, activity\_990, activity\_991,
activity\_992, activity\_993, activity\_994, activity\_995,
activity\_996, activity\_997, activity\_998, activity\_999,
activity\_1000, activity\_1001, activity\_1002, activity\_1003,
activity\_1004, activity\_1005, activity\_1006, activity\_1007,
activity\_1008, activity\_1009, activity\_1010, activity\_1011,
activity\_1012, activity\_1013, activity\_1014, activity\_1015,
activity\_1016, activity\_1017, activity\_1018, activity\_1019,
activity\_1020, activity\_1021, activity\_1022, activity\_1023,
activity\_1024, activity\_1025, activity\_1026, activity\_1027,
activity\_1028, activity\_1029, activity\_1030, activity\_1031,
activity\_1032, activity\_1033, activity\_1034, activity\_1035,
activity\_1036, activity\_1037, activity\_1038, activity\_1039,
activity\_1040, activity\_1041, activity\_1042, activity\_1043,
activity\_1044, activity\_1045, activity\_1046, activity\_1047,
activity\_1048, activity\_1049, activity\_1050, activity\_1051,
activity\_1052, activity\_1053, activity\_1054, activity\_1055,
activity\_1056, activity\_1057, activity\_1058, activity\_1059,
activity\_1060, activity\_1061, activity\_1062, activity\_1063,
activity\_1064, activity\_1065, activity\_1066, activity\_1067,
activity\_1068, activity\_1069, activity\_1070, activity\_1071,
activity\_1072, activity\_1073, activity\_1074, activity\_1075,
activity\_1076, activity\_1077, activity\_1078, activity\_1079,
activity\_1080, activity\_1081, activity\_1082, activity\_1083,
activity\_1084, activity\_1085, activity\_1086, activity\_1087,
activity\_1088, activity\_1089, activity\_1090, activity\_1091,
activity\_1092, activity\_1093, activity\_1094, activity\_1095,
activity\_1096, activity\_1097, activity\_1098, activity\_1099,
activity\_1100, activity\_1101, activity\_1102, activity\_1103,
activity\_1104, activity\_1105, activity\_1106, activity\_1107,
activity\_1108, activity\_1109, activity\_1110, activity\_1111,
activity\_1112, activity\_1113, activity\_1114, activity\_1115,
activity\_1116, activity\_1117, activity\_1118, activity\_1119,
activity\_1120, activity\_1121, activity\_1122, activity\_1123,
activity\_1124, activity\_1125, activity\_1126, activity\_1127,
activity\_1128, activity\_1129, activity\_1130, activity\_1131,
activity\_1132, activity\_1133, activity\_1134, activity\_1135,
activity\_1136, activity\_1137, activity\_1138, activity\_1139,
activity\_1140, activity\_1141, activity\_1142, activity\_1143,
activity\_1144, activity\_1145, activity\_1146, activity\_1147,
activity\_1148, activity\_1149, activity\_1150, activity\_1151,
activity\_1152, activity\_1153, activity\_1154, activity\_1155,
activity\_1156, activity\_1157, activity\_1158, activity\_1159,
activity\_1160, activity\_1161, activity\_1162, activity\_1163,
activity\_1164, activity\_1165, activity\_1166, activity\_1167,
activity\_1168, activity\_1169, activity\_1170, activity\_1171,
activity\_1172, activity\_1173, activity\_1174, activity\_1175,
activity\_1176, activity\_1177, activity\_1178, activity\_1179,
activity\_1180, activity\_1181, activity\_1182, activity\_1183,
activity\_1184, activity\_1185, activity\_1186, activity\_1187,
activity\_1188, activity\_1189, activity\_1190, activity\_1191,
activity\_1192, activity\_1193, activity\_1194, activity\_1195,
activity\_1196, activity\_1197, activity\_1198, activity\_1199,
activity\_1200, activity\_1201, activity\_1202, activity\_1203,
activity\_1204, activity\_1205, activity\_1206, activity\_1207,
activity\_1208, activity\_1209, activity\_1210, activity\_1211,
activity\_1212, activity\_1213, activity\_1214, activity\_1215,
activity\_1216, activity\_1217, activity\_1218, activity\_1219,
activity\_1220, activity\_1221, activity\_1222, activity\_1223,
activity\_1224, activity\_1225, activity\_1226, activity\_1227,
activity\_1228, activity\_1229, activity\_1230, activity\_1231,
activity\_1232, activity\_1233, activity\_1234, activity\_1235,
activity\_1236, activity\_1237, activity\_1238, activity\_1239,
activity\_1240, activity\_1241, activity\_1242, activity\_1243,
activity\_1244, activity\_1245, activity\_1246, activity\_1247,
activity\_1248, activity\_1249, activity\_1250, activity\_1251,
activity\_1252, activity\_1253, activity\_1254, activity\_1255,
activity\_1256, activity\_1257, activity\_1258, activity\_1259,
activity\_1260, activity\_1261, activity\_1262, activity\_1263,
activity\_1264, activity\_1265, activity\_1266, activity\_1267,
activity\_1268, activity\_1269, activity\_1270, activity\_1271,
activity\_1272, activity\_1273, activity\_1274, activity\_1275,
activity\_1276, activity\_1277, activity\_1278, activity\_1279,
activity\_1280, activity\_1281, activity\_1282, activity\_1283,
activity\_1284, activity\_1285, activity\_1286, activity\_1287,
activity\_1288, activity\_1289, activity\_1290, activity\_1291,
activity\_1292, activity\_1293, activity\_1294, activity\_1295,
activity\_1296, activity\_1297, activity\_1298, activity\_1299,
activity\_1300, activity\_1301, activity\_1302, activity\_1303,
activity\_1304, activity\_1305, activity\_1306, activity\_1307,
activity\_1308, activity\_1309, activity\_1310, activity\_1311,
activity\_1312, activity\_1313, activity\_1314, activity\_1315,
activity\_1316, activity\_1317, activity\_1318, activity\_1319,
activity\_1320, activity\_1321, activity\_1322, activity\_1323,
activity\_1324, activity\_1325, activity\_1326, activity\_1327,
activity\_1328, activity\_1329, activity\_1330, activity\_1331,
activity\_1332, activity\_1333, activity\_1334, activity\_1335,
activity\_1336, activity\_1337, activity\_1338, activity\_1339,
activity\_1340, activity\_1341, activity\_1342, activity\_1343,
activity\_1344, activity\_1345, activity\_1346, activity\_1347,
activity\_1348, activity\_1349, activity\_1350, activity\_1351,
activity\_1352, activity\_1353, activity\_1354, activity\_1355,
activity\_1356, activity\_1357, activity\_1358, activity\_1359,
activity\_1360, activity\_1361, activity\_1362, activity\_1363,
activity\_1364, activity\_1365, activity\_1366, activity\_1367,
activity\_1368, activity\_1369, activity\_1370, activity\_1371,
activity\_1372, activity\_1373, activity\_1374, activity\_1375,
activity\_1376, activity\_1377, activity\_1378, activity\_1379,
activity\_1380, activity\_1381, activity\_1382, activity\_1383,
activity\_1384, activity\_1385, activity\_1386, activity\_1387,
activity\_1388, activity\_1389, activity\_1390, activity\_1391,
activity\_1392, activity\_1393, activity\_1394, activity\_1395,
activity\_1396, activity\_1397, activity\_1398, activity\_1399,
activity\_1400, activity\_1401, activity\_1402, activity\_1403,
activity\_1404, activity\_1405, activity\_1406, activity\_1407,
activity\_1408, activity\_1409, activity\_1410, activity\_1411,
activity\_1412, activity\_1413, activity\_1414, activity\_1415,
activity\_1416, activity\_1417, activity\_1418, activity\_1419,
activity\_1420, activity\_1421, activity\_1422, activity\_1423,
activity\_1424, activity\_1425, activity\_1426, activity\_1427,
activity\_1428, activity\_1429, activity\_1430, activity\_1431,
activity\_1432, activity\_1433, activity\_1434, activity\_1435,
activity\_1436, activity\_1437, activity\_1438, activity\_1439,
activity\_1440, weekday\_weekend. It is five weeks of accelerometer data
collected on a 63 year-old male with BMI 25, who was admitted to the
Advanced Cardiac Care Center of Columbia University Medical Center and
diagnosed with congestive heart failure (CHF).

Traditional analyses of accelerometer data focus on the total activity
over the day. Using your tidied dataset, aggregate accross minutes to
create a total activity variable for each day, and create a table
showing these totals. Are any trends apparent?

Accelerometer data allows the inspection activity over the course of the
day. Make a single-panel plot that shows the 24-hour activity time
courses for each day and use color to indicate day of the week. Describe
in words any patterns or conclusions you can make based on this graph.
