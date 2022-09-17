Predicting Car Prices in the US
================
Addison
2022-09-12

### Introduction

This project aims to build a model to predict car prices in the US based
on a dataset that contains 204 rows, each of which denotes
characteristics of a car and other information such as price and
normalized losses in use as compared to other cars. The dataset could be
obtained through this link:
<https://drive.google.com/file/d/12ZuW16SeYDlOwtdrjN5QrswbFvQ15OPW/view?usp=sharing>.

Here are the descriptions for all the columns:

**1. symboling:** the degree of risk. The smaller the number is, the
riskier the car is

**2. normalized-losses:** normalized losses in use as compared to other
cars

**3. make:** the name of the manufacturer

**4. fuel-type:** the type of fuel the car uses

**5. aspiration:** the type of the naturally aspirated engine of the car

**6. num-of-doors:** the number of doors of the car

**7. body-style:** the style of the car

**8. drive-wheels:** the type of drivetrain

**9. engine-location:** the location of the engine

**10. wheel-base:** the distance between the centre of the front wheels
and the centre of the rear wheels

**11. length:** the length of the car

**12. width:** the width of the car

**13. height:** the height of the car

**14. curb-weight:** the weight of the vehicle including a full tank of
fuel and all standard equipment

**15. engine-type:** the type of the engine of the car

**16. num-of-cylinders:** the number of cylinders

**17. engine-size:** the size of the engine of the car

**18. fuel-system:** the fuel system of the car

**19. bore:** the inner diameter of the cylinder of the car

**20. stroke:** the distance travelled by the piston during each cycle

**21. compression-ratio:** the ratio of the maximum to minimum volume in
the cylinder of the internal combustion engine

**22. horsepower:** the degree of power the engine of the car produces

**23. peak-rpm:** peak revolutions per minute, the highest speed of the
engine when operating

**24. city-mpg:** the average miles-per-gallon for the car in a city

**25. highway-mpg:** the average miles-per-gallon for the car on the
highway

**26. price:** the price of the car

``` r
library(tidyverse)
library(lattice)
library(caret)
library(fastDummies)


cars <- read.csv("/Users/apple/Downloads/R dataquest/imports-85.data")

colnames(cars) <- c(
  "symboling",
  "normalized_losses",
  "make",
  "fuel_type",
  "aspiration",
  "num_doors",
  "body_style",
  "drive_wheels",
  "engine_location",
  "wheel_base",
  "length",
  "width",
  "height",
  "curb_weight",
  "engine_type",
  "num_cylinders",
  "engine_size",
  "fuel_system",
  "bore",
  "stroke",
  "compression_ratio",
  "horsepower",
  "peak_rpm",
  "city_mpg",
  "highway_mpg",
  "price"
)
```

The names of the columns have been added to the data according to the
dataset information published on
<https://archive.ics.uci.edu/ml/datasets/automobile>.

Now, to start with, let’s have a quick look at the first few rows as
well as essential information of the data to imagine what the data looks
like as well as get to know the data types of columns and the number of
non-null values.

``` r
head(cars)
```

    ##   symboling normalized_losses        make fuel_type aspiration num_doors
    ## 1         3                 ? alfa-romero       gas        std       two
    ## 2         1                 ? alfa-romero       gas        std       two
    ## 3         2               164        audi       gas        std      four
    ## 4         2               164        audi       gas        std      four
    ## 5         2                 ?        audi       gas        std       two
    ## 6         1               158        audi       gas        std      four
    ##    body_style drive_wheels engine_location wheel_base length width height
    ## 1 convertible          rwd           front       88.6  168.8  64.1   48.8
    ## 2   hatchback          rwd           front       94.5  171.2  65.5   52.4
    ## 3       sedan          fwd           front       99.8  176.6  66.2   54.3
    ## 4       sedan          4wd           front       99.4  176.6  66.4   54.3
    ## 5       sedan          fwd           front       99.8  177.3  66.3   53.1
    ## 6       sedan          fwd           front      105.8  192.7  71.4   55.7
    ##   curb_weight engine_type num_cylinders engine_size fuel_system bore stroke
    ## 1        2548        dohc          four         130        mpfi 3.47   2.68
    ## 2        2823        ohcv           six         152        mpfi 2.68   3.47
    ## 3        2337         ohc          four         109        mpfi 3.19   3.40
    ## 4        2824         ohc          five         136        mpfi 3.19   3.40
    ## 5        2507         ohc          five         136        mpfi 3.19   3.40
    ## 6        2844         ohc          five         136        mpfi 3.19   3.40
    ##   compression_ratio horsepower peak_rpm city_mpg highway_mpg price
    ## 1               9.0        111     5000       21          27 16500
    ## 2               9.0        154     5000       19          26 16500
    ## 3              10.0        102     5500       24          30 13950
    ## 4               8.0        115     5500       18          22 17450
    ## 5               8.5        110     5500       19          25 15250
    ## 6               8.5        110     5500       19          25 17710

``` r
glimpse(cars)
```

    ## Rows: 204
    ## Columns: 26
    ## $ symboling         <int> 3, 1, 2, 2, 2, 1, 1, 1, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2…
    ## $ normalized_losses <chr> "?", "?", "164", "164", "?", "158", "?", "158", "?",…
    ## $ make              <chr> "alfa-romero", "alfa-romero", "audi", "audi", "audi"…
    ## $ fuel_type         <chr> "gas", "gas", "gas", "gas", "gas", "gas", "gas", "ga…
    ## $ aspiration        <chr> "std", "std", "std", "std", "std", "std", "std", "tu…
    ## $ num_doors         <chr> "two", "two", "four", "four", "two", "four", "four",…
    ## $ body_style        <chr> "convertible", "hatchback", "sedan", "sedan", "sedan…
    ## $ drive_wheels      <chr> "rwd", "rwd", "fwd", "4wd", "fwd", "fwd", "fwd", "fw…
    ## $ engine_location   <chr> "front", "front", "front", "front", "front", "front"…
    ## $ wheel_base        <dbl> 88.6, 94.5, 99.8, 99.4, 99.8, 105.8, 105.8, 105.8, 9…
    ## $ length            <dbl> 168.8, 171.2, 176.6, 176.6, 177.3, 192.7, 192.7, 192…
    ## $ width             <dbl> 64.1, 65.5, 66.2, 66.4, 66.3, 71.4, 71.4, 71.4, 67.9…
    ## $ height            <dbl> 48.8, 52.4, 54.3, 54.3, 53.1, 55.7, 55.7, 55.9, 52.0…
    ## $ curb_weight       <int> 2548, 2823, 2337, 2824, 2507, 2844, 2954, 3086, 3053…
    ## $ engine_type       <chr> "dohc", "ohcv", "ohc", "ohc", "ohc", "ohc", "ohc", "…
    ## $ num_cylinders     <chr> "four", "six", "four", "five", "five", "five", "five…
    ## $ engine_size       <int> 130, 152, 109, 136, 136, 136, 136, 131, 131, 108, 10…
    ## $ fuel_system       <chr> "mpfi", "mpfi", "mpfi", "mpfi", "mpfi", "mpfi", "mpf…
    ## $ bore              <chr> "3.47", "2.68", "3.19", "3.19", "3.19", "3.19", "3.1…
    ## $ stroke            <chr> "2.68", "3.47", "3.40", "3.40", "3.40", "3.40", "3.4…
    ## $ compression_ratio <dbl> 9.00, 9.00, 10.00, 8.00, 8.50, 8.50, 8.50, 8.30, 7.0…
    ## $ horsepower        <chr> "111", "154", "102", "115", "110", "110", "110", "14…
    ## $ peak_rpm          <chr> "5000", "5000", "5500", "5500", "5500", "5500", "550…
    ## $ city_mpg          <int> 21, 19, 24, 18, 19, 19, 19, 17, 16, 23, 23, 21, 21, …
    ## $ highway_mpg       <int> 27, 26, 30, 22, 25, 25, 25, 20, 22, 29, 29, 28, 28, …
    ## $ price             <chr> "16500", "16500", "13950", "17450", "15250", "17710"…

After having a look at the data above, it is noticeable that some
numeric values are currently in the character data type
(e.g. normalized_losses and num_doors). In addition, missing values in
this dataset are presented as “?” rather than “NA”
(e.g. normalized_losses). Thus, we are going to replace those question
marks with NA to simplify our process of counting missing values as well
as convert numeric values in the character data type to the numeric data
type for further analysis.

It is worth mentioning that other character variables could be
interpreted as categories (e.g. make and body_style). Therefore, we are
going to dummy-code such variables for running the prediction model
later.

``` r
cars[cars == "?"] <- NA

colSums(is.na(cars))
```

    ##         symboling normalized_losses              make         fuel_type 
    ##                 0                40                 0                 0 
    ##        aspiration         num_doors        body_style      drive_wheels 
    ##                 0                 2                 0                 0 
    ##   engine_location        wheel_base            length             width 
    ##                 0                 0                 0                 0 
    ##            height       curb_weight       engine_type     num_cylinders 
    ##                 0                 0                 0                 0 
    ##       engine_size       fuel_system              bore            stroke 
    ##                 0                 0                 4                 4 
    ## compression_ratio        horsepower          peak_rpm          city_mpg 
    ##                 0                 2                 2                 0 
    ##       highway_mpg             price 
    ##                 0                 4

From the data above, it is obvious that there are 40 missing values in
the “normalized_losses” column, which accounts for approximately 20% of
the original dataset. Since this number is quite considerable, we will
replace missing values in the column with the average value of the
column rather than omitting them.

### Data cleaning and transformation

Before dealing with missing values, we will identify category columns
(placed in variable “cat_col”), dummy-code them and create a new data
frame containing such dummy values called “dummy_cars”.

``` r
cat_cols <- c(
  "make",
  "fuel_type",
  "aspiration",
  "body_style",
  "drive_wheels",
  "engine_location",
  "engine_type",
  "fuel_system"
)

dummy_df <- dummy_cols(cars, 
                       select_columns = cat_cols)


dummy_cars <- dummy_df %>% select(-cat_cols)

glimpse(dummy_cars)
```

    ## Rows: 204
    ## Columns: 69
    ## $ symboling              <int> 3, 1, 2, 2, 2, 1, 1, 1, 0, 2, 0, 0, 0, 1, 0, 0,…
    ## $ normalized_losses      <chr> NA, NA, "164", "164", NA, "158", NA, "158", NA,…
    ## $ num_doors              <chr> "two", "two", "four", "four", "two", "four", "f…
    ## $ wheel_base             <dbl> 88.6, 94.5, 99.8, 99.4, 99.8, 105.8, 105.8, 105…
    ## $ length                 <dbl> 168.8, 171.2, 176.6, 176.6, 177.3, 192.7, 192.7…
    ## $ width                  <dbl> 64.1, 65.5, 66.2, 66.4, 66.3, 71.4, 71.4, 71.4,…
    ## $ height                 <dbl> 48.8, 52.4, 54.3, 54.3, 53.1, 55.7, 55.7, 55.9,…
    ## $ curb_weight            <int> 2548, 2823, 2337, 2824, 2507, 2844, 2954, 3086,…
    ## $ num_cylinders          <chr> "four", "six", "four", "five", "five", "five", …
    ## $ engine_size            <int> 130, 152, 109, 136, 136, 136, 136, 131, 131, 10…
    ## $ bore                   <chr> "3.47", "2.68", "3.19", "3.19", "3.19", "3.19",…
    ## $ stroke                 <chr> "2.68", "3.47", "3.40", "3.40", "3.40", "3.40",…
    ## $ compression_ratio      <dbl> 9.00, 9.00, 10.00, 8.00, 8.50, 8.50, 8.50, 8.30…
    ## $ horsepower             <chr> "111", "154", "102", "115", "110", "110", "110"…
    ## $ peak_rpm               <chr> "5000", "5000", "5500", "5500", "5500", "5500",…
    ## $ city_mpg               <int> 21, 19, 24, 18, 19, 19, 19, 17, 16, 23, 23, 21,…
    ## $ highway_mpg            <int> 27, 26, 30, 22, 25, 25, 25, 20, 22, 29, 29, 28,…
    ## $ price                  <chr> "16500", "16500", "13950", "17450", "15250", "1…
    ## $ `make_alfa-romero`     <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_audi              <int> 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_bmw               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ make_chevrolet         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_dodge             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_honda             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_isuzu             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_jaguar            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mazda             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ `make_mercedes-benz`   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mercury           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mitsubishi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_nissan            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_peugot            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_plymouth          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_porsche           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_renault           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_saab              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_subaru            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_toyota            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_volkswagen        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_volvo             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_type_diesel       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_type_gas          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ aspiration_std         <int> 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ aspiration_turbo       <int> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_convertible <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_hardtop     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_hatchback   <int> 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_sedan       <int> 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ body_style_wagon       <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_4wd       <int> 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_fwd       <int> 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_rwd       <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_location_front  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_location_rear   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_dohc       <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_dohcv      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_l          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_ohc        <int> 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_type_ohcf       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_ohcv       <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_rotor      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_1bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_2bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_4bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_idi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_mfi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_mpfi       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ fuel_system_spdi       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_spfi       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

Now, we are going to convert the other character variables (placed in
variable “num_convert_cols”) to numeric ones. We will also replace
missing values in the “normalized_loss” column with the average value of
such a column.

``` r
num_convert_cols <- c(
  "normalized_losses",
  "bore",
  "stroke",
  "horsepower",
  "peak_rpm",
  "price"
)
  
dummy_cars[num_convert_cols] <- sapply(dummy_cars[num_convert_cols],as.numeric)


mean(dummy_cars$normalized_losses, na.rm = TRUE)
```

    ## [1] 122

``` r
dummy_cars <- dummy_cars  %>%
  mutate(normalized_losses = replace_na(normalized_losses, mean(dummy_cars$normalized_losses, na.rm = TRUE)
))

dummy_cars <- dummy_cars  %>%
  mutate(
    num_doors = str_replace_all(num_doors, pattern = "four", replacement = "4")) %>% mutate(
    num_doors = str_replace_all(num_doors, pattern = "two", replacement = "2")) %>% mutate(
    num_doors = as.numeric(num_doors))

dummy_cars <- dummy_cars  %>%
  mutate(
    num_cylinders = str_replace_all(num_cylinders, pattern = "four", replacement = "4")) %>% mutate(
      num_cylinders = str_replace_all(num_cylinders, pattern = "two", replacement = "2")) %>% mutate(
        num_cylinders = str_replace_all(num_cylinders, pattern = "three", replacement = "3")) %>% mutate(
          num_cylinders = str_replace_all(num_cylinders, pattern = "five", replacement = "5")) %>% mutate(
            num_cylinders = str_replace_all(num_cylinders, pattern = "six", replacement = "6")) %>% mutate(
              num_cylinders = str_replace_all(num_cylinders, pattern = "eight", replacement = "8")) %>% mutate(
                num_cylinders = str_replace_all(num_cylinders, pattern = "twelve", replacement = "12")) %>% mutate(
        num_cylinders = as.numeric(num_cylinders))

glimpse(dummy_cars)
```

    ## Rows: 204
    ## Columns: 69
    ## $ symboling              <int> 3, 1, 2, 2, 2, 1, 1, 1, 0, 2, 0, 0, 0, 1, 0, 0,…
    ## $ normalized_losses      <dbl> 122, 122, 164, 164, 122, 158, 122, 158, 122, 19…
    ## $ num_doors              <dbl> 2, 2, 4, 4, 2, 4, 4, 4, 2, 2, 4, 2, 4, 4, 4, 2,…
    ## $ wheel_base             <dbl> 88.6, 94.5, 99.8, 99.4, 99.8, 105.8, 105.8, 105…
    ## $ length                 <dbl> 168.8, 171.2, 176.6, 176.6, 177.3, 192.7, 192.7…
    ## $ width                  <dbl> 64.1, 65.5, 66.2, 66.4, 66.3, 71.4, 71.4, 71.4,…
    ## $ height                 <dbl> 48.8, 52.4, 54.3, 54.3, 53.1, 55.7, 55.7, 55.9,…
    ## $ curb_weight            <int> 2548, 2823, 2337, 2824, 2507, 2844, 2954, 3086,…
    ## $ num_cylinders          <dbl> 4, 6, 4, 5, 5, 5, 5, 5, 5, 4, 4, 6, 6, 6, 6, 6,…
    ## $ engine_size            <int> 130, 152, 109, 136, 136, 136, 136, 131, 131, 10…
    ## $ bore                   <dbl> 3.47, 2.68, 3.19, 3.19, 3.19, 3.19, 3.19, 3.13,…
    ## $ stroke                 <dbl> 2.68, 3.47, 3.40, 3.40, 3.40, 3.40, 3.40, 3.40,…
    ## $ compression_ratio      <dbl> 9.00, 9.00, 10.00, 8.00, 8.50, 8.50, 8.50, 8.30…
    ## $ horsepower             <dbl> 111, 154, 102, 115, 110, 110, 110, 140, 160, 10…
    ## $ peak_rpm               <dbl> 5000, 5000, 5500, 5500, 5500, 5500, 5500, 5500,…
    ## $ city_mpg               <int> 21, 19, 24, 18, 19, 19, 19, 17, 16, 23, 23, 21,…
    ## $ highway_mpg            <int> 27, 26, 30, 22, 25, 25, 25, 20, 22, 29, 29, 28,…
    ## $ price                  <dbl> 16500, 16500, 13950, 17450, 15250, 17710, 18920…
    ## $ `make_alfa-romero`     <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_audi              <int> 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_bmw               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ make_chevrolet         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_dodge             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_honda             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_isuzu             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_jaguar            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mazda             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ `make_mercedes-benz`   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mercury           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_mitsubishi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_nissan            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_peugot            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_plymouth          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_porsche           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_renault           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_saab              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_subaru            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_toyota            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_volkswagen        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ make_volvo             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_type_diesel       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_type_gas          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ aspiration_std         <int> 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ aspiration_turbo       <int> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_convertible <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_hardtop     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_hatchback   <int> 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ body_style_sedan       <int> 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ body_style_wagon       <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_4wd       <int> 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_fwd       <int> 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ drive_wheels_rwd       <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_location_front  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_location_rear   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_dohc       <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_dohcv      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_l          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_ohc        <int> 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ engine_type_ohcf       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_ohcv       <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ engine_type_rotor      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_1bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_2bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_4bbl       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_idi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_mfi        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_mpfi       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ fuel_system_spdi       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ fuel_system_spfi       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

After converting all variables into the numeric type, we are about to
remove rows containing missing values since those values just account
for roughly 5.8% of the dataset.

``` r
dummy_cars <- dummy_cars %>% drop_na()

nrow(dummy_cars)
```

    ## [1] 192

Now, the new dataset containing 192 rows is ready for training and
testing our price prediction model.

### Setting up training and testing sets

Next, we will split the data into training and testing sets.

The training set named “train_data” will contain 80% of the dataset.

The remaining 20 %of the dataset will belong to the testing test named
“test_data”.

``` r
train_indices <- createDataPartition(dummy_cars$price, p = 0.8,  list = FALSE)
train_data <- dummy_cars[train_indices,]
test_data <- dummy_cars[-train_indices,]
```

### Training and testing the prediction model

In this project, we will employ the method of K-Nearest Neighbors to
train a machine learning algorithm on the training set having “price” 
as our target and the other variables as our features. We will also 
normalize the data and use 10-fold cross-validation to reduce the 
influence of potential outliers as well as a hyperparameter grid of 
20 to improve the accuracy of the prediction model. 

``` r
ten_fold_control <- trainControl(method = "cv", number = 10)
knn_grid <- expand.grid(k = 1:20)

knn_model <- train(price ~ .,
                   data = train_data,
                   method = "knn",
                   trControl = ten_fold_control,
                   preProcess = c("center", "scale"),
                   tuneGrid = knn_grid)
knn_model
```

    ## k-Nearest Neighbors 
    ## 
    ## 156 samples
    ##  68 predictor
    ## 
    ## Pre-processing: centered (68), scaled (68) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 140, 140, 141, 141, 140, 142, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   RMSE      Rsquared   MAE     
    ##    1  3063.812  0.8633619  2129.210
    ##    2  3059.935  0.8738203  2074.150
    ##    3  3517.741  0.8325645  2343.190
    ##    4  3943.849  0.7984034  2558.680
    ##    5  4279.335  0.7702285  2768.108
    ##    6  4537.034  0.7457509  2906.759
    ##    7  4447.375  0.7624304  2857.234
    ##    8  4332.897  0.7819365  2811.975
    ##    9  4270.327  0.7900557  2749.407
    ##   10  4294.957  0.7909197  2717.971
    ##   11  4387.058  0.7961606  2739.570
    ##   12  4355.251  0.8158898  2705.501
    ##   13  4356.432  0.8180246  2664.914
    ##   14  4464.702  0.8178818  2699.818
    ##   15  4502.523  0.8237270  2714.513
    ##   16  4523.757  0.8254956  2703.489
    ##   17  4529.060  0.8305992  2686.170
    ##   18  4589.981  0.8143108  2693.641
    ##   19  4610.257  0.8132410  2689.591
    ##   20  4663.953  0.8175702  2706.490
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was k = 2.

### Model evaluation

Now, we are about to use the testing set to test our prediction model
and then have a quick look at the actual and predicted prices (price
versus predictions) for comparison. We will also compute the root mean
squared error (RMSE), R squared, and mean absolute error (MAE) to
evaluate the accuracy of our model.

``` r
test_predictions <- predict(knn_model, newdata = test_data)

predicted_test <- test_data %>% mutate(
  predictions = test_predictions
)

predicted_test %>% select(price, predictions)
```

    ##     price predictions
    ## 7   18920   16480.000
    ## 11  20970   22835.000
    ## 22   7957    5974.500
    ## 23   6229    5974.500
    ## 25   7609    6534.500
    ## 31   6529    6992.000
    ## 33   7295    9570.000
    ## 35   7895    7987.000
    ## 36   9095    7987.000
    ## 43  32250   35775.000
    ## 46   5195    6445.000
    ## 49   6695    8711.667
    ## 59  28248   28576.000
    ## 63  35056   37572.000
    ## 65  45400   37572.000
    ## 74  14869   13559.000
    ## 77   8189    7744.000
    ## 83   6849    7399.000
    ## 97  18399   18449.000
    ## 100 12440   16662.500
    ## 101 13860   17308.333
    ## 113  7609    6460.500
    ## 115 12764   10293.000
    ## 118 34028   34778.000
    ## 121 12170   15235.000
    ## 123 15510   15235.000
    ## 132  9233    7808.000
    ## 135 10198    8711.500
    ## 137 11694    9636.000
    ## 144  6938    8498.000
    ## 157  9989    9893.500
    ## 175  9495    7885.000
    ## 176  9995    8345.000
    ## 180 13845    7885.000
    ## 184 15985   14892.500
    ## 186 18420   20835.000

``` r
postResample(pred = test_predictions, obs = test_data$price)
```

    ##         RMSE     Rsquared          MAE 
    ## 2444.6956026    0.9340569 1836.2777778

### Conclusion

Based on the metrics above, it is reasonable to conclude that our
prediction model works pretty well. Specifically, high R-squared means
that the price variable is pretty well-explained by other variables
while the other error metrics are quite low.
