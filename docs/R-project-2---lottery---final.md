Developing Some Features for a Lottery-winning Calculator App
================
Addison
2022-09-14

### Introduction

This project aims to develop some features for an app calculating 6/49
lottery winning probabilities in Canada based on a dataset that contains
3665 rows, each of which describes a historical drawing day and numbers
drawn on that day from 1982 to 2018. The dataset could be obtained
through this link:
<https://drive.google.com/file/d/1xRS5_yflQs63d8FyyPG5DauHyO_JRVQc/view?usp=sharing>.

Now, to start with, let’s have a quick look at the first few rows as
well as essential information of the data to imagine what the data looks
like as well as get to know the data types of columns and the number of
null values.

``` r
library(tidyverse)

lottery <- read.csv("/Users/apple/Downloads/R dataquest/649.csv")

head(lottery)
```

    ##   PRODUCT DRAW.NUMBER SEQUENCE.NUMBER DRAW.DATE NUMBER.DRAWN.1 NUMBER.DRAWN.2
    ## 1     649           1               0 6/12/1982              3             11
    ## 2     649           2               0 6/19/1982              8             33
    ## 3     649           3               0 6/26/1982              1              6
    ## 4     649           4               0  7/3/1982              3              9
    ## 5     649           5               0 7/10/1982              5             14
    ## 6     649           6               0 7/17/1982              8             20
    ##   NUMBER.DRAWN.3 NUMBER.DRAWN.4 NUMBER.DRAWN.5 NUMBER.DRAWN.6 BONUS.NUMBER
    ## 1             12             14             41             43           13
    ## 2             36             37             39             41            9
    ## 3             23             24             27             39           34
    ## 4             10             13             20             43           34
    ## 5             21             31             34             47           45
    ## 6             21             25             31             41           33

``` r
glimpse(lottery)
```

    ## Rows: 3,665
    ## Columns: 11
    ## $ PRODUCT         <int> 649, 649, 649, 649, 649, 649, 649, 649, 649, 649, 649,…
    ## $ DRAW.NUMBER     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
    ## $ SEQUENCE.NUMBER <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ DRAW.DATE       <chr> "6/12/1982", "6/19/1982", "6/26/1982", "7/3/1982", "7/…
    ## $ NUMBER.DRAWN.1  <int> 3, 8, 1, 3, 5, 8, 18, 7, 5, 4, 7, 11, 7, 25, 8, 9, 4, …
    ## $ NUMBER.DRAWN.2  <int> 11, 33, 6, 9, 14, 20, 25, 16, 10, 15, 9, 17, 14, 28, 1…
    ## $ NUMBER.DRAWN.3  <int> 12, 36, 23, 10, 21, 21, 28, 17, 23, 30, 21, 19, 17, 29…
    ## $ NUMBER.DRAWN.4  <int> 14, 37, 24, 13, 31, 25, 33, 31, 27, 37, 33, 20, 20, 30…
    ## $ NUMBER.DRAWN.5  <int> 41, 39, 27, 20, 34, 31, 36, 40, 37, 46, 38, 36, 37, 35…
    ## $ NUMBER.DRAWN.6  <int> 43, 41, 39, 43, 47, 41, 42, 48, 38, 48, 42, 43, 47, 44…
    ## $ BONUS.NUMBER    <int> 13, 9, 34, 34, 45, 33, 7, 26, 33, 3, 45, 9, 34, 3, 31,…

``` r
colSums(is.na(cars))
```

    ## speed  dist 
    ##     0     0

The data looks clean and ready for further analysis with no missing
values.

For the purpose of this project, we will only use columns “NUMBER DRAWN”
1-6.

### Building the first feature

Now, we are going to build our first feature for the app called
“one_ticket_probability”, which takes in a vector of 6 unique numbers
and prints the probability of winning.

Since players are allowed to pick their own numbers as well as the order
of chosen numbers does not matter, we will use the combination formula
for building the first feature.

``` r
one_ticket_probability <- function(six_num_vector){
  num_outcomes <- choose(49,6)
  winning_prob <- 1/num_outcomes*100
  num_string <- paste0("(", paste(six_num_vector, collapse = ", "), ")")
  message <- sprintf("The probability of winning the big prize of %s is %.7f%%", num_string, winning_prob)
  return (message)
}
```

Now, we are about to test our first feature with a vector of 6 random
different numbers to see what the output looks like. It is also worth
noting that any combination of 6 numbers has the same probability of
winning, meaning that the output of this function is the same for any
combination of 6 random different numbers

``` r
test_input_1 = c(2, 43, 22, 23, 11, 5)
one_ticket_probability(test_input_1)
```

    ## [1] "The probability of winning the big prize of (2, 43, 22, 23, 11, 5) is 0.0000072%"

### Building the second feature

Next, we will build the second feature called
“check_historical_occurence”, which takes in two inputs: a vector of 6
random different numbers and the list called “winning_numbers” that
contains the sets of the winning numbers in the past and then prints the
number of times the combination inputted by the user occurred in the
historical record.

``` r
winning_numbers <- pmap(lottery[5:10], c, use.names = F)

check_historical_occurrence <- function(six_num_vector, winning_series) {
  num_occurrence <- 0
  for(element in winning_series){
    if(setequal(six_num_vector, element))
    {num_occurrence <- num_occurrence +1}
  }
  num_string <- paste0("(", paste(six_num_vector, collapse = ", "), ")")
  message <- sprintf("The combination %s has occurred %d time(s) in the historical record", num_string, num_occurrence)
  
  return(message)
}
```

Now, we are going to test our second feature with a vector of 6 random
different numbers to see what the output looks like.

``` r
test_input_2 <- c(33, 36, 37, 39, 8, 41)

check_historical_occurrence(test_input_2, winning_numbers)
```

    ## [1] "The combination (33, 36, 37, 39, 8, 41) has occurred 1 time(s) in the historical record"

### Building the third feature

Next, we will build the third feature called “multi_ticket_probability”,
which takes in one input, which is the number of tickets played, and
prints the probability of winning the big prize. The probability is
calculated by having the number of tickets played divided by the total
outcomes and then multiplied by 100.

``` r
multi_ticket_probability <- function(quantity) {
  num_outcomes = choose(49,6)
  winning_prob <- quantity/num_outcomes*100
  message <- sprintf("The probability of winning the big prize with %d different tickets is %.7f%%", quantity, winning_prob)
  return(message)
}
```

Now, we are going to test our third feature with different numbers of
tickets played to see what the outputs look like.

``` r
test_input_3 <- c(1, 10, 100, 10000, 1000000, 6991908, 13983816)

multi_ticket_probability(test_input_3)
```

    ## [1] "The probability of winning the big prize with 1 different tickets is 0.0000072%"         
    ## [2] "The probability of winning the big prize with 10 different tickets is 0.0000715%"        
    ## [3] "The probability of winning the big prize with 100 different tickets is 0.0007151%"       
    ## [4] "The probability of winning the big prize with 10000 different tickets is 0.0715112%"     
    ## [5] "The probability of winning the big prize with 1000000 different tickets is 7.1511238%"   
    ## [6] "The probability of winning the big prize with 6991908 different tickets is 50.0000000%"  
    ## [7] "The probability of winning the big prize with 13983816 different tickets is 100.0000000%"

### Building the fourth feature

In most 6/49 lotteries, there are smaller prizes if a player’s ticket
matches two, three, four, or five of the six numbers drawn. Thus, we
will build the fourth feature called “probability_less_6”, which takes
in an integer from 2 to 5 and prints the probability of winning a small
prize with that quantity of winning numbers.

``` r
probability_less_6 <- function(number){
  num_outcomes <- choose(49,6)
  num_combination <- choose(6, number)
  num_combination_remaining <- choose(43, 6 - number)
  successful_outcome <- num_combination*num_combination_remaining
  prob <- successful_outcome/num_outcomes*100
  message <- sprintf("The probability of winning a small prize with %d winning numbers is  %.7f%%", number, prob)
  return(message)
  
}
```

Now, we are going to test our fourth feature with all the numbers from 2
to 5 to see what the outputs look like.

``` r
test_input_4 <- c(2, 3, 4, 5)
probability_less_6(test_input_4)
```

    ## [1] "The probability of winning a small prize with 2 winning numbers is  13.2378029%"
    ## [2] "The probability of winning a small prize with 3 winning numbers is  1.7650404%" 
    ## [3] "The probability of winning a small prize with 4 winning numbers is  0.0968620%" 
    ## [4] "The probability of winning a small prize with 5 winning numbers is  0.0018450%"

### Conclusion

In this project, we successfully built four features for a
lottery-winning calculator app which are:

-   “one_ticket_probability”

-   “check_historical_occurrence”

-   “multi_ticket_probability”

-   “probability_less_6”

We hope that users could realize that the chances of winning lottery
prizes are really small, so they should not waste money on lotteries or
buy lottery tickets excessively.
