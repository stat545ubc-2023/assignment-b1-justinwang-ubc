assignmentB1
================
yiwang
2023-10-30

## Prerequisites

I will wrap up function from using the tidyverse

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## 载入程辑包：'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

## Exercise 1: Make a Function (25 points)

I will make a dummy function which can group the student by group and
can summarize each group student 100 meters running result in second
such as min, max, mean.

I will write the code and make the informative documentation together in
the following code chunk in section exercise 2, please read the next
section .

## Exercise 2: Document your Function (20 points)

``` r
#' 100 meters running results(min, max, mean, the number of people) grouped by group 
#' 
#'this function calculated the max, min, mean, and the number of people in each group.
#'
#' @param data A data frame contain the data. the reason to make this name is clear to make as data or df
#' @param group_var A variable group by, the reason to make this name is the variable to be applied by group_by() tidyselection function
#' @param na.rm A boolean value for NA to be removed, which applied to function min,max, etc. default value is true. the reason to make this name is this variable was assgin to na.rm
#' @param ... Other paramter pass to the min(),max(),max()
#' 
#' @return A Table contain the result of min, max, mean, and the number of people in each group.

grouped_100_meters_result <- function(data, group_var,var, na.rm = true, ...){
  data %>% 
    group_by({{ group_var }}) %>%
    summarize(
      min = min({{ var }}, na.rm = na.rm, ...),
      mean = mean({{ var }}, na.rm = na.rm, ...),
      max = max({{ var }},na.rm = na.rm, ...),
      n = n()
    )
  
}
```

## Exercise 3: Include examples (15 points)

make a dummy tibble which contain variable group, 100_metres result,
gender, and their name.

``` r
class_545_running_score <- tibble(
  group = c(1,2,3,1,2,3,1),
  result = c(13.5,14,11.5,13.6,12.6,16,15.5),
  gender= c("male","female","male","male","female","female","male"),
  name = c("justin","lily","james","henry","helen","lucy","kimberly")
)
class_545_running_score
```

    ## # A tibble: 7 × 4
    ##   group result gender name    
    ##   <dbl>  <dbl> <chr>  <chr>   
    ## 1     1   13.5 male   justin  
    ## 2     2   14   female lily    
    ## 3     3   11.5 male   james   
    ## 4     1   13.6 male   henry   
    ## 5     2   12.6 female helen   
    ## 6     3   16   female lucy    
    ## 7     1   15.5 male   kimberly

I will group by group variable, but we can also group by gender.

``` r
result <- grouped_100_meters_result(class_545_running_score, group, result,na.rm = TRUE)
result
```

    ## # A tibble: 3 × 5
    ##   group   min  mean   max     n
    ##   <dbl> <dbl> <dbl> <dbl> <int>
    ## 1     1  13.5  14.2  15.5     3
    ## 2     2  12.6  13.3  14       2
    ## 3     3  11.5  13.8  16       2

This table make sense as we can see that in group 1, there are 3 people,
mean 100 meters running result in second is 14.2. In group 2, mean 100
meters running result is 13.30 which is best among three group. In group
3, the mean running result is 13.75, second best.

## Exercise 4: Test the Function (25 points)

``` r
# test case 1: Vector with no NA
test_that("Test case 1: Vector with no NA's", {
  data <- tibble(
    group = c(1, 1, 2, 2, 3, 3),
    result = c(10.5, 11.2, 9.8, 9.9, 12.0, 11.5)
  )
  result <- grouped_100_meters_result(data, group, result, na.rm = TRUE)
  
 
  expect_true("group" %in% colnames(result))
  expect_length(result$group, 3)
  
  expect_equal(result %>% filter(group == 1) %>% pull(mean), 10.85)
  
})
```

    ## Test passed 🎉

``` r
# Test case 2: Vector with NA's
test_that("Test case 2: Vector with NA's", {
  data <- tibble(
    group = c(1, 1, 2, 2, 3, 3),
    result = c(10.5, NA, 9.8, 9.9, 12.0, 11.5)
  )
  result1 <- grouped_100_meters_result(data, group, result, na.rm = TRUE)
  expect_equal(result1 %>% filter(group == 1) %>% pull(mean), 10.5)
  
  result2 <- grouped_100_meters_result(data, group, result, na.rm = FALSE)
  expect_equal(result2 %>% filter(group ==1) %>% pull(mean), NA_real_)
 
})
```

    ## Test passed 😀

``` r
# Test case 3: Vector of a different type (character)
test_that("Test case 3: Vector of a different type (character)", {
 expect_error(grouped_100_meters_result(TRUE))
  expect_error(grouped_100_meters_result("hello"))
   expect_error(grouped_100_meters_result(list(1:5)))


})
```

    ## Test passed 🥇
