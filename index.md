P8400 - Epidemiology - Lab \#1
================

``` r
library(tidyverse)
library(janitor)
library(gmodels)
```

------------------------------------------------------------------------

### Lab Learning Objectives

-   ~~To become reacquainted with SAS~~
-   To review the odds ratio (OR) and risk ratio (RR)
-   To learn how sample size affects precision
-   To visually assess the relationship between measures
-   To learn how prevalence of disease among the unexposed influences
    that relationship

------------------------------------------------------------------------

### Part A

In Part A of this lab we will be using a simple, made-up dataset
examining risk factors for cardiovascular disease (CVD). We will create
2x2 tables to examine the relationship between family history, obesity,
and statin use and CVD.

| Column No. | Variable Name | Range/Description                                                                    |
|------------|---------------|--------------------------------------------------------------------------------------|
| 1          | id            | 1-20 (for CVD) and 1-50 (for CVD\_2)                                                 |
| 2          | cvd           | Cardiovascular Disease Status <br> 0 = Non-Diseased (no CVD) <br> 1 = Diseased (CVD) |
| 3          | famhx         | Family History of CVD <br> 0 = No Family History <br> 1 = Family History             |
| 4          | obese         | Obesity <br> 0 = Not Obese <br> 1 = Obese                                            |
| 5          | statin        | Statin Drug Use <br> 0 = No Statin Drug Use <br> 1 = Statin Drug Use                 |

------------------------------------------------------------------------

##### 1. \[Original SAS Instruction\] Run the format and data statement below to input the data. Run the print procedure to verify that the data were loaded correctly.

In R, we can do the same task by creating a *tibble* using the
`tribble()` function as follows:

``` r
# First, we need to create our binary variable labels
yes_no_labels <- c("No", "Yes")

# Second, we need to manually input the variable names and values
cvd <- tribble(
        ~id, ~cvd, ~famhx, ~obese, ~statin,
        1,0,1,1,1,
        2,0,0,0,0,
        3,1,1,0,0,
        4,1,1,1,1,
        5,1,0,1,0,
        6,0,0,0,1,
        7,1,1,0,0,
        8,1,0,0,1,
        9,0,0,0,1,
        10,1,1,1,0,
        11,0,1,1,1,
        12,0,0,0,0,
        13,1,1,0,0,
        14,1,1,1,1,
        15,1,0,1,0,
        16,0,0,0,1,
        17,1,1,0,0,
        18,1,0,0,1,
        19,0,0,0,1,
        20,1,1,1,0) %>%
# Third, we need to convert our variables into their appropriate types and apply our labels
    mutate(
        id = as.integer(id),
        cvd = as_factor(cvd),
        cvd = fct_relabel(cvd, ~paste0(yes_no_labels)),
        famhx = as_factor(famhx),
        famhx = fct_relabel(famhx, ~paste0(yes_no_labels)),
        obese = as_factor(obese),
        obese = fct_relabel(obese, ~paste0(yes_no_labels)),
        statin = as_factor(statin),
        statin = fct_relabel(statin, ~paste0(yes_no_labels)))

head(cvd)
```

    ## # A tibble: 6 x 5
    ##      id cvd   famhx obese statin
    ##   <int> <fct> <fct> <fct> <fct> 
    ## 1     1 No    Yes   Yes   Yes   
    ## 2     2 No    No    No    No    
    ## 3     3 Yes   Yes   No    No    
    ## 4     4 Yes   Yes   Yes   Yes   
    ## 5     5 Yes   No    Yes   No    
    ## 6     6 No    No    No    Yes

------------------------------------------------------------------------

##### 2. Create a 2x2 table of Family History and CVD Status

We can do this a few different ways, but I like to use the `tabyl()`
function in the `janitor` package. Note: as I was putting this together
I ran into some difficulties with generating my tables as I needed them
to be. As such, I am now making use of the `gmodels` package, which
functions similarly to `PROC FREQ` in SAS.

``` r
famhx_cvd_xtab <-
    CrossTable(cvd$famhx, cvd$cvd, prop.chisq = FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  20 
    ## 
    ##  
    ##              | cvd$cvd 
    ##    cvd$famhx |        No |       Yes | Row Total | 
    ## -------------|-----------|-----------|-----------|
    ##           No |         6 |         4 |        10 | 
    ##              |     0.600 |     0.400 |     0.500 | 
    ##              |     0.750 |     0.333 |           | 
    ##              |     0.300 |     0.200 |           | 
    ## -------------|-----------|-----------|-----------|
    ##          Yes |         2 |         8 |        10 | 
    ##              |     0.200 |     0.800 |     0.500 | 
    ##              |     0.250 |     0.667 |           | 
    ##              |     0.100 |     0.400 |           | 
    ## -------------|-----------|-----------|-----------|
    ## Column Total |         8 |        12 |        20 | 
    ##              |     0.400 |     0.600 |           | 
    ## -------------|-----------|-----------|-----------|
    ## 
    ## 

As we can see from our output our cells are not placed in the
traditional spots. That is, our upper-left square should reflect the
values of those who both had the exposure and the outcome, and the
lower-right square should reflect the values of those who were neither
exposed nor had the outcome. We will need to make some adjustments.

------------------------------------------------------------------------

##### 3. \[Original SAS Instruction\] Flip the table so that the cell ‘A’ is Exposed/Diseased. Next, utilize the format statement at the top of the program to label the Exposure/Disease values.

We can make use of the `fct_rev()` function in the `forcats`
package–part of the overall `tidyverse` to reverse the factor levels of
our four factor variables.

``` r
cvd <- 
    cvd %>% 
        mutate(
            cvd = fct_rev(cvd),
            famhx = fct_rev(famhx),
            obese = fct_rev(obese),
            statin = fct_rev(statin))
```

In so doing, we should arrive at our correctly formatted table.

``` r
famhx_cvd_xtab_formatted <-
    CrossTable(cvd$famhx, cvd$cvd, prop.chisq = FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  20 
    ## 
    ##  
    ##              | cvd$cvd 
    ##    cvd$famhx |       Yes |        No | Row Total | 
    ## -------------|-----------|-----------|-----------|
    ##          Yes |         8 |         2 |        10 | 
    ##              |     0.800 |     0.200 |     0.500 | 
    ##              |     0.667 |     0.250 |           | 
    ##              |     0.400 |     0.100 |           | 
    ## -------------|-----------|-----------|-----------|
    ##           No |         4 |         6 |        10 | 
    ##              |     0.400 |     0.600 |     0.500 | 
    ##              |     0.333 |     0.750 |           | 
    ##              |     0.200 |     0.300 |           | 
    ## -------------|-----------|-----------|-----------|
    ## Column Total |        12 |         8 |        20 | 
    ##              |     0.600 |     0.400 |           | 
    ## -------------|-----------|-----------|-----------|
    ## 
    ## 

------------------------------------------------------------------------

##### 4. Calculate the crude Odds Ratio (OR) and Relative Risk (RR) by hand.

We recall that the formula for the Odds Ratio is

*O**R* = *A**C*/*B**D*
