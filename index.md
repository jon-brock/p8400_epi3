P8400 - Principles of Epidemiology III - Lab \#1
================

------------------------------------------------------------------------

``` r
library(tidyverse)
library(janitor)
library(gmodels)
library(epitools)
```

------------------------------------------------------------------------

As part of my Master of Public Health (MPH) in Epidemiology program with
the Mailman School of Public Health at Columbia University we were
required to take a series of core epidemiology courses. In the Fall of
2019 I, and all other Epi students, took P8400 - Principles of
Epidemiology III. The course was structured around a \~two hour lecture
followed by a \~one hour lab. These labs are the basis for this project.

Most, if not all, schools of public health continue to teach using the
industry-standard SAS. Unfortunately, this can be very limiting to those
who might seek positions in organizations that are attempting to steer
away from their dependence on SAS, or do not use it at all. As such,
with this project I intend to go through my lab work again, though using
only R.

**Note**: None of the exercises discussed herein belong to me. I only
have access to these materials because I was a matriculated student from
September 2018 through May 2020. As such, this repository is not public
and only available to those for whom I showed.

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

#### 1. \[Original SAS Instruction\] Run the format and data statement below to input the data. Run the print procedure to verify that the data were loaded correctly.

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

#### 2. Create a 2x2 table of Family History and CVD Status

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

#### 3. \[Original SAS Instruction\] Flip the table so that the cell ‘A’ is Exposed/Diseased. Next, utilize the format statement at the top of the program to label the Exposure/Disease values.

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

#### 4. Calculate the crude Odds Ratio (OR) and Relative Risk (RR) by hand.

We recall that the formula for the **sample** Odds Ratio is

![ \\hat{OR} = \\frac{AD}{BC} ](https://latex.codecogs.com/png.latex?%20%5Chat%7BOR%7D%20%3D%20%5Cfrac%7BAD%7D%7BBC%7D%20 " \hat{OR} = \frac{AD}{BC} ")

and the formula for the **sample** Risk Ratio is

![ \\hat{RR} = \\frac{A/(A+B)}{C/(C+D)} ](https://latex.codecogs.com/png.latex?%20%5Chat%7BRR%7D%20%3D%20%5Cfrac%7BA%2F%28A%2BB%29%7D%7BC%2F%28C%2BD%29%7D%20 " \hat{RR} = \frac{A/(A+B)}{C/(C+D)} ")

Using our table values we can now simply calculate our corresponding
Odds and Risk Ratios:

![ \\hat{OR} = \\frac{8\*6}{4\*2} = \\frac{48}{8} = 6 ](https://latex.codecogs.com/png.latex?%20%5Chat%7BOR%7D%20%3D%20%5Cfrac%7B8%2A6%7D%7B4%2A2%7D%20%3D%20%5Cfrac%7B48%7D%7B8%7D%20%3D%206%20 " \hat{OR} = \frac{8*6}{4*2} = \frac{48}{8} = 6 ")

  

![ \\hat{RR} = \\frac{8/(8+2)}{4/(4+6)} = \\frac{8/10}{4/10} = 0.8/0.4 = 2 ](https://latex.codecogs.com/png.latex?%20%5Chat%7BRR%7D%20%3D%20%5Cfrac%7B8%2F%288%2B2%29%7D%7B4%2F%284%2B6%29%7D%20%3D%20%5Cfrac%7B8%2F10%7D%7B4%2F10%7D%20%3D%200.8%2F0.4%20%3D%202%20 " \hat{RR} = \frac{8/(8+2)}{4/(4+6)} = \frac{8/10}{4/10} = 0.8/0.4 = 2 ")

------------------------------------------------------------------------

#### 5. Using SAS, calculate the crude OR and RR for the associations between Family History, Statin Use, Obesity, and CVD, as well as the 95% Confidence Intervals.

``` r
# Measuring the crude association between Family History and CVD
or_famhx_cvd <- oddsratio(cvd$famhx, cvd$cvd, method = "wald")
or_famhx_cvd_tbl <- 
    as_tibble(or_famhx_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "odds ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)

rr_famhx_cvd <- riskratio(cvd$famhx, cvd$cvd, method = "wald", rev = "both")
rr_famhx_cvd_tbl <- 
    as_tibble(rr_famhx_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "risk ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)

# Measuring the crude association between Statin Use and CVD
or_statin_cvd <- oddsratio(cvd$statin, cvd$cvd, method = "wald")
or_statin_cvd_tbl <- 
    as_tibble(or_statin_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "odds ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)

rr_statin_cvd <- riskratio(cvd$statin, cvd$cvd, method = "wald", rev = "both")
rr_statin_cvd_tbl <- 
    as_tibble(rr_statin_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "risk ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)

# Measuring the crude association between Obesity and CVD
or_obese_cvd <- oddsratio(cvd$obese, cvd$cvd, method = "wald")
or_obese_cvd_tbl <- 
    as_tibble(or_obese_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "odds ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)

rr_obese_cvd <- riskratio(cvd$obese, cvd$cvd, method = "wald", rev = "both")
rr_obese_cvd_tbl <- 
    as_tibble(rr_obese_cvd$measure) %>% 
    slice(-1) %>% 
    rename(
        "risk ratio" = estimate,
        "95% lower limit" = lower,
        "95% upper limit" = upper)
```

``` r
crude_ors_tbl <- 
    bind_rows(or_famhx_cvd_tbl, or_statin_cvd_tbl, or_obese_cvd_tbl) %>% 
    knitr::kable(
        align = 'c',
        digits = 2,
        caption = "Crude Odds Ratios")

crude_rrs_tbl <- 
    bind_rows(rr_famhx_cvd_tbl, rr_statin_cvd_tbl, rr_obese_cvd_tbl) %>% 
    knitr::kable(
        align = 'c',
        digits = 2,
        caption = "Crude Risk Ratios")
```

``` r
crude_ors_tbl
```

| odds ratio | 95% lower limit | 95% upper limit |
|:----------:|:---------------:|:---------------:|
|    6.00    |      0.81       |      44.35      |
|    0.17    |      0.02       |      1.23       |
|    3.00    |      0.42       |      21.30      |

Crude Odds Ratios

``` r
crude_rrs_tbl
```

| risk ratio | 95% lower limit | 95% upper limit |
|:----------:|:---------------:|:---------------:|
|    2.0     |      0.88       |      4.54       |
|    0.5     |      0.22       |      1.14       |
|    1.5     |      0.75       |      3.00       |

Crude Risk Ratios
