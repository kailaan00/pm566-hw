hw02
================
Kaila An
2022-10-05

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(dplyr)
library(dtplyr)
library(ggplot2)
```

## Data Wrangling

Read in the data.

``` r
chsind <- data.table::fread("chs_individual.csv")
chsreg <- data.table::fread("chs_regional.csv")
```

Merge the data sets.

``` r
chs <- merge(chsind, chsreg, by="townname")
```

\#1. After merging the data, make sure you don’t have any duplicates by
counting the number of rows. Make sure it matches.

``` r
nrow(chsind)
```

    ## [1] 1200

``` r
nrow(chs)
```

    ## [1] 1200

There is no repeat data. The merged set and the original individual set
both have 1200 rows.

\#2. Create a new categorical variable named “obesity_level” using the
BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI
22-24; obese BMI\>24). To make sure the variable is rightly coded,
create a summary table that contains the minimum BMI, maximum BMI, and
the total number of observations per category.

``` r
chs$obesity_level <- as.factor (ifelse(chs$bmi < 14, 'underweight',
                                ifelse(chs$bmi < 22, 'normal', 
                                ifelse(chs$bmi < 24, 'overweight','obese' ))))
```

``` r
summary <- chs[, .(
  minimum = min(bmi),
  maximum = max(bmi),
  count   = n
)]
```

\#3. Create another categorical variable named “smoke_gas_exposure” that
summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have
four categories in total.

``` r
chs[ , smoke_gas_exposure := fifelse(smoke == 0 & gasstove == 0, "Neither",
                            fifelse(smoke == 1 & gasstove == 0, "smoke_only",
                            fifelse(smoke == 0 & gasstove == 1, "gas_only",
                            fifelse(smoke == 1 & gasstove == 1, "both", NA_character_))))]

chs[ ,.(total = length(smoke)), by = smoke_gas_exposure]
```

    ##    smoke_gas_exposure total
    ## 1:            Neither   214
    ## 2:               <NA>    60
    ## 3:         smoke_only    36
    ## 4:           gas_only   739
    ## 5:               both   151

\#4. Create four summary tables showing the average (or proportion, if
binary) and sd of “Forced expiratory volume in 1 second (ml)” and asthma
indicator by town, sex, obesity level, and “smoke_gas_exposure.”

## Looking at the Data (EDA)

Primary Questions of Interest

\#1. What is the association between BMI and FEV (forced expiratory
volume)?

\#2. What is the association between smoke and gas exposure and FEV?

\#3. What is the association between PM2.5 exposure and FEV?

Follow the EDA checklist from week 3 and the previous assignment. Be
sure to focus on the key variables. Visualization Create the following
figures and interpret them. Be sure to include easily understandable
axes, titles, and legends.

\#1. Facet plot showing scatterplots with regression lines of BMI vs FEV
by “townname”.

\#2. Stacked histograms of FEV by BMI category and FEV by smoke/gas
exposure. Use different color schemes than the ggplot default.

\#3. Barchart of BMI by smoke/gas exposure.

\#4. Statistical summary graphs of FEV by BMI and FEV by smoke/gas
exposure category.

\#5. A leaflet map showing the concentrations of PM2.5 mass in each of
the CHS communities.

\#6. Choose a visualization to examine whether PM2.5 mass is associated
with FEV.
