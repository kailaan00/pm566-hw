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
chs[ , bmi_imp := fcoalesce(bmi, mean(bmi, na.rm = TRUE)),
by = . (hispanic, male)]
```

``` r
chs[, .(
  minimum = min(bmi),
  maximum = max(bmi),
  count   = .N
), by = c("obesity_level")]
```

    ##    obesity_level  minimum  maximum count
    ## 1:        normal 14.00380 21.96387   886
    ## 2:    overweight 22.02353 23.99650    87
    ## 3:         obese 24.00647 41.26613   103
    ## 4:          <NA>       NA       NA    89
    ## 5:   underweight 11.29640 13.98601    35

\#3. Create another categorical variable named “smoke_gas_exposure” that
summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have
four categories in total.

``` r
chs[ , smoke_gas_exposure := ifelse(smoke == 0 & gasstove == 0, "Neither",
                             ifelse(smoke == 1 & gasstove == 0, "smoke_only",
                             ifelse(smoke == 0 & gasstove == 1, "gas_only",
                             ifelse(smoke == 1 & gasstove == 1, "both", NA_character_))))]

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

``` r
chs[,.(
   mean = mean(fev, na.rm=TRUE),
   sd   = sd(fev, na.rm=TRUE)
), by   = townname][order(townname)]
```

    ##          townname     mean       sd
    ##  1:        Alpine 2089.014 298.2039
    ##  2:    Atascadero 2079.374 331.8782
    ##  3: Lake Elsinore 2039.787 317.6112
    ##  4:  Lake Gregory 2091.665 337.8286
    ##  5:     Lancaster 2002.550 337.1053
    ##  6:        Lompoc 2038.227 367.4474
    ##  7:    Long Beach 1983.896 330.6271
    ##  8:     Mira Loma 1984.726 336.6416
    ##  9:     Riverside 1986.212 289.7415
    ## 10:     San Dimas 2027.806 321.9740
    ## 11:   Santa Maria 2022.553 330.0457
    ## 12:        Upland 2027.284 357.2010

``` r
chs[,.(
   mean = mean(fev, na.rm=TRUE),
   sd   = sd(fev, na.rm=TRUE)
), by   = male][order(male)]
```

    ##    male     mean       sd
    ## 1:    0 1959.105 327.2948
    ## 2:    1 2103.819 318.2036

``` r
chs[,.(
   mean = mean(fev, na.rm=TRUE),
   sd   = sd(fev, na.rm=TRUE)
), by   = obesity_level][order(obesity_level)]
```

    ##    obesity_level     mean       sd
    ## 1:        normal 1997.974 309.4085
    ## 2:         obese 2269.295 325.5054
    ## 3:    overweight 2224.322 317.4261
    ## 4:   underweight 1686.800 300.0803
    ## 5:          <NA>      NaN       NA

``` r
chs[,.(
   mean = mean(fev, na.rm=TRUE),
   sd   = sd(fev, na.rm=TRUE)
), by   = smoke_gas_exposure][order(smoke_gas_exposure)]
```

    ##    smoke_gas_exposure     mean       sd
    ## 1:            Neither 2059.943 342.5625
    ## 2:               both 2019.974 313.2327
    ## 3:           gas_only 2026.308 328.1240
    ## 4:         smoke_only 2064.346 333.2266
    ## 5:               <NA> 1999.783 364.9553

## Looking at the Data (EDA)

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

``` r
library(leaflet)
tem.pal <- colorFactor(topo.colors(2), domain = chs$pm25_mass)
leaflet(chs) %>% 
  # the looks of the map
  addTiles() %>%
  # some circles
  addCircles (color = ~tem.pal(pm25_mass),
               label = ~pm25_mass, 
               opacity=0.01, fillOpacity = 0.01, radius = 500) %>%
  # add a pretty legend
  addLegend('bottomleft', pal= tem.pal, values = chs$pm25_mass,
             title ='pm25_mass levels in CHS communities', opacity=1)
```

    ## Assuming "lon" and "lat" are longitude and latitude, respectively

![](README_files/figure-gfm/leaflet-1.png)<!-- -->

\#6. Choose a visualization to examine whether PM2.5 mass is associated
with FEV.

Primary Questions of Interest

\#1. What is the association between BMI and FEV (forced expiratory
volume)?

\#2. What is the association between smoke and gas exposure and FEV?

\#3. What is the association between PM2.5 exposure and FEV?
