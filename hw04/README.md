hw04
================
Kaila An
2022-11-15

``` r
library(parallel)
```

## HPC

# Problem 1. Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  answer <- rowSums(mat)
  answer
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  answer2 <- t(apply(mat, 1, cumsum))
  answer2
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "nanoseconds", check = "equivalent"
)
```

    ## Unit: nanoseconds
    ##          expr     min      lq    mean  median      uq     max neval
    ##     fun1(dat) 2617251 2671709 2714382 2696355 2740396 2916750   100
    ##  fun1alt(dat) 1514793 1524209 1564101 1534396 1564730 3051126   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "nanoseconds", check = "equivalent"
)
```

    ## Unit: nanoseconds
    ##          expr     min      lq    mean  median      uq     max neval
    ##     fun2(dat) 2603917 2649750 2694107 2676105 2728396 2883709   100
    ##  fun2alt(dat) 1634209 1754896 2003146 1875855 1962376 8492750   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

# Problem 2: Make things run faster with parallel computing

``` r
# simulating PI

sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   9.471   0.106   9.588

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
cl <- makePSOCKcluster(4L)
clusterSetRNGStream(cl = cl , iseed = 1231)
system.time({
  answer <- unlist(parLapply(cl = cl, rep(4000,10000),sim_pi))
  print(mean(answer))
  stopCluster(cl)
  answer
})
```

    ## [1] 3.141521

    ##    user  system elapsed 
    ##   0.007   0.001   2.587

## SQL

``` r
# setup a temporary database

# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

# Question 1. How many many movies is there avaliable in each rating catagory.

# Question 2. What is the average replacement cost and rental rate for each rating category.

# Question 3. Use table film_category together with film to find the how many films there are witth each category ID

# Question 4. Incorporate table category into the answer to the previous question to find the name of the most popular category.
