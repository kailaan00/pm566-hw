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
    ##     fun1(dat) 2629792 2665250 2706464 2689396 2737918 2851334   100
    ##  fun1alt(dat) 1515542 1523064 1555609 1534876 1568084 2615042   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "nanoseconds", check = "equivalent"
)
```

    ## Unit: nanoseconds
    ##          expr     min      lq    mean  median      uq     max neval
    ##     fun2(dat) 2646959 2691522 2760829 2750522 2826605 2979084   100
    ##  fun2alt(dat) 1639834 1781334 2005790 1895188 1943480 8092667   100

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
    ##   9.464   0.083   9.553

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
    ##   0.008   0.000   2.550

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

``` sql
select rating, count(*) as '# Movies'
from film
group by rating
```

| rating | \# Movies |
|:-------|----------:|
| G      |       180 |
| NC-17  |       210 |
| PG     |       194 |
| PG-13  |       223 |
| R      |       195 |

5 records

# Question 2. What is the average replacement cost and rental rate for each rating category.

``` sql
select rating,
  avg(replacement_cost) as 'average replacement cost',
  avg(rental_rate) as 'average rental rate'
  from film
  group by rating
```

| rating | average replacement cost | average rental rate |
|:-------|-------------------------:|--------------------:|
| G      |                 20.12333 |            2.912222 |
| NC-17  |                 20.13762 |            2.970952 |
| PG     |                 18.95907 |            3.051856 |
| PG-13  |                 20.40256 |            3.034843 |
| R      |                 20.23103 |            2.938718 |

5 records

# Question 3. Use table film_category together with film to find the how many films there are with each category ID

``` sql
select f.category_id, count(f.category_id) as 'number of films'
  from film_category 
  as f
  inner join film 
  as g on f.film_id = g.film_id
  group by f.category_id
```

| category_id | number of films |
|:------------|----------------:|
| 1           |              64 |
| 2           |              66 |
| 3           |              60 |
| 4           |              57 |
| 5           |              58 |
| 6           |              68 |
| 7           |              62 |
| 8           |              69 |
| 9           |              73 |
| 10          |              61 |

Displaying records 1 - 10

# Question 4. Incorporate table category into the answer to the previous question to find the name of the most popular category.

``` sql
select f.category_id, h.name, count(f.category_id) 
    as 'number of films'
    from film_category 
    as f
    inner join film 
    as g on f.film_id = g.film_id
    inner join category 
    as h on f.category_id = h.category_id
    group by f.category_id
    order by 'number of films' desc
```

| category_id | name    | number of films |
|------------:|:--------|----------------:|
|          16 | Travel  |              57 |
|          15 | Sports  |              74 |
|          14 | Sci-Fi  |              61 |
|          13 | New     |              63 |
|          12 | Music   |              51 |
|          11 | Horror  |              56 |
|          10 | Games   |              61 |
|           9 | Foreign |              73 |
|           8 | Family  |              69 |
|           7 | Drama   |              62 |

Displaying records 1 - 10

The name of the most popular category is ‘Sports’.
