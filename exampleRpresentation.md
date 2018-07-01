# exampleRpresentation
Bioinformatics for Big Omics Data: Advanced data manipulation
========================================================
width: 1440
height: 900
transition: none
font-family: 'Helvetica'
css: my_style.css
author: Raphael Gottardo, PhD
date: July 25, 2014

<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/deed.en_US"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a><br /><tiny>This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/deed.en_US">Creative Commons Attribution-ShareAlike 3.0 Unported License</tiny></a>.

Here is a simple change
==========

Motivation
==========

Let's first turn on the cache for increased performance.

```r
library(knitr)
# Set some global knitr options
opts_chunk$set(cache=TRUE)
```


- R has pass-by-value semantics, which minimizes accidental side effects. However, this can become a major bottleneck when dealing with large datasets both in terms of memory and speed.

- Working with `data.frame`s in R can also be painful process in terms of writing code.

- Fortunately, R provides some solution to this problems.


---------


```r
a <- rnorm(10^5)
gc()
```

```
         used (Mb) gc trigger (Mb) max used (Mb)
Ncells 248916 13.3     407500 21.8   350000 18.7
Vcells 563935  4.4    1031040  7.9   857689  6.6
```

```r
b <- a
gc()
```

```
         used (Mb) gc trigger (Mb) max used (Mb)
Ncells 249045 13.4     467875 25.0   350000 18.7
Vcells 564109  4.4    1031040  7.9   857689  6.6
```

```r
a[1] <- 0
gc()
```

```
         used (Mb) gc trigger (Mb) max used (Mb)
Ncells 249062 13.4     467875 25.0   350000 18.7
Vcells 664138  5.1    1162592  8.9   857689  6.6
```


Overview
========

Here we will review three R packages that can be used to provide efficient data manipulation:

- `data.table`: An package for efficient data storage and manipulation
- `RSQLite`: Database Interface R driver for SQLite
- `sqldf`: An R package for runing SQL statements on R data frames, optimized for convenience

<small>Thank to Kevin Ushey (@kevin_ushey) for the `data.table` notes and Matt Dowle and Arunkumar Srinivasan for helpful comments.</small>

What is data.table?
===================

`data.table` is an R package that extends. `R` `data.frame`s.

Under the hood, they are just `data.frame's, with some extra 'stuff' added on.
So, they're collections of equal-length vectors. Each vector can be of
different type.


```r
library(data.table)
dt <- data.table(x=1:3, y=c(4, 5, 6), z=letters[1:3])
dt
```

```
   x y z
1: 1 4 a
2: 2 5 b
3: 3 6 c
```

```r
class(dt)
```

```
[1] "data.table" "data.frame"
```

The extra functionality offered by `data.table` allows us to modify, reshape, 
and merge `data.table`s much quicker than `data.frame`s. **See that `data.table` inherits from `data.frame`!**

**Note:** The [development version (v1.8.11)](https://r-forge.r-project.org/scm/viewvc.php/pkg/NEWS?view=markup&root=datatable) of data.table includes a lot of new features including `melt` and `dcast` methods


Installing data.table
=====================

- stable CRAN release
    
    

```r
install.packages("data.table")
```
- latest bug-fixes + enhancements

```r
install.packages("data.table", repos="http://R-forge.R-project.org")
```


What's different?
=================

Most of your interactions with `data.table`s will be through the subset (`[`)
operator, which behaves quite differently for `data.table`s. We'll examine
a few of the common cases.

Visit [this stackoverflow question](http://stackoverflow.com/questions/13618488/what-you-can-do-with-data-frame-that-you-cant-in-data-table) for a summary of the differences between `data.frame`s and `data.table`s.

Single element subsetting
=========================


```r
library(data.table)
DF <- data.frame(x=1:3, y=4:6, z=7:9)
DT <- data.table(x=1:3, y=4:6, z=7:9)
DF[c(2,3)]
```

```
  y z
1 4 7
2 5 8
3 6 9
```

```r
DT[c(2,3)]
```

```
   x y z
1: 2 5 8
2: 3 6 9
```

By default, single-element subsetting in `data.table`s refers to rows, rather
than columns.

Row subsetting
===============


```r
library(data.table)
DF <- data.frame(x=1:3, y=4:6, z=7:9)
DT <- data.table(x=1:3, y=4:6, z=7:9)
DF[c(2,3), ]
```

```
  x y z
2 2 5 8
3 3 6 9
```

```r
DT[c(2,3), ]
```

```
   x y z
1: 2 5 8
2: 3 6 9
```

Notice: row names are lost with `data.table`s. Otherwise, output is identical.

Column subsetting
=================


```r
library(data.table)
DF <- data.frame(x=1:3, y=4:6, z=7:9)
DT <- data.table(x=1:3, y=4:6, z=7:9)
DF[, c(2,3)]
```

```
  y z
1 4 7
2 5 8
3 6 9
```

```r
DT[, c(2,3)]
```

```
[1] 2 3
```

`DT[, c(2,3)]` just returns `c(2, 3)`. Why on earth is that?

The j expression
================

The subset operator is really a function, and `data.table` modifies it to behave
differently.

Call the arguments we pass e.g. `DT[i, j]`, or `DT[i]`.

The second argument to `[` is called the `j expression`, so-called because it's
interpreted as an `R` expression. This is where most of the `data.table`
magic happens.

`j` is an expression evaluated within the frame of the `data.table`, so
it sees the column names of `DT`. Similarly for `i`.

First, let's remind ourselves what an `R` expression is.


Expressions
===========

An `expression` is a collection of statements, enclosed in a block generated by
braces `{}`.


```r
## an expression with two statements
{
  x <- 1
  y <- 2
}
## the last statement in an expression is returned
k <- { print(10); 5 }
```

```
[1] 10
```

```r
print(k)
```

```
[1] 5
```

The j expression (suite)
================

So, `data.table` does something special with the `expression` that you pass as
`j`, the second argument, to the subsetting (`[`) operator.

The return type of the final statement in our expression determines the type
of operation that will be performed.

In general, the output should either be a `list` of symbols, or a statement
using `:=`.

We'll start by looking at the `list` of symbols as an output.

An example
==========

When we simply provide a `list` to the `j expression`, we generate a new
`data.table` as output, with operations as performed within the `list` call.


```r
library(data.table)
DT <- data.table(x=1:5, y=1:5)
DT[, list(mean_x = mean(x), sum_y = sum(y), sumsq=sum(x^2+y^2))]
```

```
   mean_x sum_y sumsq
1:      3    15   110
```

Notice how the symbols `x` and `y` are looked up within the `data.table` `DT`.
No more writing `DT$` everywhere!


Using :=
=========
Using the `:=` operator tells us we should assign columns by reference into
the `data.table` `DT`:


```r
library(data.table)
DT <- data.table(x=1:5)
DT[, y := x^2]
```

```
   x  y
1: 1  1
2: 2  4
3: 3  9
4: 4 16
5: 5 25
```

```r
print(DT)
```

```
   x  y
1: 1  1
2: 2  4
3: 3  9
4: 4 16
5: 5 25
```

Using := (suite)
================
By default, `data.table`s are not copied on a direct assignment `<-`:


```r
library(data.table)
DT <- data.table(x=1)
DT2 <- DT
DT[, y := 2]
```

```
   x y
1: 1 2
```

```r
DT2
```

```
   x y
1: 1 2
```

Notice that `DT2` has changed. This is something to be mindful of; if you want
to explicitly copy a `data.table` do so with `DT2 <- copy(DT)`.

A slightly more complicated example
===================================


```r
library(data.table)
DT <- data.table(x=1:5, y=6:10, z=11:15)
DT[, m := log2( (x+1) / (y+1) )]
```

```
   x  y  z       m
1: 1  6 11 -1.8074
2: 2  7 12 -1.4150
3: 3  8 13 -1.1699
4: 4  9 14 -1.0000
5: 5 10 15 -0.8745
```

```r
print(DT)
```

```
   x  y  z       m
1: 1  6 11 -1.8074
2: 2  7 12 -1.4150
3: 3  8 13 -1.1699
4: 4  9 14 -1.0000
5: 5 10 15 -0.8745
```


Using an expression in j
========================

Note that the right-hand side of a `:=` call can be an expression.


```r
library(data.table)
DT <- data.table(x=1:5, y=6:10, z=11:15)
DT[, m := { tmp <- (x + 1) / (y + 1); log2(tmp) }]
```

```
   x  y  z       m
1: 1  6 11 -1.8074
2: 2  7 12 -1.4150
3: 3  8 13 -1.1699
4: 4  9 14 -1.0000
5: 5 10 15 -0.8745
```

```r
print(DT)
```

```
   x  y  z       m
1: 1  6 11 -1.8074
2: 2  7 12 -1.4150
3: 3  8 13 -1.1699
4: 4  9 14 -1.0000
5: 5 10 15 -0.8745
```

Multiple returns from an expression in j
========================================

The left hand side of a `:=` call can also be a character vector of names,
for which the corresponding final statement in the `j expression` should be
a list of the same length.


```r
library(data.table)
DT <- data.table(x=1:5, y=6:10, z=11:15)
DT[, c('m', 'n') := { tmp <- (x + 1) / (y + 1); list( log2(tmp), log10(tmp) ) }]
```

```
   x  y  z       m       n
1: 1  6 11 -1.8074 -0.5441
2: 2  7 12 -1.4150 -0.4260
3: 3  8 13 -1.1699 -0.3522
4: 4  9 14 -1.0000 -0.3010
5: 5 10 15 -0.8745 -0.2632
```

```r
DT[, `:=`(a=x^2, b=y^2)]
```

```
   x  y  z       m       n  a   b
1: 1  6 11 -1.8074 -0.5441  1  36
2: 2  7 12 -1.4150 -0.4260  4  49
3: 3  8 13 -1.1699 -0.3522  9  64
4: 4  9 14 -1.0000 -0.3010 16  81
5: 5 10 15 -0.8745 -0.2632 25 100
```

```r
DT[, c("c","d"):=list(x^2, y^2)]
```

```
   x  y  z       m       n  a   b  c   d
1: 1  6 11 -1.8074 -0.5441  1  36  1  36
2: 2  7 12 -1.4150 -0.4260  4  49  4  49
3: 3  8 13 -1.1699 -0.3522  9  64  9  64
4: 4  9 14 -1.0000 -0.3010 16  81 16  81
5: 5 10 15 -0.8745 -0.2632 25 100 25 100
```

The j expression revisited 
===============

So, we typically call `j` the `j expression`, but really, it's either:

1. An expression, or

2. A call to the function `:=`, for which the first argument is a set of
names (vectors to update), and the second argument is an expression, with
the final statement typically being a list of results to assign within
the `data.table`.

As I said before, `a := b` is parsed by `R` as `":="(a, b)`, hence it
looking somewhat like an operator.


```r
quote(a := b)
```

```
`:=`(a, b)
```

Why does it matter?
===================

Whenever you sub-assign a `data.frame`, `R` is forced to copy the entire
`data.frame`.

That is, whenever you write `DF$x <- 1`, `DF["x"] <- 1`, `DF[["x"]] <- 1`...

... R will make a copy of `DF` before assignment.

This is done in order to ensure any other symbols pointing at the same object
do not get modified. This is a good thing for when we need to reason about
the code we write, since, in general, we expect `R` to operate without side 
effects.

Unfortunately, it is prohibitively slow for large objects, and hence why
`:=` can be very useful.

Why does it matter?
===================










































































```
Error in library(microbenchmark) : 
  there is no package called 'microbenchmark'
```
