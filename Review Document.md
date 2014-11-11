---
title: "Overview document"
output: html_document
toc: True
---


##Libraries Used

```r
library(gmodels)
library(PSYC201)
library(ggplot2)
library(dplyr)
library(magrittr)
library(moments)
```
\pagebreak


#Univariate descriptive Statistics

##Definition of moments.

Point of clarification: Moments refers just the the set of numbers that describe a set of points, it makes sense in a physical system or to probability. They are defined according to the following function. 

$$\mu'_i = \int^b_a (x-c)^i f(x) dx$$

For probability $f(x)=p(x)$ so :

$$\mu'_i = \int^b_a (x-c)^i p(x) dx$$

As $i$ increases, you move up moments. The total number of moments as $i$ goes to $\inf$ completely describes a set of points. *Seems like you need might only need n moments, but what do I know*. In general, each moment is referred to as $\mu_i$, my guess is that using $\mu$ to represent mean is a convention to drop the 1. 



Zeroeth Moment- the probability mass function   
 
 $M_0 = \mu_0 = \Omega = 1$ 
 
First moment, the mean
  
\(M_1 = \mu_1 = \mu_{x} = \int^b_a (x-c)^1 f(x) dx =\int^b_a x f(x) dx  = E[X] \)     

Second Moment, variance. Note that $\sigma$ is the standard deviation.  
 
\(M_2 = \mu_2 = \sigma^2_x = \int^b_a (x-c)^2 f(x) dx  = E[(X-E[X])^2] = E[X^2] - E[X]^2 \)

Confirming with R

```r
# need to adjust for sample calculation by subtracting 0, but not identitcal
x <- rnorm(200, 0, 4)
var(x)
```

```
## [1] 18.08484
```

```r
sum((x^2))/(length(x) - 1) - mean(x)^2
```

```
## [1] 18.08489
```


This continues for the definition of moment, but using these in an non-normalized fashion doesn't appear normal


Third moment

\(M_3 = \mu_3 = \int^b_a (x-c)^3 f(x) dx \)  

Forth Moment

\(M_4 = \mu_4 = \int^b_a (x-c)^4 f(x) dx \)  

At this point, there now are what called "normal moments", where the are now "normliazed" by dividing by the  standard deviation. Now the $i_{th}$ moment is given by

$$\dfrac{\mu_i}{\sigma^i}$$

The third normalized or standardized moment is called Pearson moment coefficient of skeweness.


\(M_3 = \gamma = \dfrac{\mu_3}{\sigma^3} = E[ \left(\dfrac{s-\mu_1}{\sigma}\right)^3] = E[ \left(\dfrac{(E[(X-\mu)^3]}{E[(X-
mu)^2]^\frac{3}{2}}\right) \)

Confirming with r

```r
x <- rgamma(200, shape = 10)
skewness(x)
```

```
## [1] 0.512501
```

```r
mean(mean((x - mean(x))^3)/(mean((x - mean(x))^2)^(3/2)))
```

```
## [1] 0.512501
```

Forth Moment, kurtosis.

\(M_4 = \beta_2=\dfrac{\mu_4}{\sigma^4}  = E[ \left(\dfrac{(E[(X-\mu)^4]}{E[(X-
\mu)^2]^2}\right)]   \) 

Confirming with r

```r
x <- rnorm(200)
kurtosis(x)
```

```
## [1] 2.761481
```

```r
mean(mean((x - mean(x))^4)/(mean((x - mean(x))^2)^(2)))
```

```
## [1] 2.761481
```


However, because the kurtosis is equal to three in a normal distribution, is is often subtracted from the measure of kurtosis so that the sign of the value indictes whether a distribution is platykurtic (less than 0, low kurtosis) or leptokurtic ( greater than 0, high kurtosis). It is simultaneously a measure of three values. It generally indcreases with peakedness, how much variance is right next to the mean and tails and the tail thickness, but decreases with increased mass held in the sholders, or more modest deviations. However, it generally is seen as mostly a measure of tail thickness. 

\(M_4 = \beta_2=\dfrac{\mu_3}{\sigma^3}  = E[ \left(\dfrac{(E[(X-\mu)^4]}{E[(X-
mu)^2]^2}\right) -3]   \) 


##Central Limit theorem. 

Proof by assertion. It's an interesting idea. What happens when you start to sum n, iid variables. This is interesting as it assumes it observation is itself a draw from an independent distribution (which i guess is restating it's assumption). If you know the nature of the underlying distribution, you can calculate your expectation about the mean, variance, skew and kurtosis of the sum of the variables. 

Rules for adding or summing variables. 

$Mean(X+Y) = Mean(X) + Mean(Y)$  
$Mean(aX) = a * Mean(X)$  
$Var(X+Y) = Var(X) + Var(Y)$  
$Var(aX) = a^2 * Var(X)$  

Question 

Var(X+Y) = Var(X) + Var (Y)
if Y = X 
why does Var(2*X) =! Var(X + X) not clear, I guess different quantaties

This means that if one starts to sum iid varables, n increases linearly. However, by assertion the kurtosis and skew start to fall towards zero, meaning that distribution of the sum of iid variables starts to look like a normal distribution. Apprently they increase like this, regardless of the initial distribution. 

$skew(\sum^n X) = \frac{1}{\sqrt{n}} skew(x)$

$kurtosis(\sum^n X) = \frac{1}{n} kurtosis(x)$

This more or less underlies most statistics. There is the assumption that as you increase n, the expectation about the sum of the added random variables starts to resemble a normal variable (though normally you divide this value by n). This is 


## Normal variable 

Here X is distributed according to the following function.

$P(x|\mu,\sigma) = \dfrac{1}{\sqrt{2 \pi \sigma^2}} e^{ \left[- \dfrac{x-\mu}{2\sigma^2} \right]}$

Check in R

```r
gauss <- function(x, mu, sigma){
  const <- 1 / sqrt( 2 * pi * sigma^2)
  probability <- const * exp( - (x-mu)^2 / (2 * sigma^2) )
  return(probability)
} 

x<-1:600/100 - 3

qplot(x,gauss(x,0,1))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
qplot(x,dnorm(x))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) 


## Law of large numbers 

If you have now taken the sum of this set of random variables, you can now get the expected value of the mean and variance by diving by n. I think this means that there is a careful set


## Multivariate extension




##Calculating Sample Variance



##Combining variances.


  

 







Shape - skewness kurtosis

-How do you calculate these, can you recognize them in a graph.




##Reading and estimating from graphs


# Probability Distributions.



##pdf,cdf, icdf   

Here are the r commands, same for continuous and discrete probability density functions.   


![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-3.png) 

 -Not refer to law of total probability in the answers, particularly as numerator in bayes equation. 
 
 
#Random Variables and Expectation


#Null Hypothesis Sampling Testing.
