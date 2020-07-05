<p align="center">
  <img width="150" height="100" src="https://github.com/statcompute/MonotonicBinning/blob/master/data/mob.jpeg">
</p>

### <p align="center"> Monotonic Optimal Binning (MOB) </p>
### <p align="center">  for Risk Scorecard Development </p>

#### Introduction

The **MOB** (Monotonic Optimal Binning) package is a collection of R functions that would generate the monotonic binning and perform the WoE (Weight of Evidence) transformation used in consumer credit scorecard developments. Being a piecewise constant transformation in the context of logistic regressions, the WoE has also been employed in other use cases, such as consumer credit loss estimation, prepayment, and even fraud detection models.  

In addition to monotonic binning and WoE transformation, Information Value and KS statistic of each independent variables are also calculated to evaluate the variable predictiveness. 


#### Why Use Weight of Evidence

I had been asked why I spent so much effort on writing SAS macros and R functions to do monotonic binning for the WoE transformation, given the availability of other cutting-edge data mining or deep learning algorithms that will automatically generate the prediction with whatever predictors fed in the model. About 10 years ago when I worked in the decision science team of Chase, I was once told that even an idiot knows how to put X on the right-hand side of an equal sign. Nonetheless, what really distinguishes a good modeler from the rest is how to handle challenging data issues, including missing values, outliers, linearity, and predictability, in a scalable way that can be rolled out to hundreds or even thousands of potential model drivers in the production environment. 

The WoE transformation through monotonic binning provides a convenient way to address each of aforementioned concerns. 

1. Because WoE is a piecewise transformation based on the data discretization, all missing values would fall into a standalone category either by itself or to be combined with the neighbor with a similar bad rate. As a result, the special treatment for missing values is not necessary. 

2. After the monotonic binning of each variable, since the WoE value for each bin is a projection from the predictor into the response that is defined by the log ratio between event and non-event distributions, any raw value of the predictor doesn’t matter anymore and therefore the issue related to outliers  would disappear. 

3. While many modelers would like to use log or power transformations to achieve a good linear relationship between the predictor and log odds of the response, which is heuristic at best with no guarantee for the good outcome, the WoE transformation is strictly linear with respect to log odds of the response with the unity correlation. It is also worth mentioning that a numeric variable and its strictly monotone functions should converge to the same monotonic WoE transformation. 

4. At last, because the WoE is defined as the log ratio between event and non-event distributions, it is indicative of the separation between cases with Y = 0 and cases with Y = 1. As the weighted sum of WoE values with the weight being the difference in event and non-event distributions, the IV (Information Value) is an important statistic commonly used to measure the predictor importance.


#### Package Dependencies

```text
R (>= 3.3.3), stats, gbm, Rborist
```

#### Installation

Download the [mob_0.2.0.tar.gz] (https://github.com/statcompute/mob/blob/master/mob_0.2.0.tar.gz) file and then run:

```r
install.packages("mob_0.2.0.tar.gz", repos = NULL, type = "source")
```

Alternatively, you can also install the package from CRAN directly by running:


```r
install.packages("mob")
```
