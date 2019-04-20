### <p align="center"> Monotonic Optimal Binning (MOB) </p>
### <p align="center">  for Consumer Credit Risk Scorecard Development </p>

The **MOB** (Monotonic Optimal Binning) package is a collection of R functions that would generate the monotonic binning and perform the WoE (Weight of Evidence) transformation used in consumer credit scorecard developments. Being a piecewise constant transformation in the context of logistic regressions, the WoE has also been employed in other use cases, such as consumer credit loss estimation, prepayment, and even fraud detection models.  


#### Package Dependencies
parallel, Hmisc, gbm


#### Features
* #### Monotonic Binning

The function **manual_bin()** is the building block of all monotonic binning algorithms included in the MOB package and will generate the binning outcome based upon a list of cutting points for each numeric attribute. If there is any missing value in the attribute, two different treatments should be considered, depending on the ratio between good and bad accounts. First of all, with only good or only bad accounts in missing values, all cases should be merged into a category with the similar bad rate. Secondly, with both good and bad acounts in missing values, all cases should be treated as a standalone category. In the example shown below, there are 213 cases with missing values in the attribute named "tot_derog" with 70 bad and the rest good accounts. Therefore, all 213 cases are assigned into a separate category with the 32.86% bad rate and the value of WoE = 0.6416. 

```r
manual_bin(df, "bad", "tot_derog", c(1, 2))
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#    01                        $X <= 1 3741 0.6409      0      560   0.1497 -0.3811 0.0828 18.9469
#    02               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#    03                         $X > 2 1405 0.2407      0      445   0.3167  0.5871 0.0970  0.0000

```

Currently, there are four monotonic binning algorithms supported in the **MOB** package. 

1. The function **qtl_bin()** implements the iterative discretization based on quantiles across the whole data sample. In each step, a convergence criterion of the Spearman Correlation Coefficient equal to 1 or -1 would be evaluated. 
```r
qtl_bin(df, bad, tot_derog)
# $df
#  bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#   00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#   01                        $X <= 1 3741 0.6409      0      560   0.1497 -0.3811 0.0828 18.9469
#   02               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#   03               $X > 2 & $X <= 4  587 0.1006      0      176   0.2998  0.5078 0.0298 10.6623
#   04                         $X > 4  818 0.1401      0      269   0.3289  0.6426 0.0685  0.0000
# $cuts
# [1] 1 2 4
```

2. The function **bad_bin()** is a two-step process and implements a revised version of the iterative discretization. The discretization is determined by the data sample with Y = 1 and the discretization outcome is evaluated across the whole data sample based upon the abovementioned convergence criterion. Compared with **qtl_bin()**, **bad_bin()** would tend to generate a more coarse binning outcome and therefore lower statistics of KS or IV. 
```r
bad_bin(df, bad, tot_derog)
# $df
#  bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#   00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#   01                        $X <= 2 4219 0.7228      0      681   0.1614 -0.2918 0.0563 16.5222
#   02               $X > 2 & $X <= 4  587 0.1006      0      176   0.2998  0.5078 0.0298 10.6623
#   03                         $X > 4  818 0.1401      0      269   0.3289  0.6426 0.0685  0.0000
# $cuts
# [1] 2 4
```

3. The function **iso_bin()** is driven by the isotonic regression and therefore doesn't require the iteration. As a result, the process is much faster and a monotonic outcome is guaranteed. In addition, given a more granular binning outcome, **iso_bin()** is able to deliver results with higher KS and IV. 
```r
iso_bin(df, bad, tot_derog)
# $df
#  bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#   00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#   01                        $X <= 0 2850 0.4883      0      367   0.1288 -0.5559 0.1268 20.0442
#   02               $X > 0 & $X <= 1  891 0.1526      0      193   0.2166  0.0704 0.0008 18.9469
#   03               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#   04               $X > 2 & $X <= 3  332 0.0569      0       86   0.2590  0.3050 0.0058 14.6321
#   05              $X > 3 & $X <= 23 1064 0.1823      0      353   0.3318  0.6557 0.0931  0.4370
#   06                        $X > 23    9 0.0015      0        6   0.6667  2.0491 0.0090  0.0000
# $cuts
# [1]  0  1  2  3 23
```

4. The function **gbm_bin()** is driven by the generalized boosted modeling with the monotone restriction. Similar to **iso_bin()**, it also would generate more granular binning outcomes with higher KS and IV statistics, albeit a longer computing time due to the boosting process. 
```r
gbm_bin(df, bad, tot_derog)
# $df
#  bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#   00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#   01                        $X <= 0 2850 0.4883      0      367   0.1288 -0.5559 0.1268 20.0442
#   02               $X > 0 & $X <= 1  891 0.1526      0      193   0.2166  0.0704 0.0008 18.9469
#   03               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#   04               $X > 2 & $X <= 3  332 0.0569      0       86   0.2590  0.3050 0.0058 14.6321
#   05               $X > 3 & $X <= 9  848 0.1453      0      282   0.3325  0.6593 0.0750  3.2492
#   06              $X > 9 & $X <= 14  166 0.0284      0       56   0.3373  0.6808 0.0157  0.9371
#   07                        $X > 14   59 0.0101      0       21   0.3559  0.7629 0.0071  0.0000
# $cuts
# [1]  0  1  2  3  9 14
```

As previously demonstrated, above 4 functions can only apply the binning algorithm to 1 independent variable at a time. In order to make them useful in a real-world production environment, we need a wrapper that would allow us to run through hundreds or even thousands of independent variables at once. The function **batch_bin()** takes two input parameters, "data" for a data frame with the last column as the dependent variable and "method" for an integer indicating the binning algorithm to be called for. For instance, **_batch_bin(df, 1)_** will apply the **qtl_bin()** function to all (numeric) independent varaibles in a data frame "df" and generates a summary output as below.
```r
|var            |  nbin|  unique|  miss|  min|   median|      max|       ks|      iv|
|:--------------|-----:|-------:|-----:|----:|--------:|--------:|--------:|-------:|
|tot_derog      |     5|      29|   213|    0|      0.0|       32|  18.9469|  0.2055|
|tot_tr         |     5|      67|   213|    0|     16.0|       77|  15.7052|  0.1302|
|age_oldest_tr  |    10|     460|   216|    1|    137.0|      588|  19.9821|  0.2539|
|tot_open_tr    |     3|      26|  1416|    0|      5.0|       26|   6.7157|  0.0240|
|tot_rev_tr     |     3|      21|   636|    0|      3.0|       24|   9.0104|  0.0717|
|tot_rev_debt   |     3|    3880|   477|    0|   3009.5|    96260|   8.5102|  0.0627|
|tot_rev_line   |     9|    3617|   477|    0|  10573.0|   205395|  26.4924|  0.4077|
|rev_util       |     2|     101|     0|    0|     30.0|      100|  15.1570|  0.0930|
|bureau_score   |    12|     315|   315|  443|    692.5|      848|  34.8028|  0.7785|
|ltv            |     7|     145|     1|    0|    100.0|      176|  15.6254|  0.1538|
|tot_income     |     4|    1639|     5|    0|   3400.0|  8147167|   9.1526|  0.0500|
```

If we are specifically interested in the detailed binning outcome of the attribute "tot_income", then **_batch_bin(df, 1)$BinLst[["tot_income"]]_** would deliver the result as below, which is the identical output from **_qtl_bin(df, bad, tot_income)_**.
```r
# $df
#  bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv     ks
#   00                      is.na($X)    5 0.0009      5        1   0.2000 -0.0303 0.0000 0.0026
#   01                     $X <= 2570 1947 0.3336      0      486   0.2496  0.2553 0.0234 9.1526
#   02         $X > 2570 & $X <= 4510 1995 0.3418      0      406   0.2035 -0.0086 0.0000 8.8608
#   03                      $X > 4510 1890 0.3238      0      303   0.1603 -0.2999 0.0266 0.0000
# $cuts
# [1] 2570 4510
```
