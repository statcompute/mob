#### <p align="center"> Monotonic Optimal Binning (MOB) </p>
#### <p align="center">  for Consumer Credit Risk Scorecard Development </p>

The **MOB** (Monotonic Optimal Binning) package is a collection of R functions that would generate the monotonic binning and perform the WoE (Weight of Evidence) transformation used in consumer credit scorecard developments. Being a piecewise constant transformation in the context of logistic regressions, the WoE has also been employed in other use cases, such as consumer credit loss estimation, prepayment, and even fraud detection models.  

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

1. The function **qtl_bin()** implements the iterative discretization based on quantiles across the whole data sample. In each iterative step, a convergence criterion of the Spearman Correlation Coefficient equal to 1 or -1 would be evaluated. 
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

2. The function **bad_bin()** is a two-step process and implements a revised version of the iterative discretization. The discretization is driven by the data sample with Y = 1 and the discretization outcome is evaluated across the whole data sample based upon the abovementioned convergence criterion. Compared with **qtl_bin()**, **bad_bin()** would tend to generate a more coarse binning outcome and therefore lower statistics of KS or IV. 
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
