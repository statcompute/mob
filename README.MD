#### <p align="center"> Monotonic Optimal Binning (MOB) </p>
##### <p align="center">  for Consumer Credit Risk Scorecard Development </p>

This reposity hosts a collection of R functions that would generate the monotonice binning employed in consumer credit scorecard developments. 

The function [manual_bin()](https://github.com/statcompute/MonotonicBinning/blob/master/code/manual_bin.R) is the building block of all binning algorithms introduced here and will generate the binning outcome based upon a list of cutting points for each attribute. 

```r
manual_bin(df, "bad", "tot_derog", c(1, 2))
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#    01                        $X <= 1 3741 0.6409      0      560   0.1497 -0.3811 0.0828 18.9469
#    02               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#    03                         $X > 2 1405 0.2407      0      445   0.3167  0.5871 0.0970  0.0000

```
