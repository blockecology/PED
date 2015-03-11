# Data Analysis for Block and Meave (201X, Plant Eco & Div)
Sebastian Block  

This report shows the data analysis done for the paper Block and Meave (201X), published in the journal Plant Ecology & Diversity.  
The analysis was reproduced for the last time on March 9th, 2015. I used R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet" and worked in a platform x86_64-apple-darwin13.4.0 (64-bit).

## Redundancy analysis

First, load the packages required for the analysis.


```r
library(ade4)
library(vegan)
```

```
## Loading required package: permute
## Loading required package: lattice
## This is vegan 2.2-0
## 
## Attaching package: 'vegan'
## 
## The following object is masked from 'package:ade4':
## 
##     cca
```

```r
library(MASS)
library(ellipse)
library(FactoMineR)
```

```
## 
## Attaching package: 'FactoMineR'
## 
## The following object is masked from 'package:ade4':
## 
##     reconst
```

To load the data, all files must be in the working directory. 



```r
structure <- read.csv("str.csv")
env <- read.csv("env.csv")
asp <- read.csv("aspect.csv")
```

I did some modifications to the data. First, I combined all the quantitative variables of the `env` data (i.e., all except the geomorphological unit) with the `asp` data into a single object called `env.quant`. Then, I normalized the data in `env.quant` to allow comparison between variables with very different units. Finally, I added the geomorphological unit column to the normalized quantitative environmental variables to create the data frame `env.ra`, which was used for the redundancy analysis. 


```r
env.quant <- cbind(env[,-5],asp)
env.z <- scale(env.quant)
unit <- env[,5]
env.ra <- data.frame(env.z,unit)
```

### Structural data

I did a redundancy analysis on vegetation structure using the environmental data `env.ra` as explanatory variables. I assigned the results to the object `str.rda`.


```r
str.rda <- rda(structure~., env.ra, scale=T)
summary(str.rda)
```

```
## 
## Call:
## rda(formula = structure ~ altitude + slope + rock + disturbance +      aspect + unit, data = env.ra, scale = T) 
## 
## Partitioning of correlations:
##               Inertia Proportion
## Total           4.000      1.000
## Constrained     1.028      0.257
## Unconstrained   2.972      0.743
## 
## Eigenvalues, and their contribution to the correlations 
## 
## Importance of components:
##                         RDA1    RDA2    RDA3    RDA4   PC1    PC2     PC3
## Eigenvalue            0.7970 0.14050 0.08005 0.01048 1.756 0.9250 0.18135
## Proportion Explained  0.1992 0.03512 0.02001 0.00262 0.439 0.2313 0.04534
## Cumulative Proportion 0.1992 0.23436 0.25438 0.25699 0.696 0.9273 0.97263
##                           PC4
## Eigenvalue            0.10948
## Proportion Explained  0.02737
## Cumulative Proportion 1.00000
## 
## Accumulated constrained eigenvalues
## Importance of components:
##                         RDA1   RDA2    RDA3    RDA4
## Eigenvalue            0.7970 0.1405 0.08005 0.01048
## Proportion Explained  0.7753 0.1367 0.07787 0.01019
## Cumulative Proportion 0.7753 0.9119 0.98981 1.00000
## 
## Scaling 2 for species and site scores
## * Species are scaled proportional to eigenvalues
## * Sites are unscaled: weighted dispersion equal on all dimensions
## * General scaling constant of scores:  3.919476 
## 
## 
## Species scores
## 
##        RDA1    RDA2     RDA3     RDA4      PC1      PC2
## ba  -0.9246  0.1831 -0.02910 -0.16244  1.63093 -0.08778
## cov -0.9044 -0.6180  0.07885  0.01387  1.43470  0.38987
## den -0.3918  0.2247  0.50312  0.03641 -0.04655  1.82720
## hei -1.1110  0.2714 -0.21738  0.11105  1.42278 -0.23274
## 
## 
## Site scores (weighted sums of species scores)
## 
##           RDA1      RDA2     RDA3     RDA4       PC1       PC2
## row1   0.22785  0.270852  2.93543  2.99485 -0.440562  0.691015
## row2   0.83904  0.438888 -0.53142  2.84411 -0.759571 -0.351583
## row3  -1.30198  1.170564  3.16299  1.65683  0.284619  1.128730
## row4   0.51936  0.503734 -0.37649  2.81833 -0.935897 -0.192376
## row5   0.39212 -0.484969 -2.11758  1.83449 -0.592485 -0.763339
## row6  -1.52395 -0.677108 -0.48953 -4.36253  0.589039  0.035292
## row7  -0.23288  0.871005  1.84044  3.50893 -0.548670  0.598587
## row8  -0.82872  0.698119 -1.63213 -1.49074 -0.049693 -0.295598
## row9  -3.19451  1.823282 -5.33611 -0.57433  1.713923 -0.896904
## row10 -1.66047  0.044759 -0.36296 -6.08025  0.739296  0.046176
## row11  0.74386  0.321653 -0.81193  0.46492 -0.238842 -0.599955
## row12  0.02928  0.938983  2.76807 -0.95225 -0.210929  0.652865
## row13 -0.49777  0.166082  2.19364  0.12887  0.248290  0.562664
## row14 -0.35294  0.077719  3.97498  1.43795  0.224860  1.009485
## row15  0.43781  0.620014 -1.26618  1.60703 -0.192672 -0.531850
## row16 -0.62458  0.144980 -1.01823  1.59121  0.470039 -0.317093
## row17 -0.15330  0.301143 -0.74566 -4.02228  0.038103 -0.363888
## row18  0.21503 -0.238104 -0.37917 -1.79550 -0.035170 -0.384009
## row19 -0.21140  0.049961  2.81896  1.24119 -0.035303  0.669403
## row20  0.48369 -0.357933 -1.39662 -1.97098 -0.268375 -0.697621
## row21  0.40529  0.118357 -0.46167 -0.80829 -0.273158 -0.369210
## row22 -0.73699 -0.487500 -1.28856  2.06542  0.129132  0.123463
## row23  0.98976  0.520984  0.09964  1.19454 -0.822201 -0.233775
## row24 -0.17926  0.974587  0.27170  1.30371  0.003687  0.105831
## row25 -1.81344  0.004611 -0.02022 -6.25932  1.276900  0.162542
## row26 -0.37324  0.539169  0.41698 -0.10356  0.099255  0.131802
## row27  0.76246  0.011700 -1.92192  0.05080 -0.305235 -0.801849
## row28  0.30313  0.554482 -0.42097  0.79339 -0.409757 -0.177813
## row29  0.54160  0.869045  4.25898  2.25532 -0.625595  1.073244
## row30 -1.23394 -0.489382 -0.52002 -1.97958  0.926973 -0.014235
## row31  0.39780  0.552798  1.04122 -0.51695  0.135709 -0.007307
## row32 -0.37463  0.175404 -0.79200  0.96691  0.142803 -0.201658
## row33  1.69139 -0.090545 -1.93650 -1.03120 -1.122280 -1.016715
## row34  0.48151  0.486157  2.11958  3.75873 -0.646170  0.514701
## row35  0.06236 -0.286263 -0.37509 -3.19727 -0.255766 -0.296253
## row36 -0.38132 -0.526112  5.32994  4.00103 -0.266117  1.559362
## row37 -1.09137 -1.156933 -2.18382 -3.11015  0.346459 -0.507656
## row38 -1.42084 -3.343145 -0.38820 -0.11040  0.809729  0.022932
## row39 -0.98507 -1.798210 -0.28107 -0.48119  0.529806 -0.021936
## row40 -0.68616 -1.811207 -0.07914  0.02499  0.325827 -0.045470
## row41  0.84738 -0.042626 -0.13673  0.45189  0.000903  0.139604
## row42  0.52173 -0.462560 -1.05166  0.27212  0.122909 -0.043106
## row43  0.87014  0.140114 -0.09022 -1.07335  0.002405  0.129693
## row44  1.42033  0.232568 -1.46279 -0.57770 -0.413797 -0.363876
## row45  0.62162  0.462679  0.26658  1.24125  0.323972  0.125759
## row46  1.32867  0.077666 -2.41439  0.63104 -0.200387 -0.580246
## row47  0.54648 -0.163968 -0.69925 -0.21887  0.039691  0.120521
## row48  0.46100  0.013093  0.45371 -0.29796  0.234807  0.395313
## row49  1.05549 -0.033624 -1.55023 -1.65233  0.001161 -0.424578
## row50  0.75562  1.056595  1.12386 -0.91787 -0.111664  0.500916
## row51  0.98951  1.138993  0.93102  3.75441 -0.657122  0.126073
## row52  0.57955 -0.383280  0.27890 -0.11564 -0.293886  0.029130
## row53 -0.43264 -0.752247 -2.01169 -0.89838  0.414836 -0.373897
## row54  0.87810 -0.025614  0.30465  0.53367 -0.210496 -0.041841
## row55 -0.02590 -0.776680  0.30696  1.36464  0.357604  0.136570
## row56 -0.10845 -0.122607  0.89605  0.20977 -0.079788  0.340771
## row57 -0.35294 -0.551753  0.88240  0.41618  0.235583  0.283178
## row58 -0.05423 -0.484832 -1.23636 -3.23534  0.278158 -0.304450
## row59  0.66189 -0.416249  0.24633  1.42364 -0.385505  0.097930
## row60 -0.22794 -0.407290 -1.13650 -1.00796  0.340615 -0.293465
## 
## 
## Site constraints (linear combinations of constraining variables)
## 
##           RDA1     RDA2     RDA3     RDA4       PC1       PC2
## row1  -0.26089  0.62605  0.22986  0.85861 -0.440562  0.691015
## row2  -0.32384  0.26106 -0.01812  1.23257 -0.759571 -0.351583
## row3  -0.60263  0.51133 -0.20105  0.56560  0.284619  1.128730
## row4  -0.85656  0.49559 -0.51524 -0.15249 -0.935897 -0.192376
## row5  -0.67594  0.78606 -0.23465  0.66850 -0.592485 -0.763339
## row6  -0.74300  0.04717 -0.36697 -0.13296  0.589039  0.035292
## row7  -0.85423  0.59937 -0.50007  0.08323 -0.548670  0.598587
## row8  -0.97219  0.44412 -0.73927 -0.28430 -0.049693 -0.295598
## row9  -0.81074  0.66824 -0.36931  0.17432  1.713923 -0.896904
## row10 -0.66412  0.22014 -0.19257  0.13661  0.739296  0.046176
## row11  0.25869  0.64577  0.95246  0.10841 -0.238842 -0.599955
## row12 -0.11740  0.06356  0.40479 -0.55878 -0.210929  0.652865
## row13 -0.01715  0.40916  0.55454 -0.30658  0.248290  0.562664
## row14  0.21229 -0.03820  0.89201  0.26072  0.224860  1.009485
## row15  0.07986  0.18115  0.46875  0.41073 -0.192672 -0.531850
## row16  0.01796  0.40511  0.63890  0.08011  0.470039 -0.317093
## row17 -0.21855  0.14193  0.31071 -0.71155  0.038103 -0.363888
## row18  0.03804  0.29169  0.71545 -0.31724 -0.035170 -0.384009
## row19 -0.08269 -0.18752  0.68556 -0.79016 -0.035303  0.669403
## row20 -0.10137  0.11185  0.51471 -0.44550 -0.268375 -0.697621
## row21 -0.05828 -0.11958  0.55695 -1.24800 -0.273158 -0.369210
## row22 -0.50030 -0.42731 -1.47362  0.64197  0.129132  0.123463
## row23 -0.23768  0.36247  0.16373 -0.69325 -0.822201 -0.233775
## row24 -0.09425  0.14008  0.12153 -0.15297  0.003687  0.105831
## row25 -0.01921  0.59298  0.26521  0.21092  1.276900  0.162542
## row26 -0.20630  0.73432  0.05741 -0.16163  0.099255  0.131802
## row27  0.13877  0.06953  0.44495  0.29022 -0.305235 -0.801849
## row28 -0.30743  0.36311 -0.12611 -0.72674 -0.409757 -0.177813
## row29 -0.12292  0.75749  0.11158  0.16304 -0.625595  1.073244
## row30  0.07298  0.14294  0.29231  0.18886  0.926973 -0.014235
## row31  0.58637  0.46632  1.15850  0.37005  0.135709 -0.007307
## row32 -0.14828 -0.93963  0.22870  0.27441  0.142803 -0.201658
## row33 -0.16662 -0.97979  0.30111  0.51927 -1.122280 -1.016715
## row34 -0.25937 -0.76233  0.08377  0.22608 -0.646170  0.514701
## row35 -0.40073 -0.88495  0.20936 -0.16872 -0.255766 -0.296253
## row36 -0.36371 -1.10373  0.11447 -0.01730 -0.266117  1.559362
## row37 -0.74130 -0.68708 -0.31597 -0.75265  0.346459 -0.507656
## row38 -0.31602 -1.03833  0.11786  0.01573  0.809729  0.022932
## row39 -0.23871 -1.04597  0.26583 -0.39551  0.529806 -0.021936
## row40 -0.25796 -0.82257  0.29131  0.23314  0.325827 -0.045470
## row41  0.88182 -0.02301 -0.58901  0.13887  0.000903  0.139604
## row42  0.69650 -0.12574 -0.74241 -0.72514  0.122909 -0.043106
## row43  0.88549  0.19463 -0.59175 -0.26733  0.002405  0.129693
## row44  0.73552  0.16822 -0.66685 -0.56148 -0.413797 -0.363876
## row45  1.15459  0.32377  0.30476 -0.15235  0.323972  0.125759
## row46  0.92171 -0.03042 -0.62613  0.30953 -0.200387 -0.580246
## row47  0.62954 -0.07726 -1.06231 -0.40526  0.039691  0.120521
## row48  0.86907  0.30125 -0.71024  0.27567  0.234807  0.395313
## row49  0.94693  0.05214 -0.20401 -0.47669  0.001161 -0.424578
## row50  0.70728  0.49635 -0.67316 -0.27760 -0.111664  0.500916
## row51  0.14766  0.05499  0.19300  0.06012 -0.657122  0.126073
## row52  0.14628 -0.48898 -0.16414  0.46062 -0.293886  0.029130
## row53  0.07475 -0.43552 -0.40563  0.06615  0.414836 -0.373897
## row54  0.57010 -0.51296  0.24191  1.05029 -0.210496 -0.041841
## row55  0.52471 -0.42227  0.22236  0.84924  0.357604  0.136570
## row56 -0.14403 -0.05262 -0.28757 -0.70911 -0.079788  0.340771
## row57  0.05615 -0.23937  0.20518 -0.52407  0.235583  0.283178
## row58  0.21229 -0.14090 -0.25033  0.99828  0.278158 -0.304450
## row59  0.14413 -0.60001 -0.36816 -0.10289 -0.385505  0.097930
## row60  0.17493  0.05607  0.07514  0.29637  0.340615 -0.293465
## 
## 
## Biplot scores for constraining variables
## 
##                  RDA1     RDA2     RDA3     RDA4 PC1 PC2
## altitude    -0.052022  0.83310 -0.16162 -0.26436   0   0
## slope       -0.126811 -0.01535 -0.25691 -0.44548   0   0
## rock         0.702348  0.03640 -0.60210 -0.09455   0   0
## disturbance  0.136109  0.09902  0.43460  0.08652   0   0
## aspect      -0.305177 -0.12562 -0.06303 -0.07136   0   0
## unitLO      -0.203837 -0.68921  0.21697  0.02691   0   0
## unitOC      -0.597827  0.41178 -0.25696  0.27837   0   0
## unitSU       0.006159  0.17893  0.54248 -0.20061   0   0
## unitTE       0.168540 -0.24584 -0.04757  0.21609   0   0
## unitUO      -0.117956  0.23121  0.03658 -0.13147   0   0
## 
## 
## Centroids for factor constraints
## 
##             RDA1    RDA2     RDA3     RDA4 PC1 PC2
## unitCH  0.842845  0.1280 -0.55611 -0.21418   0   0
## unitLO -0.230633 -0.7798  0.24549  0.03045   0   0
## unitOC -0.676414  0.4659 -0.29074  0.31497   0   0
## unitSU  0.006969  0.2024  0.61379 -0.22698   0   0
## unitTE  0.190696 -0.2782 -0.05382  0.24450   0   0
## unitUO -0.133462  0.2616  0.04139 -0.14876   0   0
```

I then got the R squared and the adjusted R squared for this analysis.


```r
(R2 <- RsquareAdj(str.rda)$r.squared)
```

```
## [1] 0.2569949
```

```r
(R2adj <- RsquareAdj(str.rda)$adj.r.squared)
```

```
## [1] 0.1053612
```

To test the null hypothesis that no linear relationship exists between the response data and the explanatory variables, I did a permutation analysis with 1000 permutations. 


```r
anova.cca(str.rda, step=1000)
```

```
## Permutation test for rda under reduced model
## Permutation: free
## Number of permutations: 999
## 
## Model: rda(formula = structure ~ altitude + slope + rock + disturbance + aspect + unit, data = env.ra, scale = T)
##          Df Variance      F Pr(>F)  
## Model    10    1.028 1.6948  0.047 *
## Residual 49    2.972                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

To make a triplot with scaling 2. 


```r
par(mar=c(4,4,2,2))
plot(str.rda, xlab="RDA1 (19.92 %)", ylab="RDA2 (3.51 %)", 
     display=c("cn", "lc", "sp"), type="n", xlim=c(-1.3,1.3))
sites.sc <- scores(str.rda, choices=1:2, scaling=2, display="lc")
points(sites.sc, pch=1, cex=0.5)
va.sc <- scores(str.rda, choices=1:2, scaling=2, display="sp")
text(va.sc, row.names(va.sc), cex=0.9)
env.sc <- scores(str.rda, choices=1:2, scaling=2, display="bp")
arrows(0,0, env.sc[1:5,1], env.sc[1:5,2], lty=1, lwd=2.5, length=0.1)
env.names <- c("Elevation", "Slope", "Rock", "CDI", "Aspect")
text(env.sc[c(1,3,4),], env.names[c(1,3,4)], cex=0.9, font=2, pos=4)
text(env.sc[c(2,5),], env.names[c(2,5)], cex=0.9, font=2, pos=2)
unit.names <- c("CH", "LO", "OC", "SU", "TE", "UO")
unit.sc <- scores(str.rda, choices=1:2, scaling=2, display="cn")
points(unit.sc, pch=23)
text(unit.sc[c(1,2,4,5),], unit.names[c(1,2,4,5)], cex=0.7, font=2, pos=4)
text(unit.sc[c(3,6),], unit.names[c(3,6)], cex=0.7, font=2, pos=3)
```

![](Data_analysis_files/figure-html/unnamed-chunk-7-1.png) 

### Composition data

First I loaded the species data. Again, the file `PA_total.csv` must be in the working directory. To reduce the importance of large abundances, I applied the Hellinger transformation to the species data, as recommended by Borcard et al. (2011, Numerical Ecology in R)
. 

```r
spe <- read.csv("PA_total.csv", sep=",", header=T, row.names=1)
spe.he <- decostand(spe,"hellinger")
```

Then I did a redundancy analysis using all the environmental variables as explanatory variables. I tested the null hypothesis with a permutation analysis.


```r
spe.rda <- rda(spe.he~., env.ra)
summary.sperda <- summary(spe.rda)
head(summary.sperda)
```

```
## 
## Call:
## rda(formula = spe.he ~ altitude + slope + rock + disturbance +      aspect + unit, data = env.ra) 
## 
## Partitioning of variance:
##               Inertia Proportion
## Total          0.8593     1.0000
## Constrained    0.2709     0.3153
## Unconstrained  0.5884     0.6847
## 
## Eigenvalues, and their contribution to the variance 
## 
## Importance of components:
##                          RDA1    RDA2    RDA3    RDA4    RDA5    RDA6
## Eigenvalue            0.06795 0.05164 0.04341 0.02369 0.01885 0.01825
## Proportion Explained  0.07908 0.06010 0.05052 0.02757 0.02194 0.02124
## Cumulative Proportion 0.07908 0.13917 0.18970 0.21727 0.23920 0.26044
##                          RDA7    RDA8    RDA9   RDA10     PC1     PC2
## Eigenvalue            0.01308 0.01264 0.01228 0.00910 0.03710 0.02956
## Proportion Explained  0.01522 0.01471 0.01430 0.01059 0.04318 0.03440
## Cumulative Proportion 0.27566 0.29037 0.30467 0.31526 0.35844 0.39283
##                           PC3     PC4     PC5     PC6     PC7     PC8
## Eigenvalue            0.02726 0.02463 0.02326 0.02174 0.02054 0.01957
## Proportion Explained  0.03173 0.02867 0.02708 0.02530 0.02390 0.02278
## Cumulative Proportion 0.42456 0.45323 0.48030 0.50561 0.52951 0.55228
##                           PC9    PC10   PC11    PC12    PC13    PC14
## Eigenvalue            0.01858 0.01787 0.0171 0.01676 0.01637 0.01541
## Proportion Explained  0.02162 0.02080 0.0199 0.01950 0.01905 0.01794
## Cumulative Proportion 0.57390 0.59470 0.6146 0.63410 0.65315 0.67109
##                          PC15    PC16    PC17    PC18    PC19    PC20
## Eigenvalue            0.01469 0.01398 0.01316 0.01289 0.01257 0.01183
## Proportion Explained  0.01710 0.01627 0.01531 0.01500 0.01463 0.01376
## Cumulative Proportion 0.68819 0.70445 0.71976 0.73477 0.74940 0.76316
##                          PC21    PC22    PC23    PC24    PC25     PC26
## Eigenvalue            0.01164 0.01084 0.01070 0.01049 0.01013 0.009514
## Proportion Explained  0.01354 0.01262 0.01245 0.01221 0.01178 0.011070
## Cumulative Proportion 0.77670 0.78932 0.80177 0.81399 0.82577 0.836840
##                           PC27     PC28     PC29     PC30     PC31
## Eigenvalue            0.009407 0.008943 0.008698 0.008482 0.007893
## Proportion Explained  0.010950 0.010410 0.010120 0.009870 0.009190
## Cumulative Proportion 0.847790 0.858200 0.868320 0.878190 0.887380
##                           PC32     PC33     PC34     PC35     PC36
## Eigenvalue            0.007565 0.007349 0.007034 0.006934 0.006694
## Proportion Explained  0.008800 0.008550 0.008190 0.008070 0.007790
## Cumulative Proportion 0.896180 0.904740 0.912920 0.920990 0.928780
##                           PC37     PC38     PC39     PC40     PC41
## Eigenvalue            0.006353 0.005784 0.005622 0.005219 0.004988
## Proportion Explained  0.007390 0.006730 0.006540 0.006070 0.005810
## Cumulative Proportion 0.936180 0.942910 0.949450 0.955520 0.961330
##                           PC42     PC43     PC44     PC45     PC46
## Eigenvalue            0.004838 0.004789 0.004715 0.004561 0.003928
## Proportion Explained  0.005630 0.005570 0.005490 0.005310 0.004570
## Cumulative Proportion 0.966960 0.972530 0.978020 0.983330 0.987900
##                           PC47     PC48     PC49
## Eigenvalue            0.003677 0.003378 0.003343
## Proportion Explained  0.004280 0.003930 0.003890
## Cumulative Proportion 0.992180 0.996110 1.000000
## 
## Accumulated constrained eigenvalues
## Importance of components:
##                          RDA1    RDA2    RDA3    RDA4    RDA5    RDA6
## Eigenvalue            0.06795 0.05164 0.04341 0.02369 0.01885 0.01825
## Proportion Explained  0.25083 0.19063 0.16025 0.08746 0.06958 0.06736
## Cumulative Proportion 0.25083 0.44146 0.60172 0.68918 0.75876 0.82611
##                          RDA7    RDA8    RDA9   RDA10
## Eigenvalue            0.01308 0.01264 0.01228 0.00910
## Proportion Explained  0.04829 0.04665 0.04535 0.03359
## Cumulative Proportion 0.87441 0.92106 0.96641 1.00000
## 
## Scaling 2 for species and site scores
## * Species are scaled proportional to eigenvalues
## * Sites are unscaled: weighted dispersion equal on all dimensions
## * General scaling constant of scores:  2.668355 
## 
## 
## Species scores
## 
##                                               RDA1      RDA2      RDA3
## Acacia.cochliacantha                     -0.003359 -0.013391 -0.006752
## Acacia.penatula                          -0.006813 -0.010297 -0.004528
## Acaciella.angustissima.var..angustissima -0.027410 -0.054519 -0.055067
## Acaciella.painteri.var..houghii          -0.001005  0.004012 -0.010721
## Acalypha.aff..triloba                    -0.006813 -0.010297 -0.004528
## Acalypha.langiana                        -0.012237 -0.050560 -0.070710
## ....                                                                  
##                                               RDA4      RDA5      RDA6
## Acacia.cochliacantha                      0.010861 -0.010221  0.001266
## Acacia.penatula                          -0.018864  0.001808 -0.005013
## Acaciella.angustissima.var..angustissima -0.012860  0.033837 -0.012646
## Acaciella.painteri.var..houghii           0.005222 -0.021100 -0.010779
## Acalypha.aff..triloba                    -0.018864  0.001808 -0.005013
## Acalypha.langiana                         0.063054  0.018409  0.080199
## ....                                                                  
## 
## 
## Site scores (weighted sums of species scores)
## 
##         RDA1     RDA2   RDA3   RDA4     RDA5    RDA6
## OC1  -0.4770 -0.09291 0.7532 0.2125 -0.46119  0.3947
## OC2  -0.5046 -0.19478 0.7846 0.1383 -0.55588  0.1253
## OC3  -0.3533  0.06168 0.6782 0.1060 -0.21171  0.1414
## OC4  -0.4736 -0.19019 0.6348 0.1188  0.08922 -0.1280
## OC5  -0.3711 -0.01763 0.7193 0.3439  0.24355  0.2106
## OC6  -0.4631 -0.27424 0.5637 0.1864 -0.02151 -0.4369
## ....                                                
## 
## 
## Site constraints (linear combinations of constraining variables)
## 
##         RDA1      RDA2   RDA3    RDA4      RDA5      RDA6
## OC1  -0.3943 -0.141786 0.5903 0.22991 -0.423332  0.472054
## OC2  -0.4585 -0.200318 0.7284 0.06717 -0.413690  0.062382
## OC3  -0.4085 -0.036607 0.6309 0.16138 -0.002839  0.073781
## OC4  -0.3810  0.002168 0.5868 0.16456  0.099085  0.003786
## OC5  -0.4050  0.106489 0.6316 0.18617  0.402167  0.169800
## OC6  -0.3762 -0.198568 0.5686 0.17043 -0.445906 -0.295726
## ....                                                     
## 
## 
## Biplot scores for constraining variables
## 
##                  RDA1     RDA2    RDA3     RDA4     RDA5     RDA6
## altitude     0.231336  0.79690  0.4754  0.20805  0.18250 -0.04546
## slope       -0.003372 -0.11149 -0.1998  0.67388 -0.01662 -0.39309
## rock         0.908813 -0.19820  0.3002 -0.06050  0.02770 -0.04448
## disturbance -0.080655 -0.07863 -0.1951 -0.16376 -0.32443  0.85189
## aspect      -0.346488 -0.13290  0.1872 -0.33498 -0.12575  0.13342
## unitLO      -0.255897 -0.41885 -0.2968 -0.77653  0.09837  0.10760
## unitOC      -0.515829 -0.05691  0.7944  0.21470 -0.03435  0.03652
## unitSU       0.051974  0.70402 -0.2385 -0.03111 -0.39614 -0.01947
## unitTE      -0.092466 -0.44377 -0.4847  0.68834  0.13124 -0.03868
## unitUO      -0.099056  0.39376 -0.1009 -0.05393  0.11655 -0.12238
## 
## 
## Centroids for factor constraints
## 
##            RDA1     RDA2    RDA3     RDA4     RDA5     RDA6
## unitCH  0.70194 -0.13731  0.2514 -0.03194  0.06495  0.02806
## unitLO -0.19711 -0.32263 -0.2286 -0.59815  0.07577  0.08288
## unitOC -0.39734 -0.04384  0.6119  0.16538 -0.02646  0.02813
## unitSU  0.04003  0.54230 -0.1837 -0.02396 -0.30514 -0.01500
## unitTE -0.07123 -0.34183 -0.3733  0.53022  0.10109 -0.02980
## unitUO -0.07630  0.30331 -0.0777 -0.04154  0.08978 -0.09427
```

```r
# Get the R^2 and an adjusted R^2
(spR2 <- RsquareAdj(spe.rda)$r.squared)
```

```
## [1] 0.3152582
```

```r
(spR2adj <- RsquareAdj(spe.rda)$adj.r.squared)
```

```
## [1] 0.175515
```

```r
# Permutation analysis
anova.cca(spe.rda, step=1000) 
```

```
## Permutation test for rda under reduced model
## Permutation: free
## Number of permutations: 999
## 
## Model: rda(formula = spe.he ~ altitude + slope + rock + disturbance + aspect + unit, data = env.ra)
##          Df Variance     F Pr(>F)    
## Model    10  0.27089 2.256  0.001 ***
## Residual 49  0.58837                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

To make a triplot of the RDA on species data with scaling 2.


```r
# Make plotting space
par(mar=c(4,4,2,2))
plot(spe.rda, scaling=2, display=c("cn", "lc", "sp"), type="n", 
     xlab="RDA1 (7.91 %)", ylab="RDA2 (6.01 %)")

# Plot points for sites
spsites.sc <- scores(spe.rda, choices=1:2, scaling=2, display="lc")
points(spsites.sc, pch=1, cex=0.5)

# Plot points for species
sp.sc <- scores(spe.rda, choices=1:2, scaling=2, display="sp")
points(sp.sc, pch=4, cex=0.5, col="gray50")

# Plot arrows of quantitative explanatory variables and their labels
spenv.sc <- scores(spe.rda, choices=1:2, scaling=2, display="bp")
arrows(0,0, spenv.sc[1:5,1], spenv.sc[1:5,2], lty=1, lwd=2.5, length=0.1, col="black")
env.names <- c("Elevation", "Slope", "Rock", "CDI", "Aspect")
text(spenv.sc[1:3,], env.names[1:3], cex=0.9, font=2, pos=4)
text(spenv.sc[4:5,], env.names[4:5], cex=0.9, font=2, pos=1)

# Plot point for geomorphological unit centroids and their labels
unit.names <- c("CH", "LO", "OC", "SU", "TE", "UO")
spunit.sc <- scores(spe.rda, choices=1:2, scaling=2, display="cn")
points(spunit.sc, pch=23)
text(spunit.sc[c(2,3,4,6),], unit.names[c(2,3,4,6)], cex=0.7, font=2, pos=2)
text(spunit.sc[c(1,5),], unit.names[c(1,5)], cex=0.7, font=2, pos=3)
```

![](Data_analysis_files/figure-html/unnamed-chunk-10-1.png) 

### Plot for the paper

This is the code to make the figure for the paper.


```r
par(mfrow=c(2,1))
# Fig 3a - Structure RDA triplot - Scaling 2 (Correlation triplot)
par(mar=c(4,4,2,1))
plot(1, xlab="RDA1 (19.92 %)", ylab="RDA2 (3.51 %)", type="n", 
     xlim=c(-1.3,1.3), ylim=c(-1.1,1.1))
abline(v=0, lty=3); abline(h=0, lty=3); mtext("a", side=3, adj=0, cex=1.25)
sites.sc <- scores(str.rda, choices=1:2, scaling=2, display="lc")
points(sites.sc, pch=1, cex=0.5)
va.sc <- scores(str.rda, choices=1:2, scaling=2, display="sp")
text(va.sc, row.names(va.sc), cex=0.9)
env.sc <- scores(str.rda, choices=1:2, scaling=2, display="bp")
arrows(0,0, env.sc[1:5,1], env.sc[1:5,2], lty=1, lwd=2.5, length=0.1)
env.names <- c("Elevation", "Slope", "Rock", "CDI", "Aspect")
text(env.sc[c(3,4),], env.names[c(3,4)], cex=0.9, font=2, pos=4)
text(env.sc[c(2,5),], env.names[c(2,5)], cex=0.9, font=2, pos=2)
text(env.sc[1,1], env.sc[1,2], env.names[1], cex=0.9, font=2, pos=3)
unit.names <- c("CH", "LO", "OC", "SU", "TE", "UO")
unit.sc <- scores(str.rda, choices=1:2, scaling=2, display="cn")
points(unit.sc, pch=18)
text(unit.sc[c(2,4,5),], unit.names[c(2,4,5)], cex=0.7, font=2, pos=4)
text(unit.sc[c(1,3,6),], unit.names[c(1,3,6)], cex=0.7, font=2, pos=3)

# Fig 3b - Species composition RDA triplot - Scaling 2 (Correlation triplot)
# Make plotting space
par(mar=c(4,4,2,1))
plot(1, type="n", xlim=c(-0.5,1), ylim=c(-0.6,0.8),
     xlab="RDA1 (7.91 %)", ylab="RDA2 (6.01 %)")
abline(v=0, lty=3); abline(h=0, lty=3)
mtext("b", side=3, adj=0, cex=1.25)

# Plot points for sites
spsites.sc <- scores(spe.rda, choices=1:2, scaling=2, display="lc")
points(spsites.sc, pch=1, cex=0.5)

# Plot points for species
sp.sc <- scores(spe.rda, choices=1:2, scaling=2, display="sp")
points(sp.sc, pch=4, cex=0.5, col="gray50")

# Plot arrows of quantitative explanatory variables and their labels
spenv.sc <- scores(spe.rda, choices=1:2, scaling=2, display="bp")
arrows(0,0, spenv.sc[1:5,1], spenv.sc[1:5,2], lty=1, lwd=2.5, length=0.1, col="black")
env.names <- c("Elevation", "Slope", "Rock", "CDI", "Aspect")
text(spenv.sc[1:3,], env.names[1:3], cex=0.9, font=2, pos=4)
text(spenv.sc[4:5,], env.names[4:5], cex=0.9, font=2, pos=1)

# Plot point for geomorphological unit centroids and their labels
unit.names <- c("CH", "LO", "OC", "SU", "TE", "OU")
spunit.sc <- scores(spe.rda, choices=1:2, scaling=2, display="cn")
points(spunit.sc, pch=18)
text(spunit.sc[c(2,3,4,6),], unit.names[c(2,3,4,6)], cex=0.7, font=2, pos=2)
text(spunit.sc[c(1,5),], unit.names[c(1,5)], cex=0.7, font=2, pos=3)
```

![](Data_analysis_files/figure-html/unnamed-chunk-11-1.png) 
