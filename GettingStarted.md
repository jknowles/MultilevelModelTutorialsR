Getting Started with Multilevel Modeling in R
========================================================

## Jared E. Knowles

# Introduction

Analysts dealing with grouped data and complex hierarchical structures in their 
data ranging from measurements nested within participants, to counties nested within 
states or students nested within classrooms often find themselves in need of modeling 
tools to reflect this structure of their data. In R there are two predominant ways 
to fit multilevel models that account for such structure in the data. These tutorials 
will show the user how to use both the `lme4` package in R to fit linear and nonlinear 
mixed effect models, and to use `rstan` to fit fully Bayesian multilevel models. The 
focus here will be on how to fit the models in R and not the theory behind the models. 
For background on multilevel modeling, see the references. [1]

This tutorial will cover getting set up and running a few basic models using `lme4` 
in R.Future tutorials will cover:

* constructing varying intercept, varying slope, and varying slope and intercept models in R
* generating predictions and interpreting parameters from mixed-effect models
* generalized and non-linear multilevel models
* fully Bayesian multilevel models fit with `rstan` or other MCMC methods

# Setting up your enviRonment

Getting started with multilevel modeling in R is simple. `lme4` is the canonical 
package for implementing multilevel models in R, though there are a number of packages 
that depend on and enhance its feature set, including Bayesian extensions. `lme4` 
has been recently rewritten to improve speed and to incorporate a C++ codebase, and 
as such the features of the package are somewhat in flux. Be sure to update the package 
frequently. 

To install `lme4`, we just run:


```r
# Main version
install.packages("lme4")

# Or to install the dev version
library(devtools)
install_github("lme4", user = "lme4")
```





# Read in the data

Multilevel models are appropriate for a particular kind of data structure where 
units are nested within groups (generally 5+ groups) and where we want to model 
the group structure of the data. For our introductory example we will start with 
a simple example from the `lme4` documentation and explain what the model is doing. 
We will use data from Jon Starkweather at the [University of North Texas](http://www.unt.edu/rss/class/Jon/R_SC/). Visit the excellent tutorial [available here for more.](http://www.unt.edu/rss/class/Jon/R_SC/Module9/LMM_Examples.R)


```r
library(lme4)  # load library
library(arm)  # convenience functions for regression in R
lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt", 
    header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)
# summary(lmm.data)
head(lmm.data)
```

```
##   id extro  open agree social class school
## 1  1 63.69 43.43 38.03  75.06     d     IV
## 2  2 69.48 46.87 31.49  98.13     a     VI
## 3  3 79.74 32.27 40.21 116.34     d     VI
## 4  4 62.97 44.41 30.51  90.47     c     IV
## 5  5 64.25 36.86 37.44  98.52     d     IV
## 6  6 50.97 46.26 38.83  75.22     d      I
```


Here we have data on the extroversion of subjects nested within classes and within 
schools. 

# Fit the Non-Multilevel Models

Let's start by fitting a simple OLS regression of measures of openness, agreeableness, 
and socialability on extroversion. Here we use the `display` function in the 
excellent `arm` package for abbreviated output. Other options include `stargazer` 
for LaTeX typeset tables, `xtable`, or the `ascii` package for more flexible 
plain text output options. 


```r
OLSexamp <- lm(extro ~ open + agree + social, data = lmm.data)
display(OLSexamp)
```

```
## lm(formula = extro ~ open + agree + social, data = lmm.data)
##             coef.est coef.se
## (Intercept) 57.84     3.15  
## open         0.02     0.05  
## agree        0.03     0.05  
## social       0.01     0.02  
## ---
## n = 1200, k = 4
## residual sd = 9.34, R-Squared = 0.00
```


So far this model does not fit very well at all. The R model interface is quite a 
simple one with the dependent variable being specified first, followed by the 
`~` symbol. The righ hand side, predictor variables, are each named. Addition 
signs indicate that these are modeled as additive effects. Finally, we specify 
that datframe on which to calculate the model. Here we use the `lm` function to 
perform OLS regression, but there are many other options in R. 

If we want to extract measures such as the AIC, we may prefer to fit a generalized 
linear model with `glm` which produces a model fit through maximum likelihood 
estimation. Note that the model formula specification is the same. 


```r
MLexamp <- glm(extro ~ open + agree + social, data = lmm.data)
display(MLexamp)
```

```
## glm(formula = extro ~ open + agree + social, data = lmm.data)
##             coef.est coef.se
## (Intercept) 57.84     3.15  
## open         0.02     0.05  
## agree        0.03     0.05  
## social       0.01     0.02  
## ---
##   n = 1200, k = 4
##   residual deviance = 104378.2, null deviance = 104432.7 (difference = 54.5)
##   overdispersion parameter = 87.3
##   residual sd is sqrt(overdispersion) = 9.34
```

```r
AIC(MLexamp)
```

```
## [1] 8774
```


This results in a poor model fit. Let's look at a simple varying intercept model now. 

# Fit a varying intercept model

Depending on disciplinary norms, our next step might be to fit a varying intercept 
model using a grouping variable such as school or classes. Using the `glm` function 
and the familiar formula interface, such a fit is easy:


```r
MLexamp.2 <- glm(extro ~ open + agree + social + class, data = lmm.data)
display(MLexamp.2)
```

```
## glm(formula = extro ~ open + agree + social + class, data = lmm.data)
##             coef.est coef.se
## (Intercept) 56.05     3.09  
## open         0.03     0.05  
## agree       -0.01     0.05  
## social       0.01     0.02  
## classb       2.06     0.75  
## classc       3.70     0.75  
## classd       5.67     0.75  
## ---
##   n = 1200, k = 7
##   residual deviance = 99187.7, null deviance = 104432.7 (difference = 5245.0)
##   overdispersion parameter = 83.1
##   residual sd is sqrt(overdispersion) = 9.12
```

```r
AIC(MLexamp.2)
```

```
## [1] 8719
```

```r
anova(MLexamp, MLexamp.2, test = "F")
```

```
## Analysis of Deviance Table
## 
## Model 1: extro ~ open + agree + social
## Model 2: extro ~ open + agree + social + class
##   Resid. Df Resid. Dev Df Deviance    F  Pr(>F)    
## 1      1196     104378                             
## 2      1193      99188  3     5190 20.8 3.8e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


This is called a fixed-effects specification often. This is simply the case of 
fitting a separate dummy variable as a predictor for each class. We can see 
this does not provide much additional model fit. Let's see if school performs 
any better. 


```r
MLexamp.3 <- glm(extro ~ open + agree + social + school, data = lmm.data)
display(MLexamp.3)
```

```
## glm(formula = extro ~ open + agree + social + school, data = lmm.data)
##             coef.est coef.se
## (Intercept) 45.02     0.92  
## open         0.01     0.01  
## agree        0.03     0.02  
## social       0.00     0.00  
## schoolII     7.91     0.27  
## schoolIII   12.12     0.27  
## schoolIV    16.06     0.27  
## schoolV     20.43     0.27  
## schoolVI    28.05     0.27  
## ---
##   n = 1200, k = 9
##   residual deviance = 8496.2, null deviance = 104432.7 (difference = 95936.5)
##   overdispersion parameter = 7.1
##   residual sd is sqrt(overdispersion) = 2.67
```

```r
AIC(MLexamp.3)
```

```
## [1] 5774
```

```r
anova(MLexamp, MLexamp.3, test = "F")
```

```
## Analysis of Deviance Table
## 
## Model 1: extro ~ open + agree + social
## Model 2: extro ~ open + agree + social + school
##   Resid. Df Resid. Dev Df Deviance    F Pr(>F)    
## 1      1196     104378                            
## 2      1191       8496  5    95882 2688 <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


The school effect greatly improves our model fit. However, how do we interpret these
effects? 


```r
table(lmm.data$school, lmm.data$class)
```

```
##      
##        a  b  c  d
##   I   50 50 50 50
##   II  50 50 50 50
##   III 50 50 50 50
##   IV  50 50 50 50
##   V   50 50 50 50
##   VI  50 50 50 50
```


Here we can see we have a perfectly balanced design with fifty observations in 
each combination of class and school (if only data were always so nice!). 

Let's try to model each of these unique cells. To do this, we fit a model and use 
the `:` operator to specify the interaction between `school` and `class`. 


```r
MLexamp.4 <- glm(extro ~ open + agree + social + school:class, data = lmm.data)
display(MLexamp.4)
```

```
## glm(formula = extro ~ open + agree + social + school:class, data = lmm.data)
##                  coef.est coef.se
## (Intercept)       80.36     0.37 
## open               0.01     0.00 
## agree             -0.01     0.01 
## social             0.00     0.00 
## schoolI:classa   -40.39     0.20 
## schoolII:classa  -28.15     0.20 
## schoolIII:classa -23.58     0.20 
## schoolIV:classa  -19.76     0.20 
## schoolV:classa   -15.50     0.20 
## schoolVI:classa  -10.46     0.20 
## schoolI:classb   -34.60     0.20 
## schoolII:classb  -26.76     0.20 
## schoolIII:classb -22.59     0.20 
## schoolIV:classb  -18.71     0.20 
## schoolV:classb   -14.31     0.20 
## schoolVI:classb   -8.54     0.20 
## schoolI:classc   -31.86     0.20 
## schoolII:classc  -25.64     0.20 
## schoolIII:classc -21.58     0.20 
## schoolIV:classc  -17.58     0.20 
## schoolV:classc   -13.38     0.20 
## schoolVI:classc   -5.58     0.20 
## schoolI:classd   -30.00     0.20 
## schoolII:classd  -24.57     0.20 
## schoolIII:classd -20.64     0.20 
## schoolIV:classd  -16.60     0.20 
## schoolV:classd   -12.04     0.20 
## ---
##   n = 1200, k = 27
##   residual deviance = 1135.9, null deviance = 104432.7 (difference = 103296.8)
##   overdispersion parameter = 1.0
##   residual sd is sqrt(overdispersion) = 0.98
```

```r
AIC(MLexamp.4)
```

```
## [1] 3396
```


This is very useful, but what if we want to understand both the effect of the 
school and the effect of the class, as well as the effect of the schools and classes? 
Unfortunately, this is not easily done with the standard `glm`. 


```r
MLexamp.5 <- glm(extro ~ open + agree + social + school * class - 1, data = lmm.data)
display(MLexamp.5)
```

```
## glm(formula = extro ~ open + agree + social + school * class - 
##     1, data = lmm.data)
##                  coef.est coef.se
## open              0.01     0.00  
## agree            -0.01     0.01  
## social            0.00     0.00  
## schoolI          39.96     0.36  
## schoolII         52.21     0.36  
## schoolIII        56.78     0.36  
## schoolIV         60.60     0.36  
## schoolV          64.86     0.36  
## schoolVI         69.90     0.36  
## classb            5.79     0.20  
## classc            8.53     0.20  
## classd           10.39     0.20  
## schoolII:classb  -4.40     0.28  
## schoolIII:classb -4.80     0.28  
## schoolIV:classb  -4.74     0.28  
## schoolV:classb   -4.60     0.28  
## schoolVI:classb  -3.87     0.28  
## schoolII:classc  -6.02     0.28  
## schoolIII:classc -6.54     0.28  
## schoolIV:classc  -6.36     0.28  
## schoolV:classc   -6.41     0.28  
## schoolVI:classc  -3.65     0.28  
## schoolII:classd  -6.81     0.28  
## schoolIII:classd -7.45     0.28  
## schoolIV:classd  -7.24     0.28  
## schoolV:classd   -6.93     0.28  
## schoolVI:classd   0.06     0.28  
## ---
##   n = 1200, k = 27
##   residual deviance = 1135.9, null deviance = 4463029.9 (difference = 4461894.0)
##   overdispersion parameter = 1.0
##   residual sd is sqrt(overdispersion) = 0.98
```

```r
AIC(MLexamp.5)
```

```
## [1] 3396
```


# Exploring Random Slopes

Another alternative is to fit a separate model for each of the school and class 
combinations. If we believe the relationsihp between our variables may be highly 
dependent on the school and class combination, we can simply fit a series of models 
and explore the parameter variation among them:


```r
require(plyr)

modellist <- dlply(lmm.data, .(school, class), function(x) glm(extro ~ open + 
    agree + social, data = x))
display(modellist[[1]])
```

```
## glm(formula = extro ~ open + agree + social, data = x)
##             coef.est coef.se
## (Intercept) 35.87     5.90  
## open         0.05     0.09  
## agree        0.02     0.10  
## social       0.01     0.03  
## ---
##   n = 50, k = 4
##   residual deviance = 500.2, null deviance = 506.2 (difference = 5.9)
##   overdispersion parameter = 10.9
##   residual sd is sqrt(overdispersion) = 3.30
```

```r
display(modellist[[2]])
```

```
## glm(formula = extro ~ open + agree + social, data = x)
##             coef.est coef.se
## (Intercept) 47.96     2.16  
## open        -0.01     0.03  
## agree       -0.03     0.03  
## social      -0.01     0.01  
## ---
##   n = 50, k = 4
##   residual deviance = 47.9, null deviance = 49.1 (difference = 1.2)
##   overdispersion parameter = 1.0
##   residual sd is sqrt(overdispersion) = 1.02
```


We will discuss this strategy in more depth in future tutorials including how to 
performan inference on the list of models produced in this command. 

# Fit a varying intercept model with lmer

Enter `lme4`. While all of the above techniques are valid approaches to this problem, 
they are not necessarily the best approach when we are interested explicitly in 
variation among and by groups. This is where a mixed-effect modeling framework 
is useful. Now we use the `lmer` function with the familiar formula interface, 
but now group level variables are specified using a special syntax: `(1|school)` 
tells `lmer` to fit a linear model with a varying-intercept group effect using 
the variable `school`. 


```r
MLexamp.6 <- lmer(extro ~ open + agree + social + (1 | school), data = lmm.data)
display(MLexamp.6)
```

```
## lmer(formula = extro ~ open + agree + social + (1 | school), 
##     data = lmm.data)
##             coef.est coef.se
## (Intercept) 59.12     4.10  
## open         0.01     0.01  
## agree        0.03     0.02  
## social       0.00     0.00  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  school   (Intercept) 9.79    
##  Residual             2.67    
## ---
## number of obs: 1200, groups: school, 6
## AIC = 5836.1, DIC = 5789
## deviance = 5806.5
```


We can fit multiple group effects with multiple group effect terms. 



```r
MLexamp.7 <- lmer(extro ~ open + agree + social + (1 | school) + (1 | class), 
    data = lmm.data)
display(MLexamp.7)
```

```
## lmer(formula = extro ~ open + agree + social + (1 | school) + 
##     (1 | class), data = lmm.data)
##             coef.est coef.se
## (Intercept) 60.20     4.21  
## open         0.01     0.01  
## agree       -0.01     0.01  
## social       0.00     0.00  
## 
## Error terms:
##  Groups   Name        Std.Dev.
##  school   (Intercept) 9.79    
##  class    (Intercept) 2.41    
##  Residual             1.67    
## ---
## number of obs: 1200, groups: school, 6; class, 4
## AIC = 4737.9, DIC = 4683
## deviance = 4703.6
```


And finally, we can fit nested group effect terms through the following syntax:


```r
MLexamp.8 <- lmer(extro ~ open + agree + social + (1 | school/class), data = lmm.data)
display(MLexamp.8)
```

```
## lmer(formula = extro ~ open + agree + social + (1 | school/class), 
##     data = lmm.data)
##             coef.est coef.se
## (Intercept) 60.24     4.01  
## open         0.01     0.00  
## agree       -0.01     0.01  
## social       0.00     0.00  
## 
## Error terms:
##  Groups       Name        Std.Dev.
##  class:school (Intercept) 2.86    
##  school       (Intercept) 9.69    
##  Residual                 0.98    
## ---
## number of obs: 1200, groups: class:school, 24; school, 6
## AIC = 3568.6, DIC = 3508
## deviance = 3531.1
```

```r

```


Here the `(1|school/class)` says that we want to fit a mixed effect term for varying 
intercepts `1|` by schools, and for classes that are nested within schools. 

# Fit a varying slope model with lmer

But, what if we want to explore the effect of different student level indicators 
as they vary across classrooms. Instead of fitting unique models by school (or school/class) 
we can fit a varying slope model. Here we modify our random effect term to include 
variables before the grouping terms: `(1 + open|school/class)` tells R to fit 
a varying slope and varying intercept model for schools and classes nested within 
schools, and to allow the slope of the `open` variable to vary by school. 


```r
MLexamp.9 <- lmer(extro ~ open + agree + social + (1 + open | school/class), 
    data = lmm.data)
display(MLexamp.9)
```

```
## lmer(formula = extro ~ open + agree + social + (1 + open | school/class), 
##     data = lmm.data)
##             coef.est coef.se
## (Intercept) 60.26     3.93  
## open         0.01     0.01  
## agree       -0.01     0.01  
## social       0.00     0.00  
## 
## Error terms:
##  Groups       Name        Std.Dev. Corr 
##  class:school (Intercept) 2.62          
##               open        0.01     1.00 
##  school       (Intercept) 9.51          
##               open        0.00     1.00 
##  Residual                 0.98          
## ---
## number of obs: 1200, groups: class:school, 24; school, 6
## AIC = 3574.7, DIC = 3506
## deviance = 3529.3
```


# Conclusion

Fitting mixed effect models and exploring group level variation is very easy within 
the R language and ecosystem. In future tutorials we will explore comparing across 
models, doing inference with mixed-effect models, and creating graphical representations 
of mixed effect models to understand their effects. 

# Appendix


```r
print(sessionInfo(), locale = FALSE)
```

```
## R version 3.0.1 (2013-05-16)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8        arm_1.6-10      MASS_7.3-29     lme4_1.0-5     
## [5] Matrix_1.1-0    lattice_0.20-24 knitr_1.5      
## 
## loaded via a namespace (and not attached):
##  [1] abind_1.4-0    coda_0.16-1    evaluate_0.5.1 formatR_0.10  
##  [5] grid_3.0.1     minqa_1.2.1    nlme_3.1-113   splines_3.0.1 
##  [9] stringr_0.6.2  tools_3.0.1
```


[1] Examples include [Gelman and Hill](http://stat.columbia.edu/~gelman/arm), 
[Gelman et al. 2013](http://stat.columbia.edu/~gelman/book/), etc. 
