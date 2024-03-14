---
title: How to Interpret Linear Models
hidden: true
---
## [Return to R tutorials list](%base_url%/?r-language)

# Interpreting coefficients in linear models {.inner-page}
==========================================

The interpretation of coefficients in (generalized) linear models is
more subtle than you many realise, and has consequences for how we test
hypotheses and report findings. We will start by talking about
**marginal vs. conditional interpretations** of model parameters.

![](%theme_url%/img/mato.jpg)

In this example, we model plant height as a function of altitude and
temperature. These variables are negatively correlated: it is colder the
higher you go. We start by simulating some data to reflect this.

``` {.r}
library(mvtnorm)

# Specify the sample size
N <- 1000

# Specify the correlation between altitude and temperature
rho <- -0.4

# This line of code creates two correlated variables
X = rmvnorm(N, mean = c(0, 0), sigma = matrix(c(1, rho, rho, 1), 2, 2))

# Extract the first and second columns to vectors named temp and alt and plot
temp <- X[,1]
alt <- X[,2]
plot(temp, alt)
```

![](%theme_url%/img/grafico.jpg)

Now we can simulate some data for height of plants. Here we say that the
mean height of plants is 2 (when all the other variables are 0), as
temperature increases by one unit (holding altitude constant), the mean
of height will increase by 1 unit (`beta[2] = 1`), and similarly as you
increase altitude by 1 unit (holding temperature constant) then mean
height decreases by 1 (`beta[3] = -1`). Height is then normally
distributed with this mean and standard deviation of 2.

``` {.r}
beta <- c(2, 1, -1)
mu <- beta[1]+beta[2]*temp+beta[3] * alt
height <- rnorm(N, mu, sd = 2)
```

If we use a linear model to find the coefficients we get what we expect,
estimates very close to the true values.

``` {.r}
lm_both <- lm(height ~ temp + alt)
data.frame(estimated = round(lm_both$coefficients, 2), true = beta)
```

    ##             estimated true
    ## (Intercept)      1.96    2
    ## temp             0.94    1
    ## alt             -1.06   -1

The interpretation of these coefficients is that if you hold everything
else in the model constant (i.e., temperature) and add 1 to altitude,
then the estimated mean height will decrease by 1.06. Note that the
coefficient depends on the units in which altitude is measured. If
altitude is in meters then the coefficient tells you what happens when
you go up 1 meter.

The intercept is the predicted value when all the other variables are
set to 0, which sometimes makes sense (here it would be the height of
plants at sea level and 0 temperature). Other times 0 is not a
meaningful value, and if you would like to interpret the intercept it
might make sense to rescale your other variables so that their mean is
0. If you do this, then the intercept is the predicted value when all
other variables are at their mean level.

What if now we had a model with just temperature?

``` {.r}
lm1 <- lm(height ~ temp)
lm1$coefficients
```

    ## (Intercept)        temp 
    ##    1.960815    1.384433

The coefficient of temperature is now 1.38, what’s going on? Altitude is
an important predictor of plant height, and some of the information
about altitude is contained in temperature (remember they are
correlated, so as altitude increases temperature decreases). The model
accounts for this by changing the effect of temperature to take account
of the information it contains about altitude. Notice the coefficient of
temperature is wrong by approximately 0.4, the amount of correlation
between the variables.

Note: When statisticians talk about this, they use the words
**conditional** and **marginal**. Conditional is the effect of a
variable when others are held constant (as in lm\_both), while marginal
is the overall effect (as in lm1). Note: If you use the code above to
simulate your own data sets, you will get slightly different values for
the coefficients.

### Testing hypotheses

This distinction has a lot of consequences for modelling as well as
testing hypothesis. Let’s generate some data where altitude predicts
height, and temperature has no (additional) information, and then test
for temperature.

``` {.r}
mu <- 2-1*alt
height <- rnorm(N, mu, sd = 2)

mod_temp <- lm(height ~ temp)
summary(mod_temp)
```

    ## 
    ## Call:
    ## lm(formula = height ~ temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5918 -1.4418 -0.0028  1.4809  6.2444 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.99260    0.06728  29.618  < 2e-16 ***
    ## temp         0.43831    0.06608   6.633 5.38e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.127 on 998 degrees of freedom
    ## Multiple R-squared:  0.04222,    Adjusted R-squared:  0.04126 
    ## F-statistic:    44 on 1 and 998 DF,  p-value: 5.383e-11

``` {.r}
anova(mod_temp)
```

    ## Analysis of Variance Table
    ## 
    ## Response: height
    ##            Df Sum Sq Mean Sq F value    Pr(>F)    
    ## temp        1    199 199.042  43.996 5.383e-11 ***
    ## Residuals 998   4515   4.524                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of this model is telling us there is an effect of
temperature, even though technically there isn’t. It is not giving us
false information if you understand how to interpret model outputs.
Because temperature is correlated with altitude, and there is an effect
of altitude, when altitude is not in the model, the model tells us
overall there is an effect of temperature of increasing height by 0.44
(remember the correlation was 0.4). If our hypothesis is ‘Does plant
height change with temperature?’, the answer is yes, the higher the
temperature, the taller the plants.

But what about altitude? We know the temperature effect we observe is
because it is correlated with altitude, temperature does not directly
predict height. If we want to know if there is an effect of temperature
after controlling for altitude (holding altitude constant, so
conditional), then we fit the model with altitude and then test for
temperature.

``` {.r}
mod_temp_alt <- lm(height ~ alt + temp)
summary(mod_temp_alt)
```

    ## 
    ## Call:
    ## lm(formula = height ~ alt + temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2992 -1.2790  0.0308  1.3970  7.5105 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.99174    0.06145  32.411   <2e-16 ***
    ## alt         -0.95342    0.06756 -14.111   <2e-16 ***
    ## temp         0.03621    0.06675   0.542    0.588    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.943 on 997 degrees of freedom
    ## Multiple R-squared:  0.2017, Adjusted R-squared:  0.2001 
    ## F-statistic: 125.9 on 2 and 997 DF,  p-value: < 2.2e-16

``` {.r}
anova(mod_temp_alt)
```

    ## Analysis of Variance Table
    ## 
    ## Response: height
    ##            Df Sum Sq Mean Sq  F value Pr(>F)    
    ## alt         1  949.6  949.58 251.5666 <2e-16 ***
    ## temp        1    1.1    1.11   0.2943 0.5876    
    ## Residuals 997 3763.4    3.77                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The p-value is about 0.95, so we have no evidence of an effect of
temperature after controlling for altitude.

Note: The distinction between conditional and marginal interpretations
is also true for generalised linear models and mixed models.

### Categorical covariates

When we have categorical covariates (for example treatment), there are a
number of ways to code the model, which will give different
interpretations for the coefficients. Let’s simulate 120 data points
with 40 in each of three levels of a categorical treatment.

``` {.r}
N <- 120
# The effect of treatment
trt.n <- rep(c(-1, 0, 1), N/3)
mu <- 2+1*trt.n

# Labels for the treatment
treatment <- factor(rep(c("low", "med", "high"), N/3))#group labels

# Create, Y, a normally distributed response variable and plot against treatment
Y <- rnorm(N, mu, sd = 2)
boxplot(Y ~ treatment)
```

![](%theme_url%/img/grafico1.jpg)

If we put treatment in as a covariate the normal way, the model will
choose a reference treatment (here it will be high as the levels get
sorted alphabetically), so that the intercept will be the mean of this
reference group. The other coefficients will be the differences between
the other groups and the reference group.

``` {.r}
cat_lm <- lm(Y ~ treatment)
summary(cat_lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ treatment)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5634 -1.1596 -0.0194  1.0761  5.9872 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.1501     0.3021  10.426  < 2e-16 ***
    ## treatmentlow  -2.3181     0.4273  -5.425 3.17e-07 ***
    ## treatmentmed  -1.4695     0.4273  -3.439 0.000809 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.911 on 117 degrees of freedom
    ## Multiple R-squared:  0.2048, Adjusted R-squared:  0.1912 
    ## F-statistic: 15.07 on 2 and 117 DF,  p-value: 1.505e-06

So here group “high” has a mean of 3.15, and the difference between the
means of group “low” and group “high” is -2.32, and the difference
between group “med” and group “high” is -1.47. If you would like to have
another group as the reference group, you can use `relevel` to recode
your treatment factor.

``` {.r}
treatment <- relevel(treatment, ref = "low")
cat_lm <- lm(Y ~ treatment)
summary(cat_lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ treatment)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5634 -1.1596 -0.0194  1.0761  5.9872 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     0.8320     0.3021   2.754  0.00683 ** 
    ## treatmenthigh   2.3181     0.4273   5.425 3.17e-07 ***
    ## treatmentmed    0.8485     0.4273   1.986  0.04940 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.911 on 117 degrees of freedom
    ## Multiple R-squared:  0.2048, Adjusted R-squared:  0.1912 
    ## F-statistic: 15.07 on 2 and 117 DF,  p-value: 1.505e-06

``` {.r}
boxplot(Y ~ treatment)
```

![](%theme_url%/img/grafico2.jpg)

Now the intercept is the mean of group “low”, and all the other
coefficients are the differences between group “low” and the others.
Another thing you can do is to put `-1` in the model to get rid of the
intercept, and just have the means of each group as coefficients.

``` {.r}
cat_lm <- lm(Y ~ treatment - 1)
summary(cat_lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ treatment - 1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5634 -1.1596 -0.0194  1.0761  5.9872 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## treatmentlow    0.8320     0.3021   2.754  0.00683 ** 
    ## treatmenthigh   3.1501     0.3021  10.426  < 2e-16 ***
    ## treatmentmed    1.6805     0.3021   5.562 1.71e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.911 on 117 degrees of freedom
    ## Multiple R-squared:  0.5572, Adjusted R-squared:  0.5458 
    ## F-statistic: 49.07 on 3 and 117 DF,  p-value: < 2.2e-16

Now, the three coefficients are the mean of the groups.

**Contrasting the coefficients** We can also look at contrasts; these
are the difference between all pairs of groups. Load the package
[multcomp](https://cran.r-project.org/web/packages/multcomp/index.html)
and use `glht` (general linear hypotheses) to examine all pair-wise
differences.

``` {.r}
library(multcomp)

cont <- glht(cat_lm, linfct = mcp(treatment = "Tukey"))

summary(cont)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: lm(formula = Y ~ treatment - 1)
    ## 
    ## Linear Hypotheses:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## high - low == 0   2.3181     0.4273   5.425   <1e-04 ***
    ## med - low == 0    0.8485     0.4273   1.986   0.1202    
    ## med - high == 0  -1.4695     0.4273  -3.439   0.0023 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

Each line of this output compares two groups against one another. The
first line, for example, compares the “high” group to the “low” group.
So the difference between the means of the “high” and “low” groups is
2.3. The p-values and the confidence intervals given by `glht` control
for multiple testing, which is handy. If you want to see the confidence
intervals for the differences between the groups.

``` {.r}
confint(cont)
```

    ## 
    ##   Simultaneous Confidence Intervals
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: lm(formula = Y ~ treatment - 1)
    ## 
    ## Quantile = 2.3743
    ## 95% family-wise confidence level
    ##  
    ## 
    ## Linear Hypotheses:
    ##                 Estimate lwr     upr    
    ## high - low == 0  2.3181   1.3035  3.3326
    ## med - low == 0   0.8485  -0.1660  1.8631
    ## med - high == 0 -1.4695  -2.4841 -0.4550

Note: In a model with multiple covariates, the same rules still apply in
terms of conditional and marginal interpretations of coefficients.

**Interpreting coefficients in generalised linear models** In linear
models, the interpretation of model parameters is linear, as discussed
above. For generalised linear models, now read the tutorial page on
[interpreting coefficients in those
models](%base_url%/?interpreting-glm-coefficients/).

 `Last updated: May 17, 2020`