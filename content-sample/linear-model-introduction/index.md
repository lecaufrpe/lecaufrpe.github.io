---
title: Linear Models Introduction
hidden: true
---
## [Return to R tutorials list](%base_url%/?r-language)

# Linear Models Introduction

These pages have some introductions to commonly used linear models that test the response of a continuous dependent variable against one or more predictor variables that may be continuous or categorical. Note that these are only named as different techniques (e.g., regression vs ANOVA) due to common usage in the literature that you will encounter - they all involve the same linear modelling framework.


```{r,echo=F}
Plant_height = read.csv(file = "Plant_Height.csv", header = TRUE)
model <- lm(loght ~ temp, data = Plant_height)
plot(loght ~ temp, data = Plant_height, col="darkgreen",xlab = "Temperature (C)", ylab = "log(Plant height)",pch=16)
abline(model, col = "red")
```

* [Linear regression](%base_url%/?linear-regression/)
* [Analysis of variance: single factor](%base_url%/?anova-single-factor/)
* [Analysis of variance: factorial](%base_url%/?anova-factorial/)  
* [Understanding interactions](%base_url%/?understanding-interactions/)
* [Interpreting coefficients in linear models](%base_url%/?how-to-interpret-linear-models/)

# [Go to next page - Linear Regression](%base_url%/?linear-regression)

<br>
`Last updated: May 17, 2020`