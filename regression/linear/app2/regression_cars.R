
R version 3.4.1 (2017-06-30) -- "Single Candle"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

> head(cars)
  speed dist
1     4    2
2     4   10
3     7    4
4     7   22
5     8   16
6     9   10
> scatter.smooth(x=cars$speed, y=cars$dist, main=)
Error in as.graphicsAnnot(main) : argument is missing, with no default
>
> scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")
> par(mfrow=c(1,2))
> boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats($
> par(mfrow=c(1, 2))
> boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats($
> boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats($
> boxplot(cars$dist, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(c$
> boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats($
> par(mfrow=c(1, 2))
> boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats($
> boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stat$
> set.seed(100)
>
>
>
>
> trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))
> trainingData <- cars[trainingRowIndex, ]
> testData <- cars[-trainingRowIndex, ]
> lmMod <- lm(dist ~ speed, data=trainingData)
> distPred <- predict(lmMod, testData)
> summary(lmMod)

Call:
lm(formula = dist ~ speed, data = trainingData)

Residuals:
    Min      1Q  Median      3Q     Max
-23.350 -10.771  -2.137   9.255  42.231

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -22.657      7.999  -2.833  0.00735 **
speed          4.316      0.487   8.863 8.73e-11 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 15.84 on 38 degrees of freedom
Multiple R-squared:  0.674,     Adjusted R-squared:  0.6654
F-statistic: 78.56 on 1 and 38 DF,  p-value: 8.734e-11

> AIC(lmMod)
[1] 338.4489
>
>
> axtuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred$
> correlation_accuracy <- cor(actuals_preds)
Error in is.data.frame(x) : object 'actuals_preds' not found
> correlation_accuracy <- cor(axtuals_preds)
> head(axtuals_preds)
   actuals predicteds
1        2  -5.392776
4       22   7.555787
8       26  20.504349
20      26  37.769100
26      54  42.085287
31      50  50.717663


> min_max_accuracy <- mean(apply(axtuals_preds, 1, min) / apply(actuals_preds,$
Error in apply(actuals_preds, 1, max) : object 'actuals_preds' not found
> min_max_accuracy <- mean(apply(axtuals_preds, 1, min) / apply(axtuals_preds,$
> head(min_max_accuracy)
[1] 0.3800489
> min_max_accuracy
[1] 0.3800489
> min_max_accuracy <- mean(apply(axtuals_preds, 1, min) / apply(axtuals_preds,$
> mape <- mean(abs((axtuals_preds$predicteds - axtual_preds$actuals))/axtual_p$
Error in mean(abs((axtuals_preds$predicteds - axtual_preds$actuals))/axtual_pred
s$actuals) :
  object 'axtual_preds' not found
> mape <- mean(abs((axtuals_preds$predicteds - axtuals_preds$actuals))/axtual_$
Error in mean(abs((axtuals_preds$predicteds - axtuals_preds$actuals))/axtual_pre
ds$actuals) :
  object 'axtual_preds' not found
> mape <- mean(abs((axtuals_preds$predicteds - axtuals_preds$actuals))/axtuals$
> library(DAAG)
Error in library(DAAG) : there is no package called 'DAAG'
> library(DAAG)
Loading required package: lattice
Warning message:
package 'DAAG' was built under R version 3.4.4
> cvResults <- suppressWarning(CVlm(df=cars, form.lm=dist ~ speed, m=5, dots=F$
Error in suppressWarning(CVlm(df = cars, form.lm = dist ~ speed, m = 5,  :
  could not find function "suppressWarning"
