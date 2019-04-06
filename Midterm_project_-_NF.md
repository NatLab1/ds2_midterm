Midterm project - Nathalie
================
Nathalie Fadel
4/4/2019

### Import & tidy dataset

``` r
re_data = read_excel("real_estate_valuation_data_set.xlsx") %>%
  janitor::clean_names() %>%
  select(-no)

re_data = 
  re_data %>%
  rename(house_price = y_house_price_of_unit_area, 
         transaction_date = x1_transaction_date,
         house_age = x2_house_age,
         distance_mrt = x3_distance_to_the_nearest_mrt_station,
         conv_stores = x4_number_of_convenience_stores,
         latitude = x5_latitude,
         longitude = x6_longitude
         )
#renamed variables

summary(re_data)
```

    ##  transaction_date   house_age       distance_mrt      conv_stores    
    ##  Min.   :2013     Min.   : 0.000   Min.   :  23.38   Min.   : 0.000  
    ##  1st Qu.:2013     1st Qu.: 9.025   1st Qu.: 289.32   1st Qu.: 1.000  
    ##  Median :2013     Median :16.100   Median : 492.23   Median : 4.000  
    ##  Mean   :2013     Mean   :17.713   Mean   :1083.89   Mean   : 4.094  
    ##  3rd Qu.:2013     3rd Qu.:28.150   3rd Qu.:1454.28   3rd Qu.: 6.000  
    ##  Max.   :2014     Max.   :43.800   Max.   :6488.02   Max.   :10.000  
    ##     latitude       longitude      house_price    
    ##  Min.   :24.93   Min.   :121.5   Min.   :  7.60  
    ##  1st Qu.:24.96   1st Qu.:121.5   1st Qu.: 27.70  
    ##  Median :24.97   Median :121.5   Median : 38.45  
    ##  Mean   :24.97   Mean   :121.5   Mean   : 37.98  
    ##  3rd Qu.:24.98   3rd Qu.:121.5   3rd Qu.: 46.60  
    ##  Max.   :25.01   Max.   :121.6   Max.   :117.50

### Scatter Plot

``` r
data(re_data)
```

    ## Warning in data(re_data): data set 're_data' not found

``` r
# matrix of predictors 
x <- model.matrix(house_price~.,re_data)[,-1]
# vector of response
y <- re_data$house_price

theme1 <- trellis.par.get()
theme1$plot.symbol$col <- rgb(.2, .4, .2, .5)
theme1$plot.symbol$pch <- 16
theme1$plot.line$col <- rgb(.8, .1, .1, 1)
theme1$plot.line$lwd <- 2
theme1$strip.background$col <- rgb(.0, .2, .6, .2)
trellis.par.set(theme1)
featurePlot(x, y, plot = "scatter", labels = c("","Y"),
            type = c("p"), layout = c(4, 2))
```

![](Midterm_project_-_NF_files/figure-markdown_github/unnamed-chunk-2-1.png) We can see that there may be a non-linear relationship between house price and distance to MRT station, and between house price and house age. We will test both of these terms independently and together as spline terms in GAM models.

### GAM models

``` r
gam.m1 <- gam(house_price ~ transaction_date + house_age + distance_mrt + conv_stores + latitude + longitude, data = re_data)

gam.m2 <- gam(house_price ~ transaction_date + house_age + s(distance_mrt) + conv_stores + latitude + longitude, data = re_data)
#Spline term applied to distance to mrt station

gam.m3 <- gam(house_price ~ transaction_date + s(house_age) + distance_mrt + conv_stores + latitude + longitude, data = re_data)
#spline term applied to house age

gam.m4 <- gam(house_price ~ transaction_date + s(house_age) + s(distance_mrt) + conv_stores + latitude + longitude, data = re_data)
#Both house age and distance to mrt station are splined

anova(gam.m1, gam.m2, gam.m3, gam.m4, test = "F")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: house_price ~ transaction_date + house_age + distance_mrt + conv_stores + 
    ##     latitude + longitude
    ## Model 2: house_price ~ transaction_date + house_age + s(distance_mrt) + 
    ##     conv_stores + latitude + longitude
    ## Model 3: house_price ~ transaction_date + s(house_age) + distance_mrt + 
    ##     conv_stores + latitude + longitude
    ## Model 4: house_price ~ transaction_date + s(house_age) + s(distance_mrt) + 
    ##     conv_stores + latitude + longitude
    ##   Resid. Df Resid. Dev      Df Deviance      F    Pr(>F)    
    ## 1    407.00      31931                                      
    ## 2    402.17      24791  4.8295   7140.1 25.515 < 2.2e-16 ***
    ## 3    403.93      29039 -1.7641  -4247.5 41.552 1.487e-15 ***
    ## 4    399.93      23273  4.0061   5766.0 24.839 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(gam.m2)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## house_price ~ transaction_date + house_age + s(distance_mrt) + 
    ##     conv_stores + latitude + longitude
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -2.390e+04  6.324e+03  -3.780 0.000181 ***
    ## transaction_date  6.268e+00  1.385e+00   4.527 7.88e-06 ***
    ## house_age        -2.533e-01  3.546e-02  -7.144 4.27e-12 ***
    ## conv_stores       2.976e-01  1.902e-01   1.565 0.118446    
    ## latitude          2.897e+02  4.099e+01   7.068 6.95e-12 ***
    ## longitude         3.365e+01  4.527e+01   0.743 0.457748    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                   edf Ref.df     F p-value    
    ## s(distance_mrt) 4.785  5.829 27.42  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.668   Deviance explained = 67.6%
    ## GCV = 63.129  Scale est. = 61.484    n = 414

``` r
summary(gam.m3)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## house_price ~ transaction_date + s(house_age) + distance_mrt + 
    ##     conv_stores + latitude + longitude
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -1.615e+04  6.531e+03  -2.472 0.013839 *  
    ## transaction_date  5.489e+00  1.493e+00   3.677 0.000268 ***
    ## distance_mrt     -3.789e-03  6.979e-04  -5.429 9.78e-08 ***
    ## conv_stores       1.073e+00  1.822e-01   5.890 8.13e-09 ***
    ## latitude          2.353e+02  4.271e+01   5.508 6.47e-08 ***
    ## longitude        -6.092e+00  4.670e+01  -0.130 0.896266    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##               edf Ref.df     F p-value    
    ## s(house_age) 3.26  4.065 22.18  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.612   Deviance explained =   62%
    ## GCV = 73.388  Scale est. = 71.747    n = 414

``` r
summary(gam.m4)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## house_price ~ transaction_date + s(house_age) + s(distance_mrt) + 
    ##     conv_stores + latitude + longitude
    ## 
    ## Parametric coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -23376.885   6132.634  -3.812  0.00016 ***
    ## transaction_date      6.370      1.346   4.733 3.07e-06 ***
    ## conv_stores           0.347      0.185   1.876  0.06138 .  
    ## latitude            289.301     39.732   7.281 1.76e-12 ***
    ## longitude            27.699     43.804   0.632  0.52753    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                   edf Ref.df     F  p-value    
    ## s(house_age)    2.902  3.622 22.18 5.43e-15 ***
    ## s(distance_mrt) 4.463  5.450 24.50  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.687   Deviance explained = 69.6%
    ## GCV = 59.729  Scale est. = 57.945    n = 414

``` r
#GAM M4 has best R^2 value

plot(gam.m2)
```

![](Midterm_project_-_NF_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot(gam.m3)
```

![](Midterm_project_-_NF_files/figure-markdown_github/unnamed-chunk-3-2.png)

### K-Nearest Neighbors fit

``` r
set.seed(123)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = re_data$house_price, p = 0.80, list = FALSE)
training <- re_data[indxTrain,]
testing <- re_data[-indxTrain,]

dim(training); dim(testing);
```

    ## [1] 332   7

    ## [1] 82  7

``` r
#train: 332, 7
#test: 82, 7

trainX <- training[,names(training) != "house_price"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues
```

    ## Created from 332 samples and 6 variables
    ## 
    ## Pre-processing:
    ##   - centered (6)
    ##   - ignored (0)
    ##   - scaled (6)

``` r
set.seed(1)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
knn_fit <- train(house_price ~., data = training, method = "knn",
            trControl = trctrl,
            preProcess = c("center", "scale"),
            tuneLength = 10)
knn_fit
```

    ## k-Nearest Neighbors 
    ## 
    ## 332 samples
    ##   6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 297, 300, 299, 298, 300, 299, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   RMSE      Rsquared   MAE     
    ##    5  8.074996  0.6578907  5.610875
    ##    7  8.014371  0.6623392  5.562251
    ##    9  8.113341  0.6527168  5.605927
    ##   11  8.250377  0.6406296  5.702476
    ##   13  8.268577  0.6387480  5.722598
    ##   15  8.218981  0.6435400  5.732524
    ##   17  8.248154  0.6417133  5.789684
    ##   19  8.241602  0.6425379  5.796926
    ##   21  8.276370  0.6405283  5.841319
    ##   23  8.292331  0.6406226  5.850387
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was k = 7.

``` r
# Plot model error RMSE vs different values of k
ggplot(knn_fit)
```

![](Midterm_project_-_NF_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Best tuning parameter k that minimizes the RMSE
knn_fit$bestTune
```

    ##   k
    ## 2 7

``` r
# Make predictions on the test data
knn_predict <- knn_fit %>% predict(testing)
# Compute the prediction error RMSE
RMSE(knn_predict, testing$house_price)
```

    ## [1] 7.95371
