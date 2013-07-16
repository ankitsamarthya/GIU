import re
str1 = """[[1]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9412  -0.1609  -0.0442  -0.0067   5.0284  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   0.3244     0.4540   0.715 0.474791    
layer.1      -3.9006     1.1237  -3.471 0.000518 ***
layer.2      -2.2876     1.4451  -1.583 0.113421    
layer.3     -17.1203     0.4919 -34.807  < 2e-16 ***
layer.4      -1.9220     0.3237  -5.938 2.89e-09 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12080.9  on 59595  degrees of freedom
Residual deviance:  8456.8  on 59591  degrees of freedom
AIC: 8466.8

Number of Fisher Scoring iterations: 10


[[2]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.4237  -0.4178   0.3053   0.6711   4.4025  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)   7.80440    0.12318   63.36   <2e-16 ***
layer.1     -36.25265    0.92065  -39.38   <2e-16 ***
layer.2     -20.42899    0.38981  -52.41   <2e-16 ***
layer.3      -2.17595    0.04956  -43.91   <2e-16 ***
layer.4       5.81373    0.11168   52.06   <2e-16 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 80635  on 59595  degrees of freedom
Residual deviance: 47233  on 59591  degrees of freedom
AIC: 47243

Number of Fisher Scoring iterations: 8


[[3]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-6.2657  -0.2753  -0.1974  -0.1374   2.9991  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -12.83672    0.15375 -83.490  < 2e-16 ***
layer.1      22.10210    0.68906  32.076  < 2e-16 ***
layer.2      29.10117    0.45002  64.667  < 2e-16 ***
layer.3       0.42428    0.09951   4.264 2.01e-05 ***
layer.4       0.04659    0.15366   0.303    0.762    
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 57997  on 59595  degrees of freedom
Residual deviance: 20759  on 59591  degrees of freedom
AIC: 20769

Number of Fisher Scoring iterations: 8


[[4]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7521  -0.2462  -0.2050  -0.1508   3.2818  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -4.3682     0.1327 -32.918  < 2e-16 ***
layer.1     -12.5703     1.1437 -10.991  < 2e-16 ***
layer.2       4.6323     0.4257  10.883  < 2e-16 ***
layer.3      -0.4594     0.1230  -3.733 0.000189 ***
layer.4      -3.8385     0.2702 -14.206  < 2e-16 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12433  on 59595  degrees of freedom
Residual deviance: 11959  on 59591  degrees of freedom
AIC: 11969

Number of Fisher Scoring iterations: 8


[[5]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.5032   0.0000   0.0000   0.0000   3.0631  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)   -49.82      25.26  -1.973  0.04855 * 
layer.1       -61.62      81.77  -0.754  0.45113   
layer.2       -38.16      50.43  -0.757  0.44925   
layer.3        59.76      20.92   2.857  0.00427 **
layer.4        26.86      10.51   2.555  0.01061 * 
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 103.86  on 59595  degrees of freedom
Residual deviance:  50.08  on 59591  degrees of freedom
AIC: 60.08

Number of Fisher Scoring iterations: 18


[[6]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.5307  -0.0481  -0.0261  -0.0100   3.9736  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -8.1710     0.4303 -18.990  < 2e-16 ***
layer.1     -19.0012     4.9224  -3.860 0.000113 ***
layer.2      12.4413     1.5409   8.074 6.79e-16 ***
layer.3      -6.7295     1.0064  -6.687 2.28e-11 ***
layer.4      -6.7021     1.6720  -4.008 6.11e-05 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 961.84  on 59595  degrees of freedom
Residual deviance: 844.60  on 59591  degrees of freedom
AIC: 854.6

Number of Fisher Scoring iterations: 11


[[7]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.8187  -0.4185  -0.3214  -0.2647   2.9131  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -3.88872    0.08795 -44.216  < 2e-16 ***
layer.1     -6.54592    0.56516 -11.582  < 2e-16 ***
layer.2      0.88325    0.25964   3.402 0.000669 ***
layer.3      2.29566    0.07438  30.865  < 2e-16 ***
layer.4      0.97500    0.10228   9.533  < 2e-16 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 28743  on 59595  degrees of freedom
Residual deviance: 27451  on 59591  degrees of freedom
AIC: 27461

Number of Fisher Scoring iterations: 7


[[8]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9000  -0.2536  -0.1638  -0.0844   3.4456  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   0.6918     0.2383   2.903   0.0037 ** 
layer.1       5.2862     0.5225  10.117   <2e-16 ***
layer.2     -13.1665     0.7281 -18.084   <2e-16 ***
layer.3       1.4096     0.1061  13.281   <2e-16 ***
layer.4      -9.5852     0.4005 -23.934   <2e-16 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 14553  on 59595  degrees of freedom
Residual deviance: 12492  on 59591  degrees of freedom
AIC: 12502

Number of Fisher Scoring iterations: 8


[[9]]

Call:
glm(formula = formula(frml), family = binomial, data = dta, na.action = "na.omit")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8424  -0.0503  -0.0003   0.0000   8.4904  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)    1.4475     0.1234  11.727  < 2e-16 ***
layer.1       -2.5639     0.5437  -4.716 2.41e-06 ***
layer.2       -2.2964     0.3812  -6.025 1.69e-09 ***
layer.3        0.7028     0.0862   8.152 3.57e-16 ***
layer.4     -130.1836     1.9481 -66.828  < 2e-16 ***
---
Signif. codes:  0 ‰??***‰?? 0.001 ‰??**‰?? 0.01 ‰??*‰?? 0.05 ‰??.‰?? 0.1 ‰?? ‰?? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 34249  on 59595  degrees of freedom
Residual deviance: 12749  on 59591  degrees of freedom
AIC: 12759

Number of Fisher Scoring iterations: 11"""

customlist = re.split('\[\[\d{1,}\]\]',str1)

layer1=[]
layer2=[]
layer3=[]
layer4=[]
list1=[]
list2=[]
list3=[]
list4=[]

for i in 1,9:
    layer1.append(customlist[i][customlist[i].index("layer.1")+len("layer.1"):customlist[i].index("layer.2")])
    layer2.append(customlist[i][customlist[i].index("layer.2")+len("layer.2"):customlist[i].index("layer.3")])
    layer3.append(customlist[i][customlist[i].index("layer.3")+len("layer.3"):customlist[i].index("layer.4")])
    layer4.append(customlist[i][customlist[i].index("layer.4")+len("layer.4"):customlist[i].index("---")])

for i in 0,8:
    list1.append(layer1[i].split())
    list2.append(layer2[i].split())
    list3.append(layer3[i].split())
    list4.append(layer4[i].split())

print list1[0][0]
