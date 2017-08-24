


---
title: "Leg Length vs Nest Size Stepwise model regression"
author: "Ruth Sharpe"
date: "Run on 23 August, 2017 at 2017-08-23 17:22:04"
geometry: margin=1cm
header-includes:
    - \usepackage{float}
---



Model Reduction
=====================


[1] "number of terms"
[1] 5
[1] "pvalue"
[1] 1
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrl}
  \hline
 & Sum Sq & Mean Sq & NumDF & DenDF & F.value & p\_value \\ 
  \hline
logCtFm & 0.00 & 0.00 &   1 & 30.59 & 0.89 & 0.353  \\ 
  InstarNumber & 1.40 & 1.40 &   1 & 1265.81 & 1537.59 & 0.000*** \\ 
  I(logCtFm\verb|^|2) & 0.00 & 0.00 &   1 & 22.54 & 0.02 & 0.889 RMVD \\ 
  InstarNumber:InstarSex & 0.00 & 0.00 &   1 & 1258.77 & 0.40 & 0.528  \\ 
  logCtFm:InstarNumber & 0.02 & 0.02 &   1 & 1268.93 & 26.26 & 0.000*** \\ 
  logCtFm:InstarNumber:InstarSex & 0.00 & 0.00 &   1 & 1257.70 & 3.45 & 0.064. \\ 
   \hline
\end{tabular}
\end{table}
[1] "term with highest p value is: I(logCtFm^2)"
[1] "number of terms"
[1] 6
[1] "pvalue"
[1] 0.8891712
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrl}
  \hline
 & Sum Sq & Mean Sq & NumDF & DenDF & F.value & p\_value \\ 
  \hline
logCtFm & 0.01 & 0.01 &   1 & 284.30 & 7.42 & 0.007** \\ 
  InstarNumber & 1.42 & 1.42 &   1 & 1246.77 & 1558.08 & 0.000*** \\ 
  InstarNumber:InstarSex & 0.00 & 0.00 &   1 & 1259.53 & 0.40 & 0.526 RMVD \\ 
  logCtFm:InstarNumber & 0.02 & 0.02 &   1 & 1257.50 & 26.40 & 0.000*** \\ 
  logCtFm:InstarNumber:InstarSex & 0.00 & 0.00 &   1 & 1258.33 & 3.46 & 0.063. \\ 
   \hline
\end{tabular}
\end{table}
[1] "term with highest p value is: InstarNumber:InstarSex"
[1] "number of terms"
[1] 5
[1] "pvalue"
[1] 0.5257336
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrl}
  \hline
 & Sum Sq & Mean Sq & NumDF & DenDF & F.value & p\_value \\ 
  \hline
logCtFm & 0.01 & 0.01 &   1 & 282.21 & 7.32 & 0.007** \\ 
  InstarNumber & 1.56 & 1.56 &   1 & 1228.26 & 1712.07 & 0.000*** \\ 
  logCtFm:InstarNumber & 0.02 & 0.02 &   1 & 1245.66 & 26.67 & 0.000*** \\ 
  logCtFm:InstarNumber:InstarSex & 0.03 & 0.03 &   1 & 1255.23 & 30.77 & 0.000*** \\ 
   \hline
\end{tabular}
\end{table}
[1] "i = 1"
[1] "term with highest p value is: logCtFm:InstarNumber"

Final Model is:  logLeg ~ logCtFm + InstarNumber + 1 | NestID + logCtFm:InstarNumber:InstarSex\begin{table}[H]
\centering
\begin{tabular}{lrrrrrr}
  \hline
 & Sum Sq & Mean Sq & NumDF & DenDF & F.value & Pr($>$F) \\ 
  \hline
logCtFm & 0.01 & 0.01 & 1.00 & 282.21 & 7.32 & 0.0072 \\ 
  InstarNumber & 1.56 & 1.56 & 1.00 & 1228.26 & 1712.07 & 0.0000 \\ 
  logCtFm:InstarNumber:InstarSex & 0.05 & 0.02 & 2.00 & 1250.31 & 27.38 & 0.0000 \\ 
   \hline
\end{tabular}
\end{table}

\pagebreak

Checking full model fit
--------------------



```
log(LegLength)=log(ColonySize) + InstarAge + InstarAge:InstarSex + log(ColonySize):InstarAge + log(ColonySize):InstarAge:InstarSex + Ilog(ColonySize)2 + (1|Colony)-Ilog(ColonySize)2-InstarAge:InstarSex-log(ColonySize):InstarAge
```

![plot of chunk ModelFit](figure/ModelFit-1.png)![plot of chunk ModelFit](figure/ModelFit-2.png)
\pagebreak


Graph with full model superimposed
====================
 


```
Model:
logLeg ~ logCtFm + InstarNumber + InstarNumber:InstarSex + logCtFm:InstarNumber + logCtFm:InstarNumber:InstarSex + I(logCtFm^2) + (1 | NestID) - I(logCtFm^2) - InstarNumber:InstarSex - logCtFm:InstarNumber
```

```
Note: If line on graph is blue R could not plot the lmer, plotting a simple lm instead[1] "lmer"
```

![plot of chunk Graph](figure/Graph-1.png)

