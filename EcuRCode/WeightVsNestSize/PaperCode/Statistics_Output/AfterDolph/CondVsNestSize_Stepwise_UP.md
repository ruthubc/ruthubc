


---
title: "Condition vs Nest Size Up Step model addition"
author: "Ruth Sharpe"
date: "Run on 23 August, 2017 at 2017-08-23 17:29:33"
geometry: margin=1cm
header-includes:
    - \usepackage{float}
---



Model Addition
=====================


+ logCtFm + InstarNumber (1 | NestID) 
 vs 
 + logCtFm + InstarNumber + (1 | NestID) logCtFm:InstarNumber
 *****************NOT SIGNIFICANT --- STOP HERE ************** 
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrrrrl}
  \hline
 & Df & AIC & BIC & logLik & deviance & Chisq & Chi Df & Pr($>$Chisq) & stars \\ 
  \hline
object & 5.00 & -2085.73 & -2060.01 & 1047.87 & -2095.73 &  &  &  &  \\ 
  ..1 & 6.00 & -2084.93 & -2054.06 & 1048.47 & -2096.93 & 1.20 & 1.00 & 0.27 &   \\ 
   \hline
\end{tabular}
\end{table}
+ logCtFm + InstarNumber + (1 | NestID) logCtFm:InstarNumber 
 vs 
 + logCtFm + InstarNumber + (1 | NestID) + logCtFm:InstarNumber InstarNumber:InstarSex
 *****************NOT SIGNIFICANT --- STOP HERE ************** 
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrrrrl}
  \hline
 & Df & AIC & BIC & logLik & deviance & Chisq & Chi Df & Pr($>$Chisq) & stars \\ 
  \hline
object & 6.00 & -2084.93 & -2054.06 & 1048.47 & -2096.93 &  &  &  &  \\ 
  ..1 & 7.00 & -2083.06 & -2047.05 & 1048.53 & -2097.06 & 0.13 & 1.00 & 0.72 &   \\ 
   \hline
\end{tabular}
\end{table}
+ logCtFm + InstarNumber + (1 | NestID) + logCtFm:InstarNumber InstarNumber:InstarSex 
 vs 
 + logCtFm + InstarNumber + (1 | NestID) + logCtFm:InstarNumber + InstarNumber:InstarSex logCtFm:InstarNumber:InstarSex
 *****************NOT SIGNIFICANT --- STOP HERE ************** 
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrrrrl}
  \hline
 & Df & AIC & BIC & logLik & deviance & Chisq & Chi Df & Pr($>$Chisq) & stars \\ 
  \hline
object & 7.00 & -2083.06 & -2047.05 & 1048.53 & -2097.06 &  &  &  &  \\ 
  ..1 & 8.00 & -2083.39 & -2042.24 & 1049.70 & -2099.39 & 2.33 & 1.00 & 0.13 &   \\ 
   \hline
\end{tabular}
\end{table}
+ logCtFm + InstarNumber + (1 | NestID) + logCtFm:InstarNumber + InstarNumber:InstarSex logCtFm:InstarNumber:InstarSex 
 vs 
 + logCtFm + InstarNumber + (1 | NestID) + logCtFm:InstarNumber + InstarNumber:InstarSex + logCtFm:InstarNumber:InstarSex I(logCtFm^2)
 *****************NOT SIGNIFICANT --- STOP HERE ************** 
\begin{table}[H]
\centering
\begin{tabular}{rrrrrrrrrl}
  \hline
 & Df & AIC & BIC & logLik & deviance & Chisq & Chi Df & Pr($>$Chisq) & stars \\ 
  \hline
object & 8.00 & -2083.39 & -2042.24 & 1049.70 & -2099.39 &  &  &  &  \\ 
  ..1 & 9.00 & -2081.44 & -2035.14 & 1049.72 & -2099.44 & 0.05 & 1.00 & 0.82 &   \\ 
   \hline
\end{tabular}
\end{table}
Final model is  condResiduals ~ logCtFm + InstarNumber + logCtFm:InstarNumber + InstarNumber:InstarSex + (1 | NestID)

\pagebreak


Checking full model fit
--------------------



```
Condition=log(ColonySize) + InstarAge + log(ColonySize):InstarAge + InstarAge:InstarSex + (1|Colony)
```

![plot of chunk ModelFit](figure/ModelFit-1.png)![plot of chunk ModelFit](figure/ModelFit-2.png)



Graph with full model superimposed
====================
 


```
Model:
condResiduals ~ logCtFm + InstarNumber + logCtFm:InstarNumber + InstarNumber:InstarSex + (1 | NestID)
```

```
Note: If line on graph is blue R could not plot the lmer, plotting a simple lm instead[1] "lmer"
```

```
Warning: Removed 1 rows containing missing values (geom_point).

Warning: Removed 1 rows containing missing values (geom_point).

Warning: Removed 1 rows containing missing values (geom_point).
```

![plot of chunk Graph](figure/Graph-1.png)



