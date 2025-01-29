---
title: "Advanced Metrics Impact on WNBA MVP Rankings - Ordinal Regression"
author: "Carly Martin"
header-includes:
- \usepackage{float}
- \floatplacement{figure}{H}
output:
  pdf_document: default
  html_document:
    df_print: paged
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(MASS) 
library(car)
library(ggplot2)
library(brant)
library(VGAM)
WNBA_MVP <- read_excel("WNBA.MVP.xlsx")

#Renaming Columns 
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "TS%"] <- "TrueShoot_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "3PAr"] <- "ThreePoint_Att"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "TRB%"] <- "TotReb_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "AST%"] <- "Assist_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "STL%"] <- "Steal_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "BLK%"] <- "Block_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "TOV%"] <- "Turnover_Perc"
colnames(WNBA_MVP)[colnames(WNBA_MVP) == "USG%"] <- "Usage_Perc"

WNBA_MVP$Rank <- factor(WNBA_MVP$Rank, ordered = TRUE)
```

# Introduction

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Each year, the WNBA Most Valuable Player is determined by a committee of sportswriters and broadcasters. Each individual in the committee is asked to select their top 5 players from that season with their top choice receiving 10 points, 2nd receiving 7 points, 3rd receiving 5 points, 4th receiving 3 points and 5th receiving 1 point. This poses the questions, how does the panel decide who they are voting for? That is, Are certain statistics more valuable than others in the eyes of the committee? 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  This dataset is a collection of each of the top 10 WNBA MVP vote-getters from the last 20 seasons. The data includes each individual's advanced metrics. Advanced metrics are statistics that go beyond a traditional box score - points, rebounds, assists, turnovers, etc. - as they include stats such as Player Efficiency Rating and Win Shares. The data is collected by tracking on-court actions. These include points scored, rebounds, assists, steals, blocks, turnovers, etc. From these raw numbers, derived statistics such as effective field goal percentage, player efficiency, etc, are calculated.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The advanced metrics that appear in this dataset are player efficiency rating (`PER`), true shooting percentage (`TrueShoot_Perc`), three point attempt rate (`ThreePoint_Att`), free throw attempt rate (`FTr`), Offensive rebound percentage (`ORB%`), defensive rebound percentage (`DRB%`), total rebound percentage (`TotReb_Perc`), assist percentage (`Assist_Perc`), steal percentage (`Steal_Perc`), block percentage (`Block_Perc`), turnover percentage (`Turnover_Perc`), usage percentage (`Usage_Perc`), offensive win shares (`OWS`), defensive win shares (DWS), win shares (`WS`), and win shares per 48 minutes (`WS/48`). 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The NBA first started introduced these metrics during the 1996-97 season and the WNBA officially introduced it's own advanced stats pages in 2016. However, basketball-reference.com has data from all WNBA players and their advanced statistics dating back to its originating season (1997). The site is a well respected provider of sports statistics from Sports Reference, LLC that presents statistics for the WNBA, NBA, European Leagues and the ABA. Sports Reference, LLC also runs other similar pages such as Baseball Reference and Pro Football Reference. Additionally, the data includes each player's ranking in their respective MVP race and whether or not they won a championship that year.

# Methodology 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  An ordinal logistic regression (or proportional odds model) is used to examine this data. An ordinal logistic regression is used to analyze and model ordinal outcomes. Ordinal outcomes are ordered/leveled categorical variables (they are on an arbitrary scale). Therefore, an ordinal regression is useful when predicting the probability that an outcome will fall into a particular category. In this case, it will be used to predict where a player will fall in the MVP race (1-10). The ordinal outcome is a player's `rank`, and the independent variables are a player's advanced metrics. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The ordinal logistic regression uses log-odds of cumulative probabilities. First, let $Y =$ the ordinal dependent variable and $J$ represents the ordered categories. In this specific case,
    
  * $J = 1$ (First in Voting)
    
  * $J = 2$ (Second in Voting) ... 
        
  * $J = 10$ (Tenth in Voting)
    
For each category, $j$, the model defines the cumulative probability as: 
$$\pi_j = P(Y \le j |x)$$
From there, the cumulative probabilities are transformed using logit. The logit is the logarithm of the odds of the probability of a certain event occurring.

$$L_j(x) = logit(\pi_j) = log(\frac{\pi_j}{1 - \pi_j})$$

From this the multiple regression model becomes:

$$L_j(x) = \alpha_j - \beta_1(x_1) - \beta_2(x_2) - ... - \beta_k(x_k)$$

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Where $\alpha_j$ is the intercept or "cutoff" point specific to each category $j$ when all other predictors = 0. $\beta_k$ is the coefficient corresponding to the kth independent variable. It measures the effect of the independent variable on the cumulative log-odds of Y being in category j or below. Finally, $x_k$ represents the kth predictor variable. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  An ordinal logistic regression is quite similar to a standard logistic regression. Both models have categorical dependent variables, use link functions and use coefficients to measure the relationship between independent and dependent variables. The need for an ordinal regression arises when the dependent variable is categorical and non-binary. As a result, the ordinal regression yields necessary thresholds ($\alpha_j$) while a logistic regression does not. Additionally, the interpretation of the coefficient represents the log-odds of being in a lower category relative to a higher one - instead of representing the log-odds of being in either of the binary categories. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  In order to use an ordinal logistic regression, certain assumptions must be met. First, the dependent variable must be measured on an ordinal level. Next, one or more of the independent variables must either continuous, categorical or ordinal. There should be no multicollinearity. Finally, the proportional odds assumption states that each independent variable should have an identical effect on each cumulative split of the ordinal dependent variable. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  It is immediately it's clear that the complete model, containing all independent variables in the dataset is going to contain multicollinearity. For example, we would expect `ORB%` (offensive rebound percentage), `DRB%` (defensive rebound percentage), and `TotReb_Perc` to be highly correlated. This is due to the fact that `TotReb_Perc` includes both offensive rebounds and defensive rebounds in it's calculation. Additionally, `WS` (win shares) is also calculated using two other variables: `OWS` (offensive win shares) and `DWS` defensive win shares. `WS/48` is also calculated using `WS`. It's expected that these groups of variables are highly correlated. To check, each predictor's variance inflation factors will be examined: 

\vspace{0.5cm}

```{r, include=FALSE}
full_model <- polr(Rank ~ MP + PER + TrueShoot_Perc + ThreePoint_Att + FTr+ `ORB%` + 
                  `DRB%` + TotReb_Perc + Assist_Perc + Steal_Perc + Block_Perc 
                   + Turnover_Perc + Usage_Perc + OWS + DWS + WS + `WS/48`, 
                  data = WNBA_MVP, Hess = TRUE, method = 'logistic')
```

```{r}
vif(full_model)
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  If a predictor has a VIF that is greater than 5, multicollinearity is present. None of these predictors have a VIF $< 5$, in fact, most of them are extremely larger than 5. Therefore since `ORB%` and `DRB%` are included in `TRB%`, they will be discarded and only `TotReb_Perc` will be examined. Similarly, only `WS` and their effect on `Rank` will be examined.
$$L_j(x) = \alpha_j - \beta_1 (\text{MP}) - \beta_2 (\text{PER}) - \beta_3 (\text{TrueShoot\_Perc}) - \beta_4 (\text{ThreePoint\_Att}) - \beta_5 (\text{FTr}) - \beta_6 (\text{TotReb\_Perc}) - $$
$$\beta_7 (\text{Assist\_Perc}) - \beta_8 (\text{Steal\_Perc})- \beta_9 (\text{Block\_Perc}) - \beta_{10} (\text{Turnover\_Perc}) - \beta_{11} (\text{Usage\_Perc}) - \beta_{12} (\text{WS})$$
\vspace{0.1cm}

The model is fitted using either of the two R code chunks. 

```{r}
model <- polr(Rank ~ MP + PER + TrueShoot_Perc + ThreePoint_Att + FTr + 
                   TotReb_Perc + Assist_Perc + Steal_Perc
              + Block_Perc + Turnover_Perc + Usage_Perc + WS, 
              data = WNBA_MVP, 
              Hess = TRUE, method = 'logistic')
```

```{r}
po_model <- vglm(Rank ~ MP + PER + TrueShoot_Perc + ThreePoint_Att + FTr + 
                   TotReb_Perc + Assist_Perc + Steal_Perc
              + Block_Perc + Turnover_Perc + Usage_Perc + WS, 
              family = cumulative(parallel = TRUE), data = WNBA_MVP)
```

## Checking the New Model Assumptions

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The dependent variable is measured on an ordinal level. `Rank` is ordinal where athletes are categorized by their MVP race place. Additionally, all independent variables are continuous. Again, a VIF test will be used for multicollinearity:

```{r}
vif(model)
```
Even with the exclusion of specific variables there is still multicollinearity present. This is due multiple variables being correlated with the amount of minutes an athlete plays. 

A brant test can be used to examine the proportional odds assumption. 

* $H_0$: Proportional assumption holds: the relationship between the predictors and each pair of outcomes is the same

* $H_a$: Proportional assumption does not hold: the relationship between predictors and pair of outcomes is not the same

```{r}
brant(model)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Because Omnibus' p-value = 1 > 0.05, we fail to reject the null hypothesis. The proportional odds assumption holds for the model. We also fail to reject the null hypothesis for each independent variable except for `FTr` (free throw attempt rate). For all individual independent variables, (except `FTr`), the proportional odds assumption holds. 

Because minutes played `MP` is highly correlated to every predictor, removing it may solve the issue of multicollinearity

```{r}
model2 <- polr(Rank ~ PER + TrueShoot_Perc + ThreePoint_Att + FTr + 
                   TotReb_Perc + Assist_Perc + Steal_Perc
              + Block_Perc + Turnover_Perc + Usage_Perc + WS, 
              data = WNBA_MVP, 
              Hess = TRUE, method = 'logistic')
```

```{r}
vif(model2)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Now, each predictor is less than 5 (with the exception of `PER`). However, that predictor is only slightly greater than 5. Therefore removing `MP` eliminates most of the multicollinearity. However, the proportional odds assumption still needs to be checked:

```{r}
brant(model2)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Because Omnibus' p-value = 0 < 0.05, the null hypothesis is rejected. The proportional odds assumption does not for the model. However, we again fail to reject the null hypothesis for each independent variable except for `FTr` (free throw attempt rate). For all individual independent variables, (except `FTr`), the proportional odds assumption holds. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  By removing the `MP` (minutes played) variable from our model, the multicollinearity assumption will be met. However, doing so changes the results of the Brant test. Removing `MP` makes it so that the whole model violates the proportional odds assumption. This is intuitive as the minutes an athlete plays is an integral part of the calculations for most advanced statistical metrics. However, it's inclusion is also what keeps the effect of the independent variables consistent across thresholds. Therefore, the model including `MP` is still effective at predicting where players will fall in the MVP rankings but the significance of each independent variable may be unstable. 

# Results and Conclusions

```{r}
summary(po_model)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  According to the output, the estimated first threshold between a rank of 1 and 2 is statistically significant with a corresponding p-value of $0.00302 < \alpha = 0.01$. This indicates that the threshold is significantly different than zero. The same holds true for the second threshold in between a rank of 2 and 3 as that resulting p-value is 0.00938. The third and fourth thresholds are statistically significant at a significance level of $\alpha = 0.05$. Therefore, both thresholds are significantly different from zero. The fifth and sixth thresholds are somewhat statistically significant. They are only less than $\alpha = 0.1$ and therefore, are only statistically different than zero at that level. The seventh, eighth and ninth thresholds are not statistically significant indicating that there is no evidence that the thresholds differ from zero. This is understandable as committee members are more likely to align on their top vote-getters with more variability in the players earning the eighth, ninth and tenth place. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The predictors with the most significant effect on where a WNBA player will rank in the MVP race are Usage Percentage (`Usge_Perc`) and Win Shares (`WS`). Both variables have resulting p-values less than 0.01 at 0.00048 and 5.69e-07 respectively. These results logically sound as MVPs typically have the ball in their hands more than their teammates and contribute a high number of wins to their team.  

## Adding an Interaction

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  However, `PER` and `Usage_Perc` may have a joint influence on `Rank`. For example, a player with a high usage percentage and a low PER means that they often handle the ball, but are inefficient when doing so. Including an interaction term may introduce an effect that differs from their individual effects. Including it allows the model to capture the possibility that Usage percentage may have a stronger effect on `RANK` when PER is high and a weaker effect when PER is low. Their joint influence is best represented in an interaction term between the two. 
$$L_j(x) = \alpha_j - \beta_1 (\text{MP}) - \beta_2 (\text{PER}) - \beta_3 (\text{TrueShoot\_Perc}) - \beta_4 (\text{ThreePoint\_Att}) - \beta_5 (\text{FTr}) - \beta_6 (\text{TotReb\_Perc}) -$$
$$\beta_7 (\text{Assist\_Perc}) - \beta_8 (\text{Steal\_Perc}) - \beta_9 (\text{Block\_Perc}) - \beta_{10} (\text{Turnover\_Perc}) - \beta_{11} (\text{Usage\_Perc}) -$$
$$\beta_{12} (\text{WS}) - \beta_{13}(\text{PER})(\text{Usage\_Perc})$$
The assumptions still hold in this model.
```{r}
po_model_int <- vglm(Rank ~ MP + PER + TrueShoot_Perc + ThreePoint_Att + FTr + 
                   TotReb_Perc + Assist_Perc + Steal_Perc
                 + Block_Perc + Turnover_Perc + Usage_Perc + WS + Usage_Perc:PER, 
                 family = cumulative(parallel = TRUE), data = WNBA_MVP)
```

\vspace{0.5cm}

A lower AIC indicates a better-fitting model. 
```{r}
AIC(po_model)
AIC(po_model_int)
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; According to the AIC, the model with the interaction term is a better fit for the data than the model without the interaction. The AIC for the interaction term is 841.701 while the model without the term is 843.2098. 


## Results: Model with Interaction

```{r, echo = FALSE}
summary(po_model_int)
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  According to the new output, all estimated thresholds are now at least somewhat significant. The first three are significant at a $\alpha = 0.01$ significance level. All other thresholds are significant at $\alpha = 0.05$. Therefore, all thresholds are significantly different from 0 at a 0.05 significance level. Again, win shares (`WS`) is the most significant predictor of `Rank` while usage percentage is now only significant at a $\alpha = 0.05$ level. Variables with a positive coefficient in this model indicate that higher values for those variables are associated with a higher probability of being ranked closer to 1 (lower) than 10 (higher).

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  For example, for every one unit increase in win shares, the log-odds of being in a lower ranking (closer to 1) is 1.119619, holding all else constant. The exponentiated coefficient is 3.0637. This indicates that for each one unit increase in win shares, the odds of being in a lower category increase by a factor of 3.0637. Converting this to a probability results in:
$$\frac{3.0637}{3.0637 + 1} = 0.7539188$$
Therefore, for every one unit increase in win shares, the odds of being in a lower category (closer to 1) increase by 75.4\%, holding all else constant. 

Examining `usage_perc` yields that for every one percent increase, the log-odds of being in a lower ranking is 0.743002. Its corresponding exponentiated coefficient is 2.102237. Converting this to a probability results in:
$$\frac{2.102237}{2.102237 + 1} = 0.677652$$
Therefore, for every one percent increase in usage, the odds of being in a lower category (closer to 1) increase by 67.8\%, holding all else constant. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; One intriguing result comes from true shooting percentage. Its resulting coefficient is -15.359437. This means that for every one percent increase in true shooting, the log-odds of being in a higher category (because the coefficient is negative) is 15.359437, holding all else constant. This is quite surprising as its expected for players with higher true shooting percentages to perform better in the MVP race than those without. However, the resulting exponentiated coefficient is 2.135409e-07. This means that for every one percent increase in true shooting percentage, the odds of being in a higher category (closer to 10) increase by a factor of 2.135409e-07. Using this to calculate the probability results in:
$$\frac{2.135409 * 10^{-7}}{2.135409 * 10^{-7} + 1} \approx 2.135409 * 10^{-7}$$
Therefore, for every one percent increase in true shooting, the odds of being in a higher category (closer to 10) increase by 0.00002135409\%, holding all else constant. This number is quite low indicating that while `TrueShoot_Perc` is a significant predictor, a one percent increase in true shooting percentage does not change the probability of moving to a different category by much at all. 

# The Model as a Predictor

Caitlin Clark ended the 2024 season with the following advanced metrics: 

* `MP`: 1416

* `PER`: 18.8

* `TrueShoot_Perc`: 0.583 

* `ThreePoint_Att`: 0.612

* `FTr`: 0.310 

* `TotReb_Perc`: 9.4

* `Assist_Perc`: 39.1 

* `Steal_Perc`: 1.9

* `Block_Perc`: 1.7 

* `Turnover_Perc`: 25.3

* `Usage_Perc`: 27.7 

* `WS`: 3.0

```{r, echo = FALSE}
predict(object = po_model_int, newdata=data.frame(MP=1416, PER = 18.8, 
                                                  TrueShoot_Perc = 0.583, 
                                                  ThreePoint_Att = 0.612, 
                                                  FTr = 0.310,
                                                  TotReb_Perc = 9.4,
                                                  Assist_Perc = 39.1,
                                                  Steal_Perc= 1.9,
                                                  Block_Perc= 1.7,
                                                  Turnover_Perc= 25.3,
                                                  Usage_Perc= 27.7,
                                                  WS= 3.0), 
        type="response")
```
According to the model, she would have have a 2.05\% chance to win the MVP race and has around a 14-17\% chance to end up in 4th, 5th or 6th. Caitlin Clark ended up 4th in MVP voting in 2024. 

Aja Wilson ended the 2024 season with the following advanced metrics:

* `MP`: 1308 

* `PER`: 34.9

* `TrueShoot_Perc`: 0.591 

* `ThreePoint_Att`: 0.081

* `FTr`: 0.370 

* `TotReb_Perc`: 19.9

* `Assist_Perc`: 14.1

* `Steal_Perc`: 2.6

* `Block_Perc`: 6.3 

* `Turnover_Perc`: 5.3

* `Usage_Perc`: 32.2 

* `WS`: 10.9

```{r, echo = FALSE}
predict(object = po_model_int, newdata=data.frame(MP=1308, PER = 34.9, 
                                                  TrueShoot_Perc = 0.591, 
                                                  ThreePoint_Att = 0.081, 
                                                  FTr = 0.370,
                                                  TotReb_Perc = 19.9,
                                                  Assist_Perc = 14.1,
                                                  Steal_Perc= 2.6,
                                                  Block_Perc= 6.3,
                                                  Turnover_Perc= 5.3,
                                                  Usage_Perc= 32.2,
                                                  WS= 10.9), 
        type="response")
```
This model predicts that Wilson has a 93.8\% chance to win the MVP race. A'ja Wilson did win league MVP this year and she did so unanimously.

Finally, Nneka Ogwumike has been in the league for 12 years and won the MVP in 2016 with the LA Sparks. Her average advanced stats are:

* `MP`: 901.4615385 

* `PER`: 24.26923077

* `TrueShoot_Perc`: 0.6072307692 

* `ThreePoint_Att`: 0.09184615385

* `FTr`: 0.3110769231 

* `TotReb_Perc`: 14.94615385

* `Assist_Perc`: 12.76923077 

* `Steal_Perc`: 2.653846154

* `Block_Perc`: 1.638461538

* `Turnover_Perc`: 11.73846154

* `Usage_Perc`: 23.4 

* `WS`: 5.307692308

```{r, echo = FALSE}
predict(object = po_model_int, newdata=data.frame(MP=901.4615385, 
                                                  PER = 24.26923077, 
                                                  TrueShoot_Perc = 0.6072307692, 
                                                  ThreePoint_Att = 0.09184615385, 
                                                  FTr = 0.3110769231,
                                                  TotReb_Perc = 14.94615385,
                                                  Assist_Perc = 12.76923077,
                                                  Steal_Perc= 2.653846154,
                                                  Block_Perc= 1.638461538,
                                                  Turnover_Perc= 11.73846154,
                                                  Usage_Perc= 23.4,
                                                  WS= 5.307692308), 
        type="response")
```
If Ogwumike had a season in which her advanced metrics were consistent with her career averages, she would have a 0.7\% chance to win MVP and most likely end up in 9th place (20.18\% chance). 

# Discusssion

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; It appears as though the model is a solid predictor for where players will land in the MVP race given their advanced statistical metrics. The model also concluded that win shares have the most significant impact on where a player will rank in MVP voting. However, because multicollinearity is violated, the resulting standard errors could be inflated leading to unstable p-values. Therefore, this model may not be the best option when it comes to finding which metrics are statistically significant predictors of WNBA MVP rankings.

