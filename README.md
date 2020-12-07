## Coles or Woolworths?

## Executive Statement

**Aim:** 

The aim of this investigation was to determine if there was a statistically significant difference in the mean price between products sold at coles and woolworths. Or to put it more simply, which major supermarket is cheaper.

**Sample:** 

To obtain price information for identical products between the two companies, we wrote a javascript program to download the entire website [http://www.grocerycop.com.au] (http://www.grocerycop.com.au). This form of convenience sampling allowed us to gather sample data from the population in a fast and efficient manner.
The variables in the data were page number: item number, category, title, company, price and price difference.
We obtained 5552 paired samples across 10 different categories: Fridge, Bakery, Fruit-vegetables, Pantry, Freezer, Baby-health-beauty, Meat-seafood, Entertainment-international-food, Clothing-household-pet, Tobacco-drinks
Procedure: After some small manipulations and tidying of the dataset, we performed a paired samples t-test to determine if there was a statistically significant difference between the mean price of products. Our null hypothesis assumes there is no difference in means, while the alternative hypothesis states the opposite.

**Main Findings:**

When taken as a whole, a paired-samples t-test did not find evidence of a statistically significant difference between the prices of identical products sold at Coles and Woolworths. t(df=5551) = 1.947, p > 0.01, 99% CI [-0.02 0.142].
When divided by category, a paired-samples t-test did find evidence of a statistically significant difference in 4 catgories: drinks-tobacco, entertainment-international-food, baby-health-beauty, pantry

**Conclusion:**
The prices between the Coles and Woolworths are kept close together due to competition. Our statistical analysis did find a statistically significance difference in the prices of products in some categories. Since supermarkets change their prices daily, this analysis does not provide *practical significance* and we could not use our findings to predict which store would be cheaper on any given day.

## Table of Contents
1. Executive Summary
2. Load Packages and Data
3. Summary Statistics
4. Hypothesis Test
5. Interpretation
6. Further Interpretation (by category)
7. Discussion

## Load Packages and Data

```{r message=FALSE, warning=FALSE}
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(broom)
library(car)
library(stats)
library(latex2exp)

grocerycop <- read_csv("grocerycop.csv", col_types = cols(category = col_factor()))

# Removed all rows that contain NA, leaving only paired data
pairs = na.omit(grocerycop)
```

## Summary Statistics

```
g <- pairs %>% gather(company, value, coles:woolworths, factor_key = T)

g %>% group_by(company) %>% summarise(
  N   = n(),
  MEAN = mean(value, na.rm = T), 
  SD  = sd(value, na.rm = T),
  MIN = min(value, na.rm = T), 
  Q1  = quantile(value, .25, na.rm = T), 
  MEDIAN = quantile(value, .5, na.rm = T), 
  Q3   = quantile(value, .75, na.rm = T), 
  MAX  = max(value, na.rm = T), 
  IQR  = Q3-Q1, 
  LF = Q1 - 1.5*IQR,
  UF = Q3 + 1.5*IQR,
  LOUT = sum(value < LF, na.rm = T),
  UOUT = sum(value > UF, na.rm = T)
) %>% arrange(company) %>% modify_if(is.numeric, round, 2)

ggplot(g, aes(x = company, y = value)) + geom_boxplot(outlier.colour = "red") + coord_flip()
```
<img width="916" alt="1" src="https://user-images.githubusercontent.com/65587875/101334606-35153c00-38cc-11eb-9675-bc10a41e35b6.png">

<img width="912" alt="2" src="https://user-images.githubusercontent.com/65587875/101334604-347ca580-38cc-11eb-9bdd-6ab3592ca7b7.png">


From the above side-by-side boxplot, it appears that the spread of the data is very similar between coles and woolworths. They both appear to have similar means, interquartile ranges and number of outliers. The summary statistics table above confirms quantitatively that the data for coles and woolworths follow similar trends. 

```
# add the difference column
pairs = pairs %>% mutate(diffcw = coles - woolworths)

summary <- pairs %>% summarise(
  N   = n(),
  MEAN = mean(diffcw, na.rm = T), 
  SD  = sd(diffcw, na.rm = T),
  MIN = min(diffcw, na.rm = T), 
  Q1  = quantile(diffcw, .25, na.rm = T), 
  MEDIAN = quantile(diffcw, .5, na.rm = T), 
  Q3   = quantile(diffcw, .75, na.rm = T), 
  MAX  = max(diffcw, na.rm = T), 
  IQR  = Q3-Q1
) 

summary
```

The statistical summary above shows that the total difference in means is
$\mu_\Delta = \mu_c - \mu_w$ = 0.061 with sd = 2.346.

In order to determine if this difference is statistically significant, we conduct a paired samples t-test. 

## Hypothesis Test

For our paired samples, we want to determine if the difference between the population means is statistically significant.
Our null hypothesis assumes there is no statistically significant difference in means, while the alternative hypothesis states the opposite. 

$$ H_0 : \mu_\Delta = 0 $$

$$ H_A : \mu_\Delta \neq 0 $$

### Significance Level

 * Due to our large sample size we choose to use a significance level $\alpha$ of 0.01

### Assumptions

 * This data is continuous 
   - TRUE
 
 * The sample is representative of a randomly selected portion of the total population
   - We expect our sample is representative
   
 * the sample size is large enough for CLT to be effective
   - Our sample is large with n=`r summary$N`
   
 * The difference in the population means is approximately normally distributed
   - This histogram below shows that price differences is approximately normally distributed
   
```
ggplot(pairs, aes(x = diffcw)) +
  geom_histogram(aes(y=..density..), binwidth = .25, colour="black", fill="white") + 
  coord_cartesian(xlim=c(-5,5), ylim=c(0, .25)) +
  stat_function(fun = function(x) dnorm(x, summary$MEAN, summary$SD), color="red") + xlab(TeX("$\\mu_\\Delta$"))
```

<img width="805" alt="3" src="https://user-images.githubusercontent.com/65587875/101334587-2dee2e00-38cc-11eb-996a-fc2b574c2b4b.png">


 * The data has homogeneity of variance
 
   - A Levene's test was conducted to determine the assumption of equal variance in our datasets with the following statistical hypothesis: $H_{0lt} : \sigma_1^2 = \sigma_2^2$
   - The p value for Levene's test $p = 0.898 > 0.05$ therefore we **Fail to Reject $H_{0lt}$** meaning we can assume our data has homogeneity of variance.
   
### Decision Rules

 * **Reject $H_0$** if
   * p-value < 0.01 (the significance level $\alpha$) 
   * 99% CI of the mean difference does not capture $H_0 : \mu_\Delta = 0$
 * Otherwise **Fail to Reject $H_0$** 
 
 ### Paired T-Test 

```
alpha = 0.01

result = pairs %>% nest() %>%
  mutate(
    out = map(data, ~ t.test(.x$coles, .x$woolworths, paired = T, conf.level = 1-alpha) %>%
            tidy %>% rename(mean = estimate))
  ) %>%
  unnest(out, .drop = T) %>%
  mutate(alpha = alpha,
         conf.level = (1-alpha)*100,
         ptest = ifelse(p.value < alpha, paste0("p < ",alpha), paste0("p > ",alpha)),
         htest = ifelse(p.value < alpha, "Reject H0", "Fail to Reject H0")) %>% 
  arrange(p.value) %>% 
  modify_if(is.numeric, round, 3)

result %>% select(mean, p.value, ptest, htest, conf.level, conf.low, conf.high) 
```

<img width="824" alt="4" src="https://user-images.githubusercontent.com/65587875/101334599-334b7880-38cc-11eb-8202-4585fa5443c6.png">


## Interpretation

A paired-samples t-test was used to test for a statistically significant difference between the prices of identical products sold at Coles and Woolworths.

The mean difference between Coles and Woolworths prices was found to be `r result$mean`

All assumptions for using a t test were tested. A histogram was used to test normality and Levene's test was used to test homogeneity of variance.

The paired-samples t-test results are:

 * t(df=`r result$parameter`) = `r result$statistic`, `r result$ptest`,
`r result$conf.level`% CI [`r result$conf.low` `r result$conf.high`].
 * Given these findings we **`r result$htest`**

When our whole sample was analysed, the paired-samples t-test **did not** find evidence of a statistically significant difference between the prices of identical products sold at Coles and Woolworths.

## Further Interpretation (by category)

When the data was taken as a collective, we did not find a statistically significant difference.
We decided to perform this analysis again, this time grouping the data by Category.

```
 ggplot(g, aes(x = category, y = value)) + geom_boxplot(outlier.colour = "red") + coord_flip()
```

<img width="828" alt="5" src="https://user-images.githubusercontent.com/65587875/101334591-30e91e80-38cc-11eb-9d8a-b7a1cc7f8e80.png">


```{r, cols.print = 15}
result2 = pairs %>% group_by(category) %>% nest() %>%
  mutate(
    out = map(data, ~ t.test(.x$coles, .x$woolworths, paired = T, conf.level = 1-alpha) %>%
            tidy %>% rename(mean = estimate, df = parameter))
  ) %>%
  unnest(out, .drop = T) %>%
  mutate(alpha = alpha,
         conf.level = (1-alpha)*100,
         ptest = ifelse(p.value < alpha, paste0("p < ",alpha), paste0("p > ",alpha)),
         htest = ifelse(p.value < alpha, "Reject H0", "Fail to Reject H0")) %>% 
  arrange(ptest, desc(abs(mean))) %>% 
  modify_if(is.numeric, round, 3)

result2 %>% select(category, df, mean, p.value, ptest, htest, conf.level, conf.low, conf.high) 
```
 <img width="815" alt="6" src="https://user-images.githubusercontent.com/65587875/101334574-29297a00-38cc-11eb-8c00-a1c32d7b87f1.png">
 <img width="813" alt="7" src="https://user-images.githubusercontent.com/65587875/101334566-26c72000-38cc-11eb-84eb-15c095492341.png">

 
 ## Discussion 

**What did we conclude?**

Using a paired-samples t-test we determined:

 * when our sample was analysed as a whole, we **did not** find evidence for statistically significant price differences

When our sample was analysed by category:

 * in 8 categories we **did not** find a statistically significant difference between the prices

 * in 4 categories we **did** find a statistically significant difference between the prices
 
 ```{r}
result2 %>% 
  filter(p.value < alpha) %>% 
  mutate(conc = ifelse(mean < 0, "coles was on average cheaper than woolworths", "woolworths was on average cheaper than coles"),
         conclusions = paste0(conc, " by $", abs(mean %>% round(2)))) %>%
  arrange(desc(abs(mean))) %>% 
  select(category, conclusions) 
```

<img width="816" alt="8" src="https://user-images.githubusercontent.com/65587875/101334551-23339900-38cc-11eb-989c-7dc8f4503f60.png">


**What were the strengths and limitations of your investigation?\ **

A strength of our investigation was the sample size. As we were able to write a java script to scrape the entire data from the grocerycop website, we were able to analyse a very large sample size. This lead to a more accurate representation of the population and allowed us to assume the data was normally distributed when conducting the paired samples t-test.

We also performed an analysis by category using very efficient code.

**What improvements could be made or what would you do differently next time?**

In our analysis we set the value of $\alpha$ (the probablity of Type I error) to a low value.
Since $\alpha = Pr(Reject~H_0~|~H_0~is~true)$ is the probility that we detect an effect (Reject H0) when an effect does not exists (H0 is true).

There is another type of error we did not consider, $\beta$ (the probablity of Type II error).
Since $\beta = Pr(Fail~to~Reject~H_0 ~|~ H_0~is~false)$  is the probability that we detect no effect (Fail to Reject H0) when an effect does exist.

Statistical power is defined as $power = 1-\beta$. We want high power so that our hypothesis test is good at detecting a false null hypothosis. 

Below I have run a power test to determine what the power level for our t.test is:

```
ptt <- power.t.test(power = NULL, n = nrow(pairs), delta = summary$MEAN, sd = summary$SD, sig.level = alpha, type = "paired", alternative = "two.sided")
ptt2 <- power.t.test(power = 0.8, n = NULL, delta = summary$MEAN, sd = summary$SD, sig.level = alpha, type = "paired", alternative = "two.sided")
```

For our sample size of n=`r nrow(pairs)` (pairs) the power=`r ptt$power %>% round(3)` meaning that our test was **underpowered**. Therefore, the t.test would not have been able to reliably detect false null hypothesis, given the very small number of cents of difference we were trying to detect.

In order to achieve power=0.8, which is typically used in studies, our sample size would need to be increased to n=`r format(ceiling(ptt2$n),0)` (pairs) in order for the t.test to be able to reliably detect false null hypothesis.

