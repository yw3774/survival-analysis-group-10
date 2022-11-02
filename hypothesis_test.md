Hypothesis Test
================
Ying Jin
2022-11-02

Prepare the data

``` r
pbc_data = 
  read_csv("./cirrhosis.csv") %>% 
  janitor::clean_names() %>%
  mutate(status = as.factor(status),
         drug = as.factor(drug),
         sex = as.factor(sex),
         ascites = as.factor(ascites),
         hepatomegaly = as.factor(hepatomegaly),
         spiders = as.factor(spiders),
         edema = as.factor(edema),
         stage = as.factor(stage)) %>% 
  mutate(status=recode(status,"D" =1, "C"=0,"CL"=0))
```

    ## Rows: 418 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): Status, Drug, Sex, Ascites, Hepatomegaly, Spiders, Edema
    ## dbl (13): ID, N_Days, Age, Bilirubin, Cholesterol, Albumin, Copper, Alk_Phos...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Log Rank Test

Using the log rank test, we want to test whether the occurrence of death
in the D-penicillamine group ($S_1(t)$) delayed compared to the placebo
group($S_0(t)$). The hypothese are as
below:$$H_0:S_1(t) \le S_0(t)\\H_1:S_1(t) > S_0(t)$$

Conducting log rank test:

``` r
# get rid of records which have no drug info

pbc_data_1 = 
  pbc_data %>% 
  drop_na(drug)

# K-M methods
pbc_kmsurvfit = 
  survfit(formula = Surv(n_days,status)~drug,data = pbc_data_1)

# plot K_m curve
ggsurvplot(pbc_kmsurvfit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
```

![](hypothesis_test_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# log rank test
survdiff(Surv(n_days,status)~drug,data = pbc_data_1)
```

    ## Call:
    ## survdiff(formula = Surv(n_days, status) ~ drug, data = pbc_data_1)
    ## 
    ##                        N Observed Expected (O-E)^2/E (O-E)^2/V
    ## drug=D-penicillamine 158       65     63.2    0.0502     0.102
    ## drug=Placebo         154       60     61.8    0.0513     0.102
    ## 
    ##  Chisq= 0.1  on 1 degrees of freedom, p= 0.7

Since the $P\ value$ is $0.7 > 0.05$ (significance level), we fail to
reject the null hypothesis and conclude that the survival probability in
D-penicillamine group is significantly lower than that in the placebo
group.

## Other Test

Using Gehan-Breslow generalized Wilcoxon, Tarone-Ware, Peto-Peto’s,
Fleming-Harrington test, the results are as follow:

Conducting log rank test:

``` r
comp(ten(pbc_kmsurvfit))
```

    ##                      Q         Var         Z pNorm
    ## 1          -1.7811e+00  3.1192e+01 -0.318913     2
    ## n           5.2000e+01  1.5222e+06  0.042146     6
    ## sqrtN      -1.2642e+01  6.2741e+03 -0.159603     3
    ## S1         -6.7362e-01  1.9218e+01 -0.153658     4
    ## S2         -6.6179e-01  1.9004e+01 -0.151807     5
    ## FH_p=1_q=1 -8.2787e-01  8.8617e-01 -0.879435     1
    ##               maxAbsZ        Var      Q pSupBr
    ## 1          6.1204e+00 3.1192e+01 1.0959      6
    ## n          1.6980e+03 1.5222e+06 1.3762      2
    ## sqrtN      1.0190e+02 6.2741e+03 1.2864      3
    ## S1         5.4875e+00 1.9218e+01 1.2517      5
    ## S2         5.4678e+00 1.9004e+01 1.2543      4
    ## FH_p=1_q=1 1.4582e+00 8.8617e-01 1.5490      1

We can see that using different weights, the test results are the same:
the survival probability in D-penicillamine group is significantly lower
than that in the placebo group.
