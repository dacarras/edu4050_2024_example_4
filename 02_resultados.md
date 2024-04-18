Code 03: resultados
================
dacarras
Abril 18, 2024

# Cargar datos

## Cargar datos desde archivo

``` r
#--------------------------------------------------------------------
# load data
#--------------------------------------------------------------------

#------------------------------------------------
# load main library
#------------------------------------------------

library(dplyr)

#------------------------------------------------
# load data from file
#------------------------------------------------

data_civ <- readRDS('data_civ.rds') # pooled
data_exp <- readRDS('data_exp.rds') # exploratory
data_con <- readRDS('data_con.rds') # confirmatory
```

# Descriptivos

## Items

``` r
#--------------------------------------------------------------------
# item
#--------------------------------------------------------------------

#------------------------------------------------
# item info
#------------------------------------------------

items_table <- read.table(
text="
var_name  variable item_text
IS3G23A   n01      'Voting in every national election'
IS3G23B   n02      'Joining a political party'
IS3G23C   n03      'Learning about the country history'
IS3G23D   n04      'Following political issues in the newspaper, on theradio, on TV or on the Internet'
IS3G23E   n05      'Showing respect for government representatives'
IS3G23F   n06      'Engaging in political discussions'
IS3G23G   n07      'Participating in peaceful protests against laws believed to be unjust'
IS3G23H   n08      'Participating in activities to benefit people in the <local community>'
IS3G23I   n09      'Taking part in activities promoting human rights'
IS3G23J   n10      'Taking part in activities to protect the environment'
IS3G23K   n11      'Working hard'
IS3G23L   n12      'Always obeying the law'
IS3G22A   td1      '[R] Political leaders give government jobs to their family members.'
IS3G22B   td2      '[R] One company or the government owns all newspapers in a country.'
IS3G22C   td3      'People are allowed to publicly criticize the government.'
IS3G22D   td4      'All adult citizens have the right to elect their political leaders.'
IS3G22E   td5      'People are able to protest if they think a law is unfair.'
IS3G22F   td6      '[R] The police have the right to hold people suspected of threatening national security in jail without trial.'
IS3G22G   td7      'Differences in income between poor and rich people are small.'
IS3G22H   td8      '[R] The government influences decisions by courts of justice.'
IS3G22I   td9      'All <ethnic/racial> groups in the country have the same rights.'
",
header=TRUE, stringsAsFactors = FALSE)

# display item table
knitr::kable(items_table)
```

| var_name | variable | item_text                                                                                                        |
|:---------|:---------|:-----------------------------------------------------------------------------------------------------------------|
| IS3G23A  | n01      | Voting in every national election                                                                                |
| IS3G23B  | n02      | Joining a political party                                                                                        |
| IS3G23C  | n03      | Learning about the country history                                                                               |
| IS3G23D  | n04      | Following political issues in the newspaper, on theradio, on TV or on the Internet                               |
| IS3G23E  | n05      | Showing respect for government representatives                                                                   |
| IS3G23F  | n06      | Engaging in political discussions                                                                                |
| IS3G23G  | n07      | Participating in peaceful protests against laws believed to be unjust                                            |
| IS3G23H  | n08      | Participating in activities to benefit people in the <local community>                                           |
| IS3G23I  | n09      | Taking part in activities promoting human rights                                                                 |
| IS3G23J  | n10      | Taking part in activities to protect the environment                                                             |
| IS3G23K  | n11      | Working hard                                                                                                     |
| IS3G23L  | n12      | Always obeying the law                                                                                           |
| IS3G22A  | td1      | \[R\] Political leaders give government jobs to their family members.                                            |
| IS3G22B  | td2      | \[R\] One company or the government owns all newspapers in a country.                                            |
| IS3G22C  | td3      | People are allowed to publicly criticize the government.                                                         |
| IS3G22D  | td4      | All adult citizens have the right to elect their political leaders.                                              |
| IS3G22E  | td5      | People are able to protest if they think a law is unfair.                                                        |
| IS3G22F  | td6      | \[R\] The police have the right to hold people suspected of threatening national security in jail without trial. |
| IS3G22G  | td7      | Differences in income between poor and rich people are small.                                                    |
| IS3G22H  | td8      | \[R\] The government influences decisions by courts of justice.                                                  |
| IS3G22I  | td9      | All <ethnic/racial> groups in the country have the same rights.                                                  |

``` r
#------------------------------------------------
# item selection
#------------------------------------------------

items_selected <- read.table(
text="
var_name  variable item_text
IS3G22D   td4      'All adult citizens have the right to elect their political leaders.'
IS3G22I   td9      'All <ethnic/racial> groups in the country have the same rights.'
IS3G22E   td5      'People are able to protest if they think a law is unfair.'
IS3G22H   td8      '[R] The government influences decisions by courts of justice.'
IS3G22B   td2      '[R] One company or the government owns all newspapers in a country.'
",
header=TRUE, stringsAsFactors = FALSE)

# display item table
knitr::kable(items_selected)
```

| var_name | variable | item_text                                                             |
|:---------|:---------|:----------------------------------------------------------------------|
| IS3G22D  | td4      | All adult citizens have the right to elect their political leaders.   |
| IS3G22I  | td9      | All <ethnic/racial> groups in the country have the same rights.       |
| IS3G22E  | td5      | People are able to protest if they think a law is unfair.             |
| IS3G22H  | td8      | \[R\] The government influences decisions by courts of justice.       |
| IS3G22B  | td2      | \[R\] One company or the government owns all newspapers in a country. |

## Proporciones de respuesta

``` r
#--------------------------------------------------------------------
# load data
#--------------------------------------------------------------------

#------------------------------------------------
# pooled sample
#------------------------------------------------

data_civ %>%
dplyr::select(td4, td9, td5, td8, td2) %>%
ilsa::wide_resp() %>%
knitr::kable(., digits = 2)
```

| variable |   00 |   01 |   NA | hist     |
|:---------|-----:|-----:|-----:|:---------|
| td4      | 0.18 | 0.80 | 0.03 | ▂▁▁▁▁▁▁▇ |
| td9      | 0.37 | 0.60 | 0.03 | ▅▁▁▁▁▁▁▇ |
| td5      | 0.34 | 0.64 | 0.03 | ▅▁▁▁▁▁▁▇ |
| td8      | 0.71 | 0.26 | 0.03 | ▇▁▁▁▁▁▁▃ |
| td2      | 0.52 | 0.45 | 0.03 | ▇▁▁▁▁▁▁▇ |

# Mixture Modelling

## Ajustar modelo de 3 clases

``` r
#------------------------------------------------------------------------------
# lca modelling
#------------------------------------------------------------------------------

# -----------------------------------------------
# class model
# -----------------------------------------------

class_model <- glca::item(td4, td9, td5, td8, td2) ~ 1

# -----------------------------------------------
# number of classes
# -----------------------------------------------

ac3 <- glca::glca(class_model, data = data_civ, nclass = 3, seed = 1, verbose = FALSE)

# -----------------------------------------------
# model fit
# -----------------------------------------------

fit_ac3 <- glca::gofglca(ac3, nboot = 100, test = 'boot', seed = 20221122)

# -----------------------------------------------
# display table
# -----------------------------------------------

fit_ac3$gtable %>%
tibble::as_tibble() %>%
knitr::kable(., digits = 2)
```

| logLik |  AIC | CAIC |  BIC | entropy | Res.Df | Gsq | Boot p-value |
|-------:|-----:|-----:|-----:|--------:|-------:|----:|-------------:|
|  -3883 | 7800 | 7906 | 7889 |    0.61 |     14 |  61 |            0 |

## Resultados

``` r
#------------------------------------------------------------------------------
# results
#------------------------------------------------------------------------------

# -----------------------------------------------
# display results
# -----------------------------------------------

library(glca)
summary(ac3)
```

    ## 
    ## Call:
    ## glca::glca(formula = class_model, data = data_civ, nclass = 3, 
    ##     seed = 1, verbose = FALSE)
    ## 
    ## Manifest items : td4 td9 td5 td8 td2 
    ## 
    ## Categories for manifest items :
    ##     Y = 1 Y = 2
    ## td4     0     1
    ## td9     0     1
    ## td5     0     1
    ## td8     0     1
    ## td2     0     1
    ## 
    ## Model : Latent class analysis 
    ## 
    ## Number of latent classes : 3 
    ## Number of observations : 1377 
    ## Number of parameters : 17 
    ## 
    ## log-likelihood : -3883 
    ##      G-squared : 61 
    ##            AIC : 7800 
    ##            BIC : 7889 
    ## 
    ## Marginal prevalences for latent classes :
    ## Class 1 Class 2 Class 3 
    ##    0.46    0.36    0.19 
    ## 
    ## Class prevalences by group :
    ##     Class 1 Class 2 Class 3
    ## ALL    0.46    0.36    0.19
    ## 
    ## Item-response probabilities (Y = 1) :
    ##           td4  td9  td5  td8  td2
    ## Class 1 0.078 0.33 0.26 1.00 0.68
    ## Class 2 0.047 0.18 0.17 0.38 0.24
    ## Class 3 0.700 0.90 0.90 0.76 0.76
    ## 
    ## Item-response probabilities (Y = 2) :
    ##          td4  td9  td5  td8  td2
    ## Class 1 0.92 0.67 0.74 0.00 0.32
    ## Class 2 0.95 0.82 0.83 0.62 0.76
    ## Class 3 0.30 0.10 0.10 0.24 0.24
