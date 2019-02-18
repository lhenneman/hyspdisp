---
title: "hyspdisp"
author: "Lucas Henneman"
date: "2/18/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# ```hyspdisp```

```hyspdisp``` is an R package that runs[HYSPLIT](https://ready.arl.noaa.gov/HYSPLIT.php) many times. The results can then be aggregated to ZIP code level to create national estimates of exposure from various sources.  For example, plumes from several power plants can be tracked for many days and cumulative impacts estimated. The package relies on a modified version of the ```SplitR``` [package](https://github.com/lhenneman/SplitR).

### Install ```hyspdisp``` from github and load 
First, download and install ```hyspdisp```
```{r}
devtools::install_github("lhenneman/hyspdisp@dev2")
library(hyspdisp)
```

### Browse the available vignettes

```{r}
browseVignettes(package = "hyspdisp")
```




















