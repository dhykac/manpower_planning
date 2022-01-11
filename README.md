# Manpower Planning using Product Movement Forecast

This repository is about forecasting manpower planning by using product movement forecasting each day.

## Sources
- [Dataset for Python](https://github.com/dhykac/manpower_planning/blob/main/PRODUCTIVITY%20DESEMBER%202021.XLSX)
```python
# Packages for Python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.holtwinters import ExponentialSmoothing
```
- [Dataset for R](https://github.com/dhykac/manpower_planning/blob/main/manpower_planning.R)
```r
# Library for R
library(scales)
library(rlang)
library(lazyeval)
library(ggplot2)
library(fpp2)
library(readxl)
```

## Objectives
This repository tried to answer following question :
- How much the workload for next day based on daily workloads for each queue on the past one month?
- How many manpower for each queue will be?

Challenges :
- Daily productivity target for forklift operators are 175 pallets and reachtrucks are 90 pallets each day
- NARROW, NARROW-OUT, and NARROW-RPL are considered as one type workloads because each Reachtruck could handle all of it.
- Aside from Reachtruck queue, the other workload are handled by forklift operators with different person for each queue. So all of the queue considered as different type workloads.

## Update timelines
- Update 04.01.2022 : Added trend parameter to Exponential Smoothing.
- Update 11.01.2022 : Added new R script & R markdown version with three different methods (Naive Bayes, ARIMA, Exponential Smoothing).

# Result Overview
to be added soon!
