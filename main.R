# Copyright (c) 2024 Raffaele Mattera and Margaux Brégère
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sub license, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

rm(list = ls())
library(rio)
library(tidyverse)
library(readr)
library(zoo)
library(mgcv)
library(readxl)
library(readr)
library(MASS)
library(sf)
library(spdep)
library(giscoR) 
library(ClustGeo) 
library(FoReco) 
my_colors = c('#003f5c','#dd5182','#ffa600','#ff6e54','#955196','#444e86')

source('scripts/preprocessing.R')

data_train <- data %>% filter(lubridate::year(date) < 2021) %>% filter(lubridate::year(date) > 2017)
paste0("TRAIN DATA SET: from ", min(data_train$date), ' to ', max(data_train$date))

data_calib <- data %>% filter(lubridate::year(date) == 2021) 
paste0("CALIBRATION DATA SET: from ", min(data_calib$date), ' to ', max(data_calib$date))

data_test  <- data %>% filter(lubridate::year(date) > 2021) 
paste0("TEST DATA SET: from ", min(data_test$date), ' to ', max(data_test$date))

rm(data)
source('scripts/clustering.R')
source('scripts/forecasting.R')
source('scripts/reconciliation.R')
source('scripts/testing.R')