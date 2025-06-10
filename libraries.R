
# Libraries for this project

## Reading and saving files
library(readr); library(writexl) # for read_csv(); write_xlsx()  
library(here)

# Cleaning
library(plyr) # mapvalues()
library(MASS)
library(dplyr)
library(tidyr) # for gather()
select <- dplyr::select

# Analysis
library(quantreg)
library(bayesQR)
library(Brq)

library(emg) # For building exponenetially modified distribution (for testing)

## Visualizing
library(ggplot2)

### extra ggplot features
library(ggpubr) # Adding p-values to ggplots
library(ggrepel)
library(scales); library(RColorBrewer) 
library(signs) # for signs_format(), uses proper minus sign instead of hyphen

library(flextable)
library(gtsummary)
library(gt)
library(labelled)
library(officer)


