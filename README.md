# Bachelor Thesis R Code

Welcome to the GitHub repository for the R script corresponding to my Bachelor thesis at the Faculty of Business, Economics and Social Sciences at the University of Bern.

**Thesis Title:**
"Obesity and Health Care Use: An Empirical Analysis of the Swiss Household Panel between 2004 and 2019."

## Project Overview

This repository contains the R script used in my Bachelor thesis, exploring the relationship between obesity and health care use based on data from the Swiss Household Panel and the Federal Statistical Office.

## Data Sources

- **Swiss Household Panel:** [Data Link](https://forscenter.ch/projects/swiss-household-panel/data/) (last checked on 04/03/2024).
- **Federal Statistical Office:** [Data Link](https://www.bfs.admin.ch/asset/de/25025263) (last checked on 04/03/2024).

**Note:** The actual data files are not included in this repository. Please download them from the provided links and place them in the `data/` directory before running the R script.

## Dependencies

The following R packages are used in this project:

```R
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(estimatr)
library(car)
library(xtable)
library(fastDummies)
library(texreg)
library(ggdag)
library(dagitty)
library(ggtext)
library(pxR)
```
## Usage

To run the R script:

1. Install the required R packages.
2. Download the data from the provided links.
3. Place the downloaded data files in the `data/` directory.
4. Execute the R script in your preferred R environment.

## Folder Structure

/bachelorthesis
|-- /data
|   |-- /Data_STATA
|       |-- [List of STATA data files]
|   |-- /px-x-1405000000_101.px
|-- /script
|   |-- scriptfile_bachelor_thesis.R
|-- README.md
|-- LICENSE.md

## License

This project is licensed under the MIT License. See the LICENSE.md file for details.
