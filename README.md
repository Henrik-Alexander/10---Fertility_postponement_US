April 2023

# Catching Up: The role of postponement potential for the recent fertility decline in the United States
This folder contains the code to fully reproduce the result of the research note **Catching Up: The role of postponement potential for the recent fertility decline in the United States**. All code on which this analysis is based was written in the [**R**](https://www.r-project.org/) statistical programming language.

### Packages
This work would not have been possible with the scientific and programming contributions of people who developed packages and made them available free of use on [**R-Cran**](https://cran.r-project.org/). I list the packages used in this project to acknowledge the contribution of the authors and to ensure that people can download the required packages in order to fully reproduce the results. Furthermore, the interested reader can follow the link on the package name to read the vignettes.

- [`stargazer`](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) by Marek Hlavac
- [`feisr`](https://cran.r-project.org/web/packages/feisr/index.html) by Tobias Rüttenauer
- [`tidyverse`](https://cran.r-project.org/web/packages/tidyverse/index.html) by Hadley Wickham
- [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) by Matt Dowle et al.
- [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html) by Achim Zeileis et al.
- [`reshape2`](https://cran.r-project.org/web/packages/reshape2/index.html) by Hadley Wickham
- [`usdata`](https://cran.rstudio.com/web/packages/usdata/index.html>) by Mine  Çetinkaya-Rundel et al.
- [`plm`](https://cran.r-project.org/web/packages/plm/plm.pdf) by Yves Croissant et al.
- [`clusterSEs`](https://cran.r-project.org/web/packages/clusterSEs/index.html) by Justin Esarey
- [`lmtest`](https://cran.r-project.org/web/packages/lmtest/index.html) by Torsten Hothorn et al.
- [`starpolisher`](https://github.com/ChandlerLutz/starpolishr) by Chandler Lutz
- [`aTSA`](https://cran.r-project.org/web/packages/aTSA/aTSA.pdf) by Debin Qiu
- [`readxl`](https://cran.r-project.org/web/packages/readxl/index.html) by Jennifer Bryan
- [`quantreg`](https://cran.r-project.org/web/packages/quantreg/index.html) by Roger Koenker
- [`SparseM`](https://cran.r-project.org/web/packages/SparseM/index.html) by Roger Koenker et al.
- [`rqpd`](https://r-forge.r-project.org/projects/rqpd/) by Roger Koenker and Stefan Holst Bache
- [`patchwork`](https://cran.r-project.org/web/packages/patchwork/index.html) by Thomas Lin Pedersen
- [`ggrepel`](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html) by Kamil Slowikowski
- [`bea.R`](https://cran.r-project.org/web/packages/bea.R/bea.R.pdf) by Andrea Batch


## Reproduction Procedures

1. Register at the following websites: 
    * [US Mortality DataBase](https://usa.mortality.org/mp/auth.pl)
    * [Global Data Lab](https://globaldatalab.org/register/) 
    * [BEA](https://apps.bea.gov/API/signup/)

2. Download the data
    
    2.1. **Automatically:** Include the account information from [US Mortality DataBase](https://usa.mortality.org/mp/auth.pl), [Global Data Lab](https://globaldatalab.org/register/) and the API-key for [BEA](https://apps.bea.gov/API/signup/) in the `meta-file.R` script, set `down_type` = "automatic" and run the meta-file
    
    2.2. **Manually:** Visit the websites provided in the section raw_data below, download the data from the websites and store under the names provided in the description
3. Run the scripts `01.` to `10.`


# Directory Structure
The structure of the repository is as follows:

```
.
├-- .gitignore
├-- Code
│   ├── 01_edit_births.R	<- Cleans the birth data
│   ├── 02_edit_cps.R    <- Cleans the cps data
│   ├── 03_rates.R    <- Estimates rates
│   ├── 08_descriptives.R   <- Makes descriptive statistics
│   ├── old_imputation.R    <- Outdated approach to impute missings
├-- Raw
│   ├── cps_000003.csv
│   ├── cps_19902010.csv
├-- Figures
├-- Functions
│   ├── Packages.R		       <- Installs and loads the packages
│   ├── Graphics.R           <- Sets the graphic style
│   ├── Functions.R          <- Installs the functions
├-- Results
├-- Readme.md
└── Meta.R		   <- Runs the entire project

```

### Raw_data:

The raw_data subdirectory consists already of most required files to reproduce the results,,
except for 1) the birth files from the national bureau of economic research and 2) the population
data from . 

1) **Birth data** can be obtained from:
[from https://www.nber.org/data/vital-statistics-natality-data.html}

Please download the data for the years 1969 to 2018,
 and save the files in the folder "Raw_data/Births" in the following way:
   "natl1969.csv" - "natl2018.csv" 

