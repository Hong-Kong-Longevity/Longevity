Data and R syntax for the following article:
Michael Y Ni, Vladimir Canudas-Romo, Jian Shi, Francis P Flores, Mathew SC Chow, Xiaoxin I Yao, Sai Yin Ho, Tai Hing Lam, C. Mary Schooling, Alan D Lopez, Majid Ezzati, Gabriel M Leung
Understanding Hong Kong’s path to becoming the world’s longest living population: a comparative study with long-living, high-income countries

List of files
Datasets (Folders)
I. WHO Mortality Database - Publicly available mortality data by age, sex, country and cause of death available at https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/download-the-raw-data-files-of-the-who-mortality-database
II. Human Mortality Database - Publicly available deaths, exposure and life tables data by age, sex and country available at https://www.mortality.org/hmd/
III. World bank - Publicly available life expectancy at birth data by country and sex available at https://data.worldbank.org/indicator/SP.DYN.LE00.IN

Codes
I. [HK death data.R] - Contains data management for Hong Kong deaths data from the Hong Kong Census and Statistics Department (not publicly available and cannot be made available to others)
II.[Joinpoint.R] - Contains syntax to plot the joinpoint regression results
III. [Arriaga's decomposition.R] - Contains syntax for the arriaga's decomposition domestic comparisons (within Hong Kong)
IV.[Aggregated HIC life tables.R] - Contains syntax to calculate an aggregated life tables for the 18 high-income countries
V. [data prep_TCALCI_broad.R] - Contains codes for preparation for the datasets used in the example of applying the function of confidence interval calculation for cause-specific TCAL difference in "TCAL functions.R".
VI. [TCAL functions.R] - Contains functions used in TCAL analyses.
VII. [HK vs HICs Arriaga's decomposition.R] - Contains syntax for the arriaga's decomposition international comparisons (Hong Kong vs High-income countries)
VIII. [Smoking AF.R] - Contains calculation of smoking attributable fractions by sex and age groups 
IX. [Smoking attributable mortality plots.R] - Contains syntax in plotting smoking-attributable mortality
X. [LifeTableFUN.R] - Contains syntax for the confidence interval calculation of life expectancy at birth
XI [Migrants analysis.R] - Contains syntax to calculate the life expectancy at birth by migrant status in Hong Kong and mortality rates by single age for Hong Kong and high-income countries
XII. [e0 HK vs Japan 1986-2017.R] - Contains syntax to calculate confidence interval of Hong Kong and Japan's life expectancy at birth]