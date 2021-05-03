Data and R syntax for the following article:<br/>
Michael Y Ni, Vladimir Canudas-Romo, Jian Shi, Francis P Flores, Mathew SC Chow, Xiaoxin I Yao, Sai Yin Ho, Tai Hing Lam, C. Mary Schooling, Alan D Lopez, Majid Ezzati, Gabriel M Leung<br/>
Understanding Hong Kong’s path to becoming the world’s longest living population: a comparative study with long-living, high-income countries<br/>
<br/>
Data sources<br/>
I. WHO Mortality Database - *Note: Files cannot be uploaded due to size limit* Publicly available mortality data by age, sex, country and cause of death available at https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/download-the-raw-data-files-of-the-who-mortality-database<br/>
II. Human Mortality Database - Publicly available deaths, exposure and life tables data by age, sex and country available at https://www.mortality.org/hmd/<br/>
III. World bank - Publicly available life expectancy at birth data by country and sex available at https://data.worldbank.org/indicator/SP.DYN.LE00.IN<br/>
<br/>
Codes<br/>
I. [HK death data.R] - Data management for Hong Kong deaths data from the Hong Kong Census and Statistics Department (non-public data; data access requires an application to the Hong Kong Census and Statistics Department)<br/>
II.[Joinpoint.R] - Syntax to plot the joinpoint regression results<br/>
III. [Arriaga's decomposition.R] - Contains syntax for the Arriaga's decomposition domestic comparisons (within Hong Kong)<br/>
IV.[Aggregated high-income countries life tables.R] - Contains syntax to calculate an aggregated life tables for the 18 high-income countries<br/>
V. [data prep_TCALCI_broad.R] - Contains codes for preparation for the datasets used in the example of applying the function of confidence interval calculation for cause-specific Truncated cross-sectional average length of life (TCAL) difference in "TCAL functions.R".<br/>
VI. [TCAL functions.R] - Contains functions used in TCAL analyses.<br/>
VII. [HK vs HICs Arriaga's decomposition.R] - Contains syntax for the Arriaga's decomposition international comparisons (Hong Kong vs High-income countries)<br/>
VIII. [Smoking AF.R] - Contains calculation of smoking attributable fractions by sex and age groups <br/>
IX. [Smoking attributable mortality plots.R] - Contains syntax in plotting smoking-attributable mortality<br/>
X. [LifeTableFUN.R] - Contains syntax for the confidence interval calculation of life expectancy at birth<br/>
XI [Migrants analysis.R] - Contains syntax to calculate the life expectancy at birth by migrant status in Hong Kong and mortality rates by single age for Hong Kong and high-income countries<br/>
XII. [e0 HK vs Japan 1986-2017.R] - Contains syntax to calculate confidence interval of Hong Kong and Japan's life expectancy at birth]<br/>
