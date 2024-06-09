[![doi](https://img.shields.io/badge/DOI-10.17605/OSF.IO/Z5DJB-blue)][doi]

[doi]: https://doi.org/10.17605/OSF.IO/Z5DJB

# The impact of early death on birth counts in the United States, 1950&ndash;2019 

## Purpose of this Repository
The repository **ReproMx** was created to enable the replication of findings reported in:

Polizzi, A., & Tilstra, A. M. (2024). The impact of early death on birth counts in the United States, 1950 to 2019. *PNAS Nexus*, 3(6). https://doi.org/10.1093/pnasnexus/pgae058,

hereafter *our manuscript*.

## Repository Structure
The repository **ReproMx** contains two main folders, *Journal* and *SocArXiv*.

### *1. Journal*
The main folder **Journal** contains all data and `R` scripts necessary to replicate the information reported in the journal version of our manuscript. This includes the information reported in the Supplementary Information. The data and analysis files are respectively stored in the sub-folders *data* and *scripts*.

#### a. data
This sub-folder stores all data files necessary to replicate our findings. 

We use data from the [United Nations World Population Prospects](https://population.un.org/wpp/) (UNWPP), version 2022, provided by the United Nations Department of Economic and Social Affairs (UNDESA). The .zip files provided in the **data** sub-folder do **not** need to be unzipped before running the analysis files in the **scripts** sub-folder.

UNWPP 2022 data are distributed under a [Creative Commons license CC BY 3.0 IGO](https://creativecommons.org/licenses/by/3.0/igo/). Please note that our reported findings are based on UNWPP version 2022 and that data distributed by UNDESA may have been updated or revised in the meantime.

#### b. scripts
This sub-folder contains the analysis files necessary to replicate all findings reported in our main manuscript and the Supplementary Information using the statistical software `R`: 

- The file `98-data.R` loads the UNWPP input data from the **data** sub-folder.

- The file `99-functions.R` (a) builds a function `life.table()` that calculates life table functions, taking as input a vector of age-specific mortality rates; (b) builds a function `CCPM()` that stochastically or deterministically projects a starting population forward, taking as input information on the male and female population age structures, male and female survivorship ratios, male and female net migration counts, female fertility rates, as well as sex ratios at birth.   

- The file `01-analysis.R` installs all `R` packages necessary to replicate our findings. It automatically executes the files `98-data.R` and `99-functions.R` described above. Furthermore, the file `01-analysis.R` (a) carries out counterfactual population projections, and (b) calculates the three formal demographic indicators reviewed in our manuscript (probability of survival to age 50, reproductive-age life expectancy, reproduction-survival ratio) for the specified countries and time period. Plots of &apos;missing births&apos; and the three formal demographic indicators are stored in *.pdf* and *.svg* format in an automatically generated sub-folder **out**. The file `01-analysis.R` also stores the formal demographic indicators and the output from the counterfactual population projections in a combined `.RData` file in the automatically generated **out** sub-folder for later access.  

### 2. *SocArXiv*
The main folder **SocArXiv** contains all materials associated with version 1 and version 2 of our manuscript preprint, as posted on [*SocArXiv*](https://doi.org/10.31235/osf.io/fdj6y). This main folder is just for reference, as the analytical strategy and code have changed following the peer review of our manuscript.

## How to Use this Repository
In order to run the `R` code provided in the repository **ReproMx**, please proceed in the following order:

1. Download the repository from `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Double click on the file `ReproMx.Rproj` in the **Journal** folder. This should open `RStudio` on your machine.  
3. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `01-analysis.R` located in the **scripts** sub-folder.
4. You should now be able to run our code without adjusting any directories.

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png