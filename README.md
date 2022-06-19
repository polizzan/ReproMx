# Implications of rising female reproductive-age mortality for fertility in the United States, 2010–2019 

## Project Abstract
Recent increases in female reproductive-age mortality in the United States (U.S.) imply that fewer women lived to bear children than would be expected had mortality stayed constant. We apply a cohort component projection model with alternative specifications to counterfactually estimate the impact of changes in female reproductive-age mortality on fertility in the period 2010–2019. We estimate the number of live births if (1) female mortality in the observation period had changed under three alternative scenarios, and, separately, if (2) age-specific fertility rates had remained constant after 2010. We find that fertility in the U.S. would have been higher had female all-cause mortality between ages 12 and 55 not increased after 2010, and if the U.S. had experienced European patterns of female reproductive-age mortality. For each cause of death, the number of children that would have additionally been born depends on the magnitude and timing of changes in female reproductive-age mortality and the average age at death. Thus, increases in mortality from accidental drug poisonings and suicide have had the strongest implications for U.S. fertility. However, we conclude that changes in women’s reproductive patterns have had much larger consequences for fertility than simultaneous changes in female reproductive-age mortality.

## Purpose of the Repository
The repository **ReproMx** was created to enable the (re-)production of findings reported in:
> Polizzi, Antonino, & Tilstra, Andrea M. (2022). *Implications of rising female reproductive-age mortality for fertility in the United States, 2010-2019*.

published on SocArXiv (hereafter *our manuscript*).

## Repository Structure
The repository **ReproMx** contains three folders:
- **data**

	used to store all data files necessary for analysis

- **plots**

	contains all plots produced by the analysis files (see **scripts** folder) and shown in our manuscript

- **scripts** 

	contains all analysis files necessary to (re-)produce the findings reported in our manuscript

## Data
The folder **data** is used to store the data files necessary to (re-)produce the findings reported in our manuscript. In this section, we describe the content of each data file in more detail.

We use data provided by the **Human Mortality Database** (HMD), the **Human Fertility Database** (HFD), and **CDC WONDER** for our analysis. As per the HMD and HFD User Agreements, we are unable to share the input data used for our analysis. However, data can be downloaded directly from HMD (https://www.mortality.org) and HFD (https://www.humanfertility.org) after setting up user accounts with the two databases. In addition, data provided by HMD and HFD can be downloaded via `R` using the functions `readHMDweb()` and `readHFDweb()` from the `HMDHFDplus` package. This procedure also requires setting up user accounts with HMD and HFD. Please note that the function `readHMDweb()` currently still points to the 'former' HMD website (https://former.mortality.org). Our analysis files (see the **Scripts** section) assume that data provided by HMD and HFD were manually downloaded and stored as .txt files in the folder **data** following our naming conventions. However, we also provide directions below on how to obtain the appropriate data via `R` using the functions `readHMDweb()` and `readHFDweb()`.

Data provided by CDC WONDER must be downloaded manually (https://wonder.cdc.gov/ucd-icd10.html) and stored as .txt files in the folder **data** following our naming conventions. The query parameters used to obtain the necessary data provided by CDC WONDER are listed below. To obtain data provided by CDC WONDER, a user account is *not* necessary.

Please note that data distributed by HMD, HFD, and CDC WONDER may have been updated after the publication of our manuscript. Replication results may therefore fully or partially differ from those published in our manuscript.

### HMD data

- **DEUTNP_Mx_1x1.txt**
		
	period death rates by single year of age and calendar year for Germany (total population)

- **ESP_Mx_1x1.txt**

	period death rates by single year of age and calendar year for Spain

- **FRATNP_Mx_1x1.txt**

	period death rates by single year of age and calendar year for France (total population)

- **GBRTENW_Mx_1x1.txt**

	period death rates by single year of age and calendar year for England and Wales (total population)

- **ITA_Mx_1x1.txt**

	period death rates by single year of age and calendar year for Italy

- **USA_Deaths_1x1.txt**

	period death counts by single year of age and calendar year for the U.S.

- **USA_Exposures_1x1.txt**

	period exposure counts by single year of age and calendar year for the U.S.

- **USA_Mx_1x1.txt**

	period death rates by single year of age and calendar year for the U.S.

- **USA_Population.txt**

	period population counts by single year of age and calendar year for the U.S.

### HFD data

- **USA_asfrRR.txt**

	period fertility rates by single year of age and calendar year for the U.S.

- **USA_asfrRRbo.txt**

	period fertility rates by single year of age, calendar year, and birth order for the U.S.

- **USA_birthsRR.txt**

	period birth counts by single year of age and calendar year for the U.S.

### CDC WONDER data

- **Underlying Cause of Death_113 causes_12-55_2010-2014.txt**

	period death counts for U.S. females by single year of age (12-55), calendar year (2010-2014), and underlying cause of death (CDC WONDER 113 Cause List)

	<table>
  		<tr><th colspan=2, align="center">Query parameters</th></tr>
  		<tr><td>Gender</td><td>Female</td></tr>
  		<tr><td>Single-Year Ages</td><td>12-55</td></tr>
  		<tr><td>Year</td><td>2010-2014</td></tr>
  		<tr><td>Group By</td><td>Year; Single-Year Ages; <br>ICD-10 113 Cause List</td></tr>
  		<tr><td>Show Totals</td><td>Disabled</td></tr>
  		<tr><td>Show Zero Values</td><td>True</td></tr>
  		<tr><td>Show Suppressed</td><td>True</td></tr>
	</table>

- **Underlying Cause of Death_113 causes_12-55_2015-2019.txt**

	period death counts for U.S. females by single year of age (12-55), calendar year (2015-2019), and underlying cause of death (CDC WONDER 113 Cause List)

	<table>
  		<tr><th colspan=2, align="center">Query parameters</th></tr>
  		<tr><td>Gender</td><td>Female</td></tr>
  		<tr><td>Single-Year Ages</td><td>12-55</td></tr>
  		<tr><td>Year</td><td>2015-2019</td></tr>
  		<tr><td>Group By</td><td>Year; Single-Year Ages; <br>ICD-10 113 Cause List</td></tr>
  		<tr><td>Show Totals</td><td>Disabled</td></tr>
  		<tr><td>Show Zero Values</td><td>True</td></tr>
  		<tr><td>Show Suppressed</td><td>True</td></tr>
	</table>

- **Underlying Cause of Death_Drug overdoses_12-55_2010-2019.txt**

	period death counts (ICD-10 causes X40-X44 combined) for U.S. females by single year of age (12-55) and calendar year (2010-2019)

	<table>
  		<tr><th colspan=2, align="center">Query parameters</th></tr>
  		<tr><td>Gender</td><td>Female</td></tr>
  		<tr><td>ICD-10 Codes</td><td>X40; X41; X42, X43; X44</td></tr>
  		<tr><td>Single-Year Ages</td><td>12-55</td></tr>
  		<tr><td>Year</td><td>2010-2019</td></tr>
  		<tr><td>Group By</td><td>Year; Single-Year Ages</td></tr>
  		<tr><td>Show Totals</td><td>False</td></tr>
  		<tr><td>Show Zero Values</td><td>True</td></tr>
  		<tr><td>Show Suppressed</td><td>True</td></tr>
	</table>


- **Underlying Cause of Death_Leading causes_12-55_2019.txt**

	2019 period death counts for U.S. females (ages 12-55 combined) by leading cause of death <br>

	<table>
  		<tr><th colspan=2, align="center">Query parameters</th></tr>
  		<tr><td>Gender</td><td>Female</td></tr>
  		<tr><td>Single-Year Ages</td><td>12-55</td></tr>
  		<tr><td>Year</td><td>2019</td></tr>
  		<tr><td>Group By</td><td>15 Leading Causes of Death</td></tr>
  		<tr><td>Show Totals</td><td>Disabled</td></tr>
  		<tr><td>Show Zero Values</td><td>Disabled</td></tr>
  		<tr><td>Show Suppressed</td><td>Disabled</td></tr>
	</table>

- **Underlying Cause of Death_Total_12-55_2010-2019.txt**

	period death counts for U.S. females by single year of age (12-55) and calendar year (2010-2019)

	<table>
  		<tr><th colspan=2, align="center">Query parameters</th></tr>
  		<tr><td>Gender</td><td>Female</td></tr>
  		<tr><td>Single-Year Ages</td><td>12-55</td></tr>
  		<tr><td>Year</td><td>2010-2019</td></tr>
  		<tr><td>Group By</td><td>Year; Single-Year Ages</td></tr>
  		<tr><td>Show Totals</td><td>False</td></tr>
  		<tr><td>Show Zero Values</td><td>True</td></tr>
  		<tr><td>Show Suppressed</td><td>True</td></tr>
	</table>

## Scripts
The folder **scripts** contains the analysis files necessary to (re-)produce all findings reported in our manuscript. In this section, we describe the purpose of each analysis file in more detail.

### 00-main.R
This file installs all `R` packages necessary to (re-)produce our findings. It automatically executes the files `01-preparation.R`, `02-descriptives.R`, `03-projections.R`, `04-sensitivity analyses.R`, `05-plots.R`, `06-appendix.R`, and `99-life table functions.R`, which are described in more detail below.

### 01-preparation.R
This file loads the necessary data provided by HMD, HFD, and CDC WONDER (see the **Data** section) and prepares them for further analysis. It assumes that all data were manually downloaded and stored as .txt files in the folder **data** following our naming conventions. However, we have also included the code to download data provided by HMD and HFD using the functions `readHMDweb()` and `readHFDweb()` from the `HMDHFDplus` package. The code to download data provided by HMD and HFD using the functions `readHMDweb()` and `readHFDweb()` is commented out in the file `01-preparation.R`. To download data provided by HMD and HFD using the functions `readHMDweb()` and `readHFDweb()`, simply delete the `#` in front of the code you wish to execute (and add `#` in front of the code you do *not* wish to execute). Also ensure that your HMD and HFD usernames and passwords are passed down to `R` at the beginning of the file. Data provided by CDC WONDER must be downloaded manually following the instructions given in the **Data** section.

### 02-descriptives.R
This file (re-)produces descriptive results reported in the sub-section **Descriptive Results** of our manuscript, such as trends in temporary life expectancy and the total fertility rate.

### 03-projections.R
This file (re-)produces projection results reported in the sub-sections **Scenario 1**, **Scenario 2**, **Scenario 3**, and **Scenario 4** of our manuscript.

### 04-sensitivity analyses.R
This file (re-)produces results from sensitivity analyses reported in the sub-section **Sensitivity Analyses** of our manuscript.

### 05-plots.R
This file creates Figures 1, 2, 3, and 4 shown in our manuscript. The respective .svg files are saved in the folder **plots**.

### 06-appendix.R
This file creates Appendices 1, 2, and 3 shown in our manuscript. The respective .svg files are saved in the folder **plots**.

### 98-reported values.R
This file is designed to facilitate the (re-)calculation of all values reported in our manuscript. It outputs all values reported in our manuscript into the `R` console, rounded to the appropriate decimal. This file is not automatically executed by the file `00-main.R` and must be run manually after executing the file `00-main.R`. 

### 99-life table functions.R
This file creates the functions used to calculate temporary life expectancy (see the sub-section **Descriptive Results** of our manuscript) and the female survivorship ratio (see the sub-section **Cohort Component Projection Model** of our manuscript). This file must therefore be executed before running the files `02-descriptives.R` and `03-projections.R`.

## Contact
Please contact antonino.polizzi@nuffield.ox.ac.uk for any questions and comments regarding the project <br>
**Implications of rising female reproductive-age mortality for fertility in the United States, 2010–2019** <br>
and/or the repository **ReproMx**.
