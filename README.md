# Examining women's choice between home and institutional births: insights from the Salud Mesoamérica Initiative (SMI)

### Data and scripts underlying the findings described in Examining women's choice between home and institutional births: insights from the Salud Mesoamérica Initiative (SMI)

This repository contains the complete analytical pipeline, processed data, and R scripts required to reproduce the findings on childbirth choices (home vs. institutional) across Guatemala, Chiapas, Honduras, and Nicaragua, as presented in the associated research paper.


## Overview
The project is structured as a reproducible workflow that evaluates childbirth experiences under the WHO Positive Childbirth Experience framework. Using secondary data from household surveys, the pipeline performs:

- Data harmonization: Cleaning and merging multi-country household survey data.

- Statistical analysis: Implementation of design-adjusted chi-squared tests (Rao-Scott correction) to account for complex survey designs.

- Longitudinal trends: Analysis of birth setting transitions across three follow-up points. 

- Multiple pregnancies: analysis of institutional quality of care and reasons for home birth for women that switch birth settings.

All analyses were implemented using the R Programming Language.


## Repository Structure
The repository is organized to ensure that every figure and table in the paper can be traced back to its specific source code:

[/Data:](https://github.com/LuciaBart/SMIchildbirth/tree/main/Data) Contains anonymized secondary data from household surveys (Guatemala, Chiapas, Nicaragua, and Honduras).

[/Scripts:](https://github.com/LuciaBart/SMIchildbirth/tree/main/Scripts)

- 01_Birthsettingsdistribution.R: Analysis of the place of delivery for the 4 countries in each follow-up (Figs. 2-5). 

- 02_Multiplepregnancies.R: Analysis of women with more than 1 child for the second follow-up (Figs. 7 and 8)


## Instructions for Reproduction

To replicate the study results, follow the steps below:

1. Clone or Download this repository to your local environment.

2. Ensure that R is installed.

3. Install the required library dependencies.

4. Run this command in R console:

`
source("RUN_PROCESS.r")
`

Note: The RUN_PROCESS.r script manages the sequential execution of all modular scripts. It reads files from `inputs/` and populates the `outputs/` folder. Please be aware that existing files in the `outputs/` directory will be overwritten upon execution.


## Citation

If you utilize these materials in your research, please cite the original publication.

