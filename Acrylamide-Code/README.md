# Code for: Acrylamide exposure and risk analysis: A global perspective on human biomonitoring data

This repository contains the R code used to generate the EDI values and sensitivity analyses for the manuscript **"Acrylamide exposure and risk analysis: A global perspective on human biomonitoring data"**.

## File List

The repository includes the following three R script files:

1.  **`blood.R`**
    *   **Purpose**: Used to calculate the Estimated Daily Intake (EDI) of Acrylamide (AA) across different countries and populations, based on hemoglobin adducts as exposure biomarkers.

2.  **`urine1.R`**
    *   **Purpose**: Used to calculate the Estimated Daily Intake (EDI) of Acrylamide (AA) for different countries, based on urinary metabolites as exposure biomarkers.

3.  **`urine2.R`**
    *   **Purpose**: Calculates the Estimated Daily Intake (EDI) of Acrylamide (AA) across different countries, using urinary metabolites as exposure biomarkers. It also generates the sensitivity analysis plots based on the urinary excretion factor (FUE).

## Instructions for Use

1.  **Requirements**:
    *   R Version: >= 4.0.0
    *   **Required R Packages**: Please install the following packages before running the scripts: `readxl`, `dplyr`, `ggplot2`, `tidyr`, `openxlsx`.

2.  **Execution Order**:
    To reproduce the analysis pipeline, it is suggested to run the scripts in the following order:
    `blood.R` -> `urine1.R` -> `urine2.R`

3.  **Data**:
    *   This repository does **NOT** contain the original analyzed datasets.
    *   Access to the anonymized data used to generate the results can be requested from the authors as described in the **"Data Availability Statement"** section of the manuscript.


---
*Last updated: January 18, 2026*

