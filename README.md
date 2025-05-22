# BUSI 444: Advanced Computer Assisted Mass Appraisal - Case Study Examination

## Course Information
* **Institution:** The University of British Columbia [cite: 1]
* **Program:** Certificate Program in Real Property Assessment / Diploma Program in Urban Land Economics [cite: 1]
* **Course:** BUSI 444 - Advanced Computer Assisted Mass Appraisal [cite: 24]
* **Examination Date:** January 5, 2021 (as per document) [cite: 1]

## Project Overview
This repository contains the work completed for the BUSI 444 Case Study Examination. The primary goal of this project was to develop valuation models based on a provided database of property sales for Marilu City. [cite: 1] The project also required the creation of an appraisal report detailing the results and the methodologies used to obtain them. [cite: 2] This report was intended for the Assessor of Marilu City to aid in valuing properties for the 2018 Assessment Roll. [cite: 3, 4]

The valuation date for this project is July 1, 2017, with the model intended to inform the 2018 Assessment Roll. [cite: 4] The City of Marilu operates on a full market value assessment base, and the models developed aim to reflect the full actual (market) value of the fee simple interest in the properties. [cite: 5, 6]

## Key Objectives & Requirements
* Develop two distinct valuation models:
    * A market-adjusted cost approach model. [cite: 63, 64]
    * A direct sales comparison-based approach model (additive multiple linear regression). [cite: 63, 70]
* Describe the general model forms using standard mathematical symbols. [cite: 64, 70]
* Review and select appropriate variables from the database, including transformations. [cite: 65, 71]
* Develop a market-based land value using the land residual technique for the cost model. [cite: 67]
* Calibrate both models. [cite: 68, 73]
* Completely test and evaluate both models from appraisal and assessment perspectives. [cite: 74, 75]
* Provide conclusions on model quality and recommend the most effective model to the Assessor. [cite: 76]
* Recommend any necessary actions for the Assessor before using the model. [cite: 77]
* Meet IAAO's minimum requirements (CODs < 15% and median ASRs between 90% and 110%). [cite: 80]
* Include syntax files for all data transformations in the appendix. [cite: 14, 15]

## Marilu City Context
* **Location:** Mid-sized city in Northern Ontario. [cite: 16]
* **Population (2016 Census):** 70,563. [cite: 16]
* **Climate:** Long cold winters, short summers. [cite: 17]
* **Major Industry:** Forestry. [cite: 18]
* **Real Estate:** Approximately 24,000 dwelling units, 70% owned, 30% rented. Single detached homes make up 65% of housing stock. [cite: 25, 26] Sales data is from the densely developed urban core. [cite: 29]

## Database
* The project utilized the "Marilu database," a fresh copy of the dataset used in Project 1 of BUSI 444. [cite: 111]
* For the direct sales comparison approach, the database was split into:
    * **MODEL database:** At least 300 sales. [cite: 113]
    * **TEST database:** At least 60 sales and no more than 150 sales. [cite: 113]
    * A `RANDOM` variable was used for this split. [cite: 114, 115]
* Cost data was referenced from the BUSI 444 Course Workbook. [cite: 120]

## Methodology
The project involved:
1.  **Data Preparation:** Reviewing variables, performing necessary transformations. [cite: 65, 71]
2.  **Cost Model Development:** [cite: 63]
    * Defining the general model. [cite: 64]
    * Market-based land valuation (land residual technique). [cite: 67]
    * Model calibration using provided cost data and derived depreciation. [cite: 68]
    * Developing market adjustments.
3.  **Direct Sales Comparison Model Development:** [cite: 63]
    * Defining the general additive multiple linear regression model. [cite: 70]
    * Variable selection using various analytic techniques. [cite: 71]
    * Model calibration using multiple linear regression. [cite: 73]
4.  **Model Testing & Evaluation:** Both models were thoroughly tested for appraisal level, dispersion (compared to IAAO standards), horizontal equity, and vertical equity. [cite: 74, 160]
5.  **Reporting:** An appraisal report was prepared following the "Mass Appraisal Report Outline" provided in the examination details. [cite: 7, 8, 126]

## Repository Structure
*(You can customize this section based on how you've organized your files)*
* `/report`: Contains the final appraisal report (e.g., `BUSI444_CaseStudy_Report.pdf`).
* `/syntax`: Contains SPSS syntax files for data transformations and model calibration. [cite: 14, 160]
* `/data`: (If you are allowed to share the raw data or subsets, otherwise omit or note that data is from the course).
* `/output`: Key SPSS outputs, charts, and tables used in the report (often referenced from the appendix). [cite: 12, 160]
* `README.md`: This file.

## Software
R*
