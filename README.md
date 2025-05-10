# Code For HS paper1
This file inludes codes and data for 'Homogeneity and Spatial Spillovers Challenge Efficient Market Hypothesis in Local House Markets_Evidence from UK'


# Data Source:

House Price/Sales Volume: https://www.gov.uk/government/statistical-data-sets/uk-house-price-index-data-downloads-january-2024 (or search ‘UK House Price Index: data downloads January 2024’ in GOV.UK in Google)

Shapefile: https://www.arcgis.com/home/item.html?id=a4ffd65eb2c3424d897d2c22f373f49e 

LAD code to regional code: https://geoportal.statistics.gov.uk/datasets/ecc34decb1e5465b96bf055b4524edbf_0/explore (or search ‘Local Authority District to Region (April 2023) Lookup in EN’ in ONS in Google)
![image](https://github.com/user-attachments/assets/a841d10e-ad2f-4e58-860c-f4c39cff4b4c)


# Code Instructions:

•	First, open ‘Step1_data_filtering.R’ in ‘Code_For_Publication’ folder, 

o	Set Line 4 ‘CodeSource’ as the source of all codes.
o	Set Line 11 ‘FileSource’ as the source of all data.
o	Set Line 12 ‘OutputSource’ the same as ‘FileSource’.
o	Run.

•	Second, open ‘Step2_dbf_to_Wdist.R’

o	Set ‘CodeSource’ as the source of all codes.
o	Set ‘FileSource’ as the source of all data.
o	Set ‘OutputSource’ the same as ‘FileSource’.
o	Run.

•	Third, open ‘Step3_Table2_3_4_5_7.R’

o	Set ‘CodeSource’ as the source of all codes.
o	Set ‘FileSource’ as the source of all data.
o	Set ‘OutputSource’ as the target folder to store results.
o	Set ‘Roll = 60’, ‘MaxLags = 6’, ‘PreTime = 1:6’.
o	Run.
o	Table 2 will be available in ‘ModelResiduals_RMSE_Lags1_PreTime1_Roll60.xlsx’ and ‘ModelResiduals_ReMSPE_Lags1_PreTime1_Roll60.xlsx’
o	Table 3 will be available in sheet ‘NAR AR’, ‘NAR ARp’, and ‘ARp AR’ in ‘DM_test_New_Lags1_PreTime1_Roll60.xlsx’
o	Table 4 will be available in sheet ‘NAR NARX’, ‘ARp ARXp’, and ‘AR ARX’ in ‘DM_test_New_Lags1_PreTime1_Roll60.xlsx’
o	Table 5 will be available in ‘ReMSPE_UK Nation_MaxLags6_MaxPreTime6_Roll60.xlsx’
o	Table 7 will be available by setting different ‘Roll’, and repeat Run the code.


•	Then run rest of codes for corresponding figures and tables. Please remember to change ‘CodeSource’, ‘FileSource’, and ‘OutputSource’ in advance.
![image](https://github.com/user-attachments/assets/ca0c6e29-43c6-4023-abc9-2d4abf4a81f9)

