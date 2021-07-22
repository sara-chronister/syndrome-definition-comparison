# Syndrome Definition Comparison

## Background:

The purpose of this tool is to allow ESSENCE users to compare the data details (line level) results of either two or three syndrome definitions to gain a better understanding of how the syndromes are similar and different. This project produces several important outputs: 
* An html document that can be shared with others and opened using either Chrome or Firefox browsers. This document contains no identifiable data, only aggregate results, for security purposes. 
* A csv file for each combination of definitions possible containing a subset of variables important in the manual review process. These files do contain identifiable information and should be handled accordingly. 

## Instructions:

### Download and set up files:
* Download the project files by clicking the green "Code" button and selecting "Download ZIP".
* Unzip the files to the location that you want the project files to be stored.
* In the parent folder (same location as the .Rproj file) create two new folders: "Output (TwoDefs)" and "Output (ThreeDefs)". This will be where the program drops the generated CSV files that you can use for further analysis. Spelling and capitalization are important, so please ensure it matches the names listed here exactly.

### In Excel:
* Open the "Definition Comparison Table.xlsx" file
* Fill out all relevant information in all tabs. Detailed information about each of the fields is in the top row. DO NOT CHANGE THE LOCATIONS OF ANY OF THE FIELDS. The program will fail if you do.
* Save (without changing the name of the file) once you have all the information you need about your definitions.

### In RStudio:
* In your file location, open the "SyndromeDefinitionComparison.Rproj" file.
* In RStudio navigate to the Files tab (by default in the bottom right pane) and open the "DefinitionComparisonTemplate_ThreeDefs.Rmd" file.
  * You can rename this file to reflect the definitions you are comparing. This will be the file name of the report you generate.
* If you are a new user, or are not sure if you have all of the required packages, please follow the instructions in lines 11-23.
* If your organization uses proxy settings when connecting to the internet, or if you are not sure, follow the instructions in lines 36-42.
* All users must enter their password by following the instructions in lines 31-34. (Note that line 33 is only required if you need to use proxy settings.)
* To run the report, look for a button at the top of the script that says "Knit" with an icon that looks like a needle and ball of yarn next to it. Click that button and wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser and can be shared with anyone.

For questions please contact sara.chronister@maricopa.gov.
