# Analysis code for scientific article 

Title: Trade union strength and workers’ health and wellbeing across low-, middle-, and high-income countries: a repeated cross-sectional study of 69 countries

Authors: Felix Forster, Jacques Wels

## Guide for reproducing the results

1. Download this project from GitHub. If you are unfamiliar with cloning GitHub repositories, you can download the files by clicking the green "<> Code" button on the top of this page and select "Download ZIP".
2. Download package `functionsUnionHealth`, which contains custom functions, from Github (see section "Further topics/Function package" below)
3. Download datasets (see section "Further topics/Data sources" below)
4. Run "run-project.R" in the main directory of this project (This takes some time and it might be more feasible to run it line by line. The tasks that take the most time are (1) fitting the models, as the dataset is large and we use country-specific cut-points, and (2) drawing samples repeatedly for creating the prediction plots (by function `functionsUnionHealth::evaluate_clm()`). All tasks that take a considerable amount of time are cached and run quickly from the second time onwards (see section "Further topics/Cache" below).)
5. All plots and tables can be found in folder "Output". In addition, the scripts create a table called "result-section-predictions.docx", which contains the numbers used in the description of Figure 3 in the Results section of the article.
6. With all objects in the cache, open "descriptives-text.qmd" in the main directory of this project. This is a Quarto file, which creates the first paragraph of the Results section. Click "Preview" to create a pdf file that contains the text with the numbers created from code. (If you have difficulties rendering a pdf, try changing the format in line 3 from pdf to html.)

## Project structure

Folders:
- Cache: An empty folder, which will hold cached objects (see section "Further topics/Cache" below)
- Graph: Only contains the Graphviz file for creating the flowchart
- Input: An empty folder, where you put the data sources (see section "Further topics/Data sources" below)
- Misc: Only contains a table (as .rds) that specifies required packages and the versions we used. It is used in "R/0_init.R" for comparing your package setup to what we used.
- Output: An empty folder, where tables and figures will be saved
- R: Contains most R scripts

Files in the main directory of the project:
- descriptives-text.qmd: A Quarto file for calculating the numbers used in the first paragraph of the Results section
- paper-union-wvs.Rproj: If you use RStudio, open this file to open the project
- README.md: The file you are currently reading
- run-project.R: The R script you run. It sources all other scripts in folder R.

Files in R:
A description of each script can be found at the top of the corresponding file. R files are split into three groups, indicated by their prefix:
- 0_: Initialisation scripts
- data_: Scripts preparing data
    - data_0: Runs all data processing scripts
    - data_1: Data processing
    - data_2: Descriptives
- post_: Short for post-data, i.e., scripts that use the processed data
    - post_1: Fit main models
    - post_2: Create model-based figures for main text
    - post_3: Sensitivity analyses

## Further topics

### Will the code save files on my computer?

Yes. The cache will save its files in folder "Cache", figures and tables will be saved in folder "Output", and the rendered pdf (or html) of Quarto document "descriptives-text.qmd" will be saved in the main directory. No files are saved outside the main directory.

### Data sources

The following instructions describe how to download the necessary datasets at time of publication. Please refer to the references in the article for more information if the following descriptions no longer work.

#### ICTWSS

How to get the data:
1. Go to https://www.oecd.org/en/data/datasets/oecdaias-ictwss-database.html
2. Scroll to section "Material/Data version 2.0 (September 2025)"
3. Click "In csv" to download the csv file. The file name is assumed to be "ICTWSS_v2.csv".
4. Move the file to folder "Input" in the main directory of this project

#### World Values Survey

How to get the data:
1. Go to https://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp; alternatively go to https://www.worldvaluessurvey.org and then to "Data and Documentation", "Data Download", and "Timeseries (1981-2022)"
2. Scroll to "Longitudinal Data Files"
3. Click "WVS TimeSeries 1981 2022 Rds v5 0.zip" to download the file
4. Fill in the download form and agree to the conditions of use
5. Unzip the file. The file name is assumed to be "WVS_Time_Series_1981-2022_rds_v5_0.rds".
6. Move the file to folder "Input" in the main directory of this project

#### ILOSTAT

ILOSTAT data is extracted via the `Rilostat` package directly in the R code, i.e., no data needs to be downloaded via the browser.

We know of the following changes to the dataset since conducting our analysis:

Trade Union Density data:
- Haiti has been removed
- Typos in the word "continuous" have been corrected
- Country name "Palestine, State of" has been changed to "Occupied Palestinian Territory"

Collective Bargaining Coverage data:
- Typos in the word "continuous" have been corrected
- For Estonia, " and Administrative data for small firms" has been removed from the source label

#### World Bank - country income classifications

We used version 2026_03_10 (see file suffix). We know of the following changes in the dataset since conducting our analysis:
- The country of Nauru has been renamed to Naoero

How to get the data:
1. Go to https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups 
2. Click "historical classification by income in XLSX format" to download the data
3. Open the downloaded .xlsx file and process it as follows:
    - Go to tab "Country Analytical History"
    - In the main menu in "Excel", click on "Data" and then on "Filter" to remove the filtering arrows in row 6
    - Remove rows 1-5, so that the first row is now what was previously row 6, i.e., the row with the years
    - Give new variable names to the first two columns by entering "country_short" in cell 1A, and "country_long" in cell 1B
    - Remove the rows between "Zimbabwe" and "Czechoslovakia (former)". We want to keep the data from former countries.
    - Remove everything below "Yugoslavia (former)"
    - Save tab "Country Analytical History" as "CSV (Comma delimited) (*.csv)". Use file name "OGHIST.csv".
 4. Move the file to folder "Input" in the main directory of this project

#### World Bank - GINI index

We used version v2_499 (see file suffix). We know of the following changes in the dataset since conducting our analysis:
- A new row "Fragility, Conflict, and Violence (FCV) situations" has been added
- Several values for China have been changed by 0.1:
    - 1987: 29.4 -> 29.3
    - 1993: 33.9 -> 33.8
    - 2008: 43.0 -> 42.9
    - 2011: 42.4 -> 42.3
    - 2012: 42.2 -> 42.1
    - 2017: 39.1 -> 39.0
    - 2021: 35.7 -> 35.8
 - The changes to Chinese data affect the values for 1986, 1987, 1988, 1992, 1993, 1994, 2007, 2008, 2011, 2012, 2017, and 2021. Chinese WVS data are available for 1990, 1995, 2001, 2007, 2018. As 2007 is part of both sets, this might slightly affect the results of fully adjusted models for both self-rated health and life satisfaction. Models that only include low- and lower-middle-income countries will also be affected. Models that are not fully adjusted should not be affected, because the GINI index is not included in these models.
- For Switzerland, a value has been added for 2023: 33.5. This change affects the values for the years 2023-2025. As Swiss WVS data is only available in 1989 and 1996, this change has no effect on the results.
- For India, a value has been added for 2023: 23.0. India is not included in the analysis. Therefore, this change has no effect on the results.
- For Uganda, a value has been added for 2023: 38.2. This change affects the values for the years 2021-2025. As WVS data for Uganda is only available in 2001, this change has no effect on the results.
- For Moldova, a value has been added for 2024: 26.8. This change affects the values for the years 2024-2025. As WVS data for Moldova is only available in 1996, 2002, and 2006, this change has no effect on the results. 
- The name for Nauru has been changed to Naoero

There are additional changes in the Metadata_Country file:
- A new row "Fragility, Conflict, and Violence (FCV) situations" has been added
- The notes for Egypt, Myanmar, and Zimbabwe changed, but only additional spaces have been introduced

How to get the data:
1. Go to https://data.worldbank.org/indicator/SI.POV.GINI
2. In the sidebar, there is a section "Download". Click on "CSV" to download the data.
3. Unzip the downloaded folder and rename it to "gini"
4. Move "gini" to folder "Input" in the main directory of this project
5. In the folder, there are three files:
    - API_SI.POV.GINI... (Will be used)
    - Metadata_Country_API_SI.POV.GINI... (Will be used)
    - Metadata_Indicator_API_SI.POV.GINI... (Will not be used)
6. Rename file "API_SI.POV.GINI..." to "API_SI.POV.GINI.csv" by removing the end of the file name
7. Rename file "Metadata_Country_API_SI.POV.GINI..." to "Metadata_Country_API_SI.POV.GINI.csv" by removing the end of the file name "API_SI.POV.GINI.csv" and remove 
8. Open file "API_SI.POV.GINI.csv" and remove the "Data Source" and "Last Updated Date" lines. The file should start with the variable names, i.e., "Country Name", etc.

#### V-Dem

V-Dem data is extracted using the `vdemdata` package directly in the R code, i.e., no data needs to be downloaded via the browser.

### Function package

This project uses custom R functions, which have been extracted and put in their own R package, called `functionsUnionHealth`. We chose to create a seperate project in the form of a package to be able to use established infrastructure for testing and documenting functions. The package is not available on CRAN, only on GitHub. It is another repository of user "forsterepi", where this repository (`paper-union-wvs`) lives as well. In order to be able to run the code in this project, you need to install package `functionsUnionHealth`. You can install it by running the following code:

```r
pak::pak("forsterepi/functionsUnionHealth")
```

The link to the project is: https://github.com/forsterepi/functionsUnionHealth

We use the following functions:
- coefs_clm(): Extracts coefficient table from our models
- evaluate_clm(): Processes and evaluates our models

### Cache

Some tasks take a long time to complete. "Caching" saves the output of said time-intensive tasks and loads them instead of re-running the code. Here, we use the `storr` package and save objects in .rds format.

The main directory contains the empty folder "Cache". The initialisation script "R/0_init.R" sets up the cache infrastructure in this folder, when run for the first time. When the cache infrastructure is set up, "R/0_init.R" "re-connects" you to the folder.

Throughout the scripts you will see the following code structure:

```r
if (cache$exists("name")) {
  object <- cache$get("name")
} else {
  object <- some_code()
  ## more code
  cache$set("name", object)
}
```

The IF condition checks if the object is already in the cache. If yes, the object is loaded from the cache. If not, R runs the code that actually creates the object. This means that the interesting code is in the ELSE part.

If you want to invalidate the cache, i.e., you want to delete the object from the cache, use the following code (and replace "name" with the actual name of the object). After deleting the object, you need to re-run the code.

```r
cache$del("name")
```

To be specific, the above code only deletes the pointer to the file. In order to delete the file itself, you need to additionally run the garbage collector.

```r
cache$gc()
```

You can also list all object names that are available in the cache.

```r
cache$list()
```
