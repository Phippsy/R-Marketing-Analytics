# Marketing Analytics methods for R
The code in this repository explores common marketing analytics applications for using R, the statistical language. Each file is a summary of notes taken from ["R for Marketing Research and Analytics"](http://www.springer.com/us/book/9783319144351), (Chapman, Christopher N. and McDonnell Feit, Elea).

The contents of each file are as follows:

- **Data Vis Methods.R:** A look at creating histograms and boxplots from the Base R package. 
- **Data Exploration Methods.R:** Generating random data using rep(), rbinom(), sample() and rpois() (lines 1-56). Summarising data tables using table(), simple plots and finally creating a function "firstAnalysis()" to summarise data attributes.
- **Reducing Data Complexity.R:** A brief look at simplifying ordinal brand ratings. Data is rescaled, aggregated and then visualised with heatmaps.
- **comparingGroups.R:** Random data generation from (lines 1-105). After this, an example of comparing consumer segments data using aggregate(), table(), then visualisation to compare segment attributes with barcharts and boxplots.
- **continuousVariableMethods.R:** An example to explore visualisation to compare continuous variables using generated retailer data (lines 1-44). Visualisations include basic plots, hist(), gpairs. Finally, we look at common data transformations prior to modelling (lines 194-210).
- **groupsStatisticalTests.R:** Explores a number of functions in base-R to compare group characteristics. Includes chisq.test(), binom.test(), binom.confint() and t.test(). Finally, an explaration of ANOVA within R.
- **linearDriverModels.R:** A look at creating models with lm(), comparing models and selecting the most efficient model. Contains a lot of commented text to accommodate output included from the R console.
- **linearModelSummary.R:** A summary of the linearDriverModels.R file.
- **standardPackages.R:** A list of packages frequently used during the analysis.
- **statsChecker.R:** A single file containing the firstAnalysis() function which was created in "Data Exploration Methods.R", above.
