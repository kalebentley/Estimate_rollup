# Estiamte Rollup
Background:
This analysis is designed to combine independently produced estimates of abundance and generate a rollup estimate of abundance and, if desired, pHOS and pAge
The analysis was specifically built to generate an annual rollup, population-level estimate for Lewis Basin tule fall Chinook by origin (hatchery, wild).  However, the analysis is agnostic to the specific populations/groupings.
Specifically, the analysis uses estimates of abundance (mean & standard deviation) by year and origin for three sub-populations - EF Lewis, NF Lewis, and Cedar Creek - as data.
The analysis combines the estimates using two approaches: 1.) log-normal moment matching, and 2.) Bayesian model that reconstructs the posterior distribution of each estimate
Although the approaches are different, both should generate nearly identical results. The main difference is that the second approach is able to generate estimates of uncertainty for pHOS/pNOS.

How to Use:
1.) Open the .csv file within the data sub-folder and update with estimates of abundance, as needed, following the existing data strucutre
    -- This analysis, and corresponding data file, are intended to be added to each year.  The output will separate estimates by Year (and Origin)
    -- Currently, the analysis is set up to create rollup estimates for one population/grouping (but could easily be updated to accommated data/estimate for >1 rollup grouping)
2.) Open the .Rmd file (which will launch the analysis Project in RStudio)
3.) Open the .R file (which loads the "master" analysis script)
4.) Run the analysis script line-by-line or select all and hit the "Run" button
5.) Files containing the combined estimates of abundance & proporition by origion will be generated and saved within the "output" subfolder 
    -- currently, the script creates a new subfolder based on the date the analysis was run to store the output files
    -- the combined estimates of abundance by Year and Param (e.g., Origin) will also show up in the R console 

Useful Information:
1.) To run the analysis script, all R packages must be loaded. The "xlsx" package has caused some issues in the past if you do not have JAVA installed on your computer
2.) To run the Bayesian model, you'll need JAGS installed and set up on your computer
3.) Make sure the data file is completely filled out for a given year before running a "final" rollup estimate
	--  If there are any missing/empty values in the "mean" and "sd" data fields for a given year, the moment matching approach (#1) will generate NAs for the roll-up estimate.  
   	-- However, the Bayesian approach will generate a rollup estimate for whatever data it given (it simply converts the empty/NA values to 0.001). Therefore, the estimate will not be accurate.
