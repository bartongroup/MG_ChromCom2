# Chromatin compaction 2

Software to accompany the manuscript "Exclusion of condensin I from the nucleus during prophase coordinates mitotic chromosome reorganization to complete sister chromatid resolution", John K. Eykelenboom, Marek Gierliński, Zuojun Yue, Tomoyuki U. Tanaka bioRxiv 2024.04.26.591320; doi: https://doi.org/10.1101/2024.04.26.591320.

## Usage

We suggest using RStudio. Start in the top project directory.

### Install required R packages

The first step is to install required R packages using `renv`:

```
install.packages("renv")
renv::restore()
```

### Shiny app

The Shiny app for state parsing can be run

```
shiny::runApp("shiny/parsing_states")
```

The first thing needed in the app is to load Excel file data by clicking "Load Excel files". Once this is finished, you can set parsing parameters and press "Submit" to see the results. 

### Project report

The report including many additional analyses can be created by running the `targets` pipeline.

```
targets::tar_make()
```

This will analyse data and create a report (in directory `doc`). 
