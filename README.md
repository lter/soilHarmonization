soilHarmonization
================

<!-- README.md is generated from README.Rmd. Please edit the latter. -->
### overview

`soilHarmonization` is an R package to aid the harmonization of data (and notes about data) for the LTER Soil Organic Matter (SOM) Working Group synthesis effort. This working group is examining SOM and other soil-related variables to evaluate competing theories that underlie models of soil dynamics.

Data provided by working group particants and other data sources are aggregated into a project Google Drive. Though all related to soils, these data vary vastly in their structure, units of measure, granularity and other details. To faciliate their use in models, the data must be homogenized to a sufficient degree such that cross-site, -project, -time comparisons are feasible.

To facilitate data harmonization, data providers are tasked with generating a *key file* that serves as a guide to translate the user-provided data into a common, project-wide structure and format. For each data set provided, the key file should contain general details about the data provider, the project from which the data were generated, and generalized details that apply to the data broadly (e.g., mean annual precipitation at the study site). Such generalized information is referred to as *location* or *locational* data in this project. At a finer resolution, the key file should contain mappings between the provided data and common terminology and units employed by the project for that data type. For example, the project-designated term for the standing stock of soil organic matter is `soc_stock` in units of g/m<sup>2</sup>. If the provided data included information about the standing stock of soil organic matter in a column titled `soil C` with units of %, that translation will be noted by the data provided on the Profile\_data tab of the key file. When run, the script will rename the column titled `soil C` to `som_stock` and apply the appropriate units conversion.

Each data set resides in its own subdirectory on the project Google Drive directory. Data providers must provide a key file for each data set.

### application

add content ~ script functionality

### installation

Install the current version from GitHub (after installing the `devtools` package from CRAN):

``` r
devtools::install_github("srearl/soilHarmonization")
```

### use

The script takes two input parameters: `directoryName` and `temporaryDirectory`. `directoryName` is quoted the name of the target Google Drive directory where the data and key file are located. Note that you must have read + write access to the target directory. `temporaryDirectory` is the quoted name and path of a directory on your local computer where the script will write output before uploading to the target Google Drive directory from which the data and key file were accessed. Script output includes a combined notes file and homogenized versions of the all provided data, each appended with *HMGZD* in the file names.

Special notes about the `temporarydirectory`:

-   the full-path should be provided (e.g., '/home/desktop/cdr\_output/')
-   the directory name should feature a trailing forward (Linux, Mac) or backward (Windows) slash (but the script should add this if not provided)
-   the script will create the directory if it does not exist
-   the same `temporaryDirectory` can be used for multiple iterations but the script will delete any content so be sure to move or back up files in the `temporaryDirectory` that you wish to save

**running the homogenization function, example:**

``` r
data_homogenization(directoryName = 'Luquillo elevation gradient', 
                    temporaryDirectory = '~/Desktop/luq_homogenized')
```
