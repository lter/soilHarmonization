soilHarmonization
================

<!-- README.md is generated from README.Rmd. Please edit the latter. -->

### overview

`soilHarmonization` is an R package to aid the harmonization of data
(and notes about data) for the LTER Soil Organic Matter (SOM) Working
Group synthesis effort. This working group is examining SOM and other
soil-related variables to evaluate competing theories that underlie
models of soil dynamics.

Data provided by working group particants and other data sources are
aggregated into a project Google Drive. Though all related to soils,
these data vary vastly in their structure, units of measure, granularity
and other details. To faciliate their use in models, the data must be
homogenized to a sufficient degree such that cross-site, -project, -time
comparisons are feasible.

To facilitate data harmonization, data providers are tasked with
generating a *key file* that serves as a guide to translate the
user-provided data into a common, project-wide structure and format. For
each data set provided, the key file should contain general details
about the data provider, the project from which the data were generated,
and generalized details that apply to the data broadly (e.g., mean
annual precipitation at the study site). Such generalized information is
referred to as *location* or *locational* data in this project. At a
finer resolution, the key file should contain mappings between the
provided data and common terminology and units employed by the project
for that data type. For example, the project-designated term for the
standing stock of soil organic matter is `soc_stock` in units of
g/m<sup>2</sup>. If the provided data included information about the
standing stock of soil organic matter in a column titled `soil C` with
units of %, that translation will be noted by the data provided on the
Profile\_data tab of the key file. When run, the script will rename the
column titled `soil C` to `som_stock` and apply the appropriate units
conversion.

Each data set resides in its own subdirectory on the project Google
Drive directory. Data providers must provide a key file for each data
set.

### application

…need content about accessing and creating a key
    file…

### navigation

  - [installation](https://github.com/srearl/soilHarmonization#installation)
  - [data-homogenization](https://github.com/srearl/soilHarmonization#data-homogenization)
  - [homogenization-QC](https://github.com/srearl/soilHarmonization#homogenization-QC)
  - [keyfile-upversion](https://github.com/srearl/soilHarmonization#keyfile-upversion)

### installation

Install the current version from GitHub (after installing the `devtools`
package from CRAN):

``` r
devtools::install_github("srearl/soilHarmonization")
```

### data-homogenization

The `data_homogenization` script takes two input parameters:
`directoryName` and `temporaryDirectory`. `directoryName` is quoted the
name of the target Google Drive directory where the data and key file
are located. Note that you must have read + write access to the target
directory. `temporaryDirectory` is the quoted name and path of a
directory on your local computer where the script will write output
before uploading to the target Google Drive directory from which the
data and key file were accessed. Script output includes a combined notes
file and homogenized versions of the all provided data, each appended
with *HMGZD* in the file names.

Special notes about the `temporarydirectory`:

  - the full-path should be provided (e.g.,
    ‘/home/desktop/cdr\_output/’)
  - the directory name should feature a trailing forward (Linux, Mac) or
    backward (Windows) slash (but the script should add this if not
    provided)
  - the script will create the directory if it does not exist
  - the same `temporaryDirectory` can be used for multiple iterations
    but the script will delete any content so be sure to move or back up
    files in the `temporaryDirectory` that you wish to save

**running the homogenization function, example:**

``` r
data_homogenization(directoryName = 'Luquillo elevation gradient', 
                    temporaryDirectory = '~/Desktop/luq_homogenized')
```

### homogenization-QC

Following successful application of the `data_homogenization` script, a
quality-control function (`homogenization_QC`) may be used to assess
some aspects of the data homogeniztion process. `homogenization_QC`
performs three Q-C checks: (1) reports the number of rows in the
provided data file(s) and homogenized data file(s); (2) evalutes whether
all location data provided in the key file were successfully
incorporated into the homogenized data files(s); and (3) confirms that
all profile-level variables entered into the key file were included in
the homogenized data with a summary of those variables. In addition, the
script generates plots all treatment and experimental (i.e., considered
independent) variables identified in the key file against all dependent
variables identified in the key file. Box plots are generated when the
independent variable is categorical whereas scatter plots are generated
when the independent variable is numeric. Please keep in mind that the
plots are to provide only a general, visual assessment and comparison of
the data provided for error-checking purposes, and are not intended to
be exhaustive or of publication quality.

As with the `data_homogenization` script, `homogenization_QC` takes two
input parameters: `directoryName` and `temporaryDirectory`.
`directoryName` is the quoted the name of the target Google Drive
directory where the data, key file, and now homogenized data and notes
are located. Note that you must have read + write access to the target
directory. `temporaryDirectory` is the quoted name and path of a
directory on your local computer where the script will write output
before uploading to the target Google Drive directory from which the
files were accessed. Script output is a single html file with a file
name generated from the temporaryDirectory and directoryName, and
appended with "\_HMGZD\_QC.html". Please note that html files do not
render properly if opended in Google Drive, so the file should be
downloaded and opened using a web browser, or viewed from the
`temporaryDirectory` where a copy of the file will also reside.

Special notes about the `temporarydirectory`:

  - the full-path should be provided (e.g.,
    ‘/home/desktop/cdr\_output/’)
  - the directory name should feature a trailing forward (Linux, Mac) or
    backward (Windows) slash (but the script should add this if not
    provided)
  - the script will create the directory if it does not exist
  - the same `temporaryDirectory` can be used for multiple iterations
    but the script will delete any content so be sure to move or back up
    files in the `temporaryDirectory` that you wish to save

**running the quality-control function, example:**

``` r
homogenization_QC(directoryName = 'Luquillo elevation gradient', 
                  temporaryDirectory = '~/Desktop/luq_homogenized')
```

### keyfile-upversion

Early work with the SOM data indicated that additional details about the
data sets are required. To accomodate more detail, new additions to the
key file are needed. The key\_update\_v2 function addresses desired
changes to the key files. It is critical that information already
entered into key files was not lost, so the new key file features had to
be added to existing key file without information loss.

*key\_update\_v2 workflow:*

1.  download project key file with googledrive
2.  load downloaded (now xlsx) into R with openxlsx::loadWorkbook (see
    note)
3.  additions to the workbook object as needed
4.  prescribe validations with openxlsx::dataValidation
5.  fix styling as needed with openxlsx::createSytle/addStyle
6.  write workbook back to file with openxlsx::write.xlsx (or
    saveWorkbook)
7.  upload workbook back to project directory to be followed by a
    re-homog

*Key file version 2 new features include:*

1.  several new metadata fields in the location tab (‘time\_series’,
    ‘gradient’, ‘experiments’, ‘control\_id’, ‘number\_treatments’,
    ‘merge\_align’, ‘key\_version’).
2.  A new ‘logical’ column in the Units tab to facilitate a YES, NO
    drop-down option for several of the new metadata fields added to the
    location tab.
3.  Revised options in the Units tab for the list of drop-down options
    in the treatment rows of the Profile tab.
4.  Add pull-down menu for units field of lit\_lig in location tab.
5.  Clarify meanings (Var\_long field) of profle tab c\_tot and soc
    (bulk, not fraction).
6.  Duplicate var values in the Location and Profile tabs are renamed
    with unique names.

This workflow defaults to being run on Aurora with default file paths
set to that environment. These paths can be altered for the function to
work outside of the Aurora environemnt, but the lter-som group **SHOULD
NOT** do this as we need to document all changes, and a log file must
exist.

**running the key file update to version 2 function, example:**

``` r
key_update_v2('621_Key_Key_test')
```
