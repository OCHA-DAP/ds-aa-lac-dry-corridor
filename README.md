# Central American Dry Corridor

[![Generic badge](https://img.shields.io/badge/STATUS-ENDORSED-%231EBFB3)](https://shields.io/)

## Directory structure

The code in this repository is organized as follows:

```shell
├── _targets.R    # Main analytical pipeline to produce thresholds for AA monitoring
├── analysis      # Any remaining analytical work/visualization not in _targets, but published elsewhere
├── exploration   # Experimental work not intended to be replicated
├── src           # Code to run monitoring system pipeline
|
├── .gitignore
├── README.md
└── requirements.txt

```

## Reproducing this analysis

There are several fairly significant data downloading and processing steps that must be run before 
initiating the analytical pipeline. 

Therefore you should create a directory where you would like the data to be stored,
and point to it using an environment variable called
`AA_DATA_DIR`. You can then download the data by running the `.R` & `.py`
scripts contained in the `data-raw` folder.

The bulk of the analytical pipeline is controlled by a standard targets pipeline contained in `_targets.R`.
Install targets with 

```r
install.packages("targets")
```

You can then initiate the pipeline by running from anywhere within the project
or repo.

```r
targets::tar_make()
```

## Monitoring

The monitoring system is implemented independently of the analytical pipeline.
The only input from the analytical pipeline is the table of thresholds which is
currently stored in our blob storage `projects/ds-aa-lac-dry-corridor/thresholds_CDs_INSIV.parquet`

The monitoring is not fully automated as the Guatemala INSIVUMEH forecast data is not obtained
entirely systematically. The current protocol is that someone from INSIVUMEH
should share the updated forecast 1x per month (between the 5th & 10th).
To ensure integration of the INSIVUMEH forecast the CHD DS analyst responsible
should email the INSIVUMEH contact point and monitor the response/data manually.

Therefore on the 6th of each month when the ECMWF forecast in the Copernicus Data Store (CD)
is updated. We manually run:

1. `src/01_download_latest_ecmwf_cds.R`
2. `src/02_monitor_forecast.R`

Step 1 will download the latest forecast for the AOI to the blob whereas step 2
will implement the monitoring and send the email.

Step 2 - When the code from step 2 is executed it will automatically detect if the 
INSIVUMEH forecast has been updated (by checking specific folder).

**preliminary run:** On 6th of the month, the INSIVUMEH forecast will not be detected and a "preliminary"
email update will be sent with Guatemala status excluded.

**final run:** Can be performed as soon as the INSIVUNEH forecast is received. You must first move the folders shared
by INSIVUMEH to the folder below

```r
file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "new_format")
```

When the files are in the path again you can run 2. `src/02_monitor_forecast.R` which will auto detect
the files and send the final email with Guatemala status included.


## Development

If you would like to instead receive the processed data from our team, please
[contact us](mailto:centrehumdata@un.org).
