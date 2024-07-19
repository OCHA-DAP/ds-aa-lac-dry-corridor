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
entirely systematically.  The current protocol is that someone from INSIVUMEH
should share the updated forecast 1x per month (between the 5th & 10th).
However, to ensure integration of the INSIVUMEH forecast the CHD DS analyst responsible
should email the INSIVUMEH contact point and monitor the response/data manually. The INSIVUMEH
contact typically shares the national forecast data as `.nc` files in a shared gdrive folder. Once
the data is shared the CHD analyst should load the new files into the blob in the `dev` `global` 
container with the following file path syntax:

```
"raster/raw/insivumeh_pronos_deterministic_forecast_{valid_month}_start{publication_month}{YYYY}.nc"
```

Where both `valid_month` and `publication_month` are abbreviated month names (i.e "Jun", "Aug").
**Note* the name should be the same format as that provided by INSIVUMEH just with the prefix of
"insivumeh" added.

Due to the delay in receiving the national forecast in guatemala the trigger monitoring system must be 2 separate runs that
occur on different days:

**preliminary run:** On 6th of the month, the INSIVUMEH forecast will not be detected and a "preliminary"
email update will be sent with Guatemala status excluded.

**final run:** Can be performed as soon as the INSIVUNEH forecast is received. You must first move the folders shared
by INSIVUMEH to the blob file path listed above.



Therefore on the 6th of each month when the ECMWF forecast in the Copernicus Data Store (CD)
is updated. We manually run:

1. `src/01_download_latest_ecmwf_cds.R`
2. `src/02_monitor_forecast.R`

Step 1 will download the latest forecast for the AOI to the blob.

Step 2 will load the forecast data, threshold, and determine and send a monitoring email indicating whether the thresholds
have been breached for any country. When the code from step 2 is executed it will automatically detect if the 
INSIVUMEH forecast has been updated (by checking specific blob path). **Note** the status for Guatemala is not
provided in this email as the National forecast is the official forecast for the mechanism.

Once the INSIVUMEH forecast has been received and migrated to the appropriate blob location, the analyst must re-run 
step 2 which will provide a final email update for the month with the status for Guatemala included

## Development

If you would like to instead receive the processed data from our team, please
[contact us](mailto:centrehumdata@un.org).
