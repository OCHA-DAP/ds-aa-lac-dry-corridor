"""
Download historical CHIRPS monthly precipitation data for Central America AOIs.

Extracts zonal mean statistics for admin 1 regions from 1981 to present.
Uses FAO GAUL admin 1 boundaries.

AOIs:
    - Guatemala: Chiquimula, Baja Verapaz, Quiché
    - Honduras: El Paraíso, Francisco Morazán
    - El Salvador: San Vicente
"""

import re

import ee
import geemap
import ocha_stratus as stratus
import pandas as pd

ee.Initialize()

# Configuration
# Mapping: {country: [list of admin1 names as they appear in GAUL]}
# Note: Names may need adjustment based on GAUL spelling/accents
AOI_CONFIG = {
    "Guatemala": ["Chiquimula", "Baja Verapaz", "Quiché"],
    "Honduras": ["Paraiso", "Francisco Morazan"],
    "El Salvador": ["San Vicente"],
}

START_DATE = "1981-01-01"
END_DATE = "2026-02-01"  # Exclusive
SCALE = 5566  # ~0.05 degree, matches CHIRPS resolution

BLOB_NAME = "ds-aa-lac-dry-corridor/raw/chirps/2026_cadc_drought_v3_aoi_chirps_monthly_historical.parquet"


def get_monthly_chirps():
    """
    Get CHIRPS daily data aggregated to monthly sums.

    Returns ImageCollection with monthly precipitation totals.
    """
    chirps_daily = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY")

    # Create list of year-month combinations
    start = ee.Date(START_DATE)
    end = ee.Date(END_DATE)

    # Generate monthly dates
    n_months = end.difference(start, "month").round()
    months = ee.List.sequence(0, n_months.subtract(1))

    def aggregate_month(month_offset):
        """Aggregate daily CHIRPS to monthly sum."""
        month_offset = ee.Number(month_offset)
        month_start = start.advance(month_offset, "month")
        month_end = month_start.advance(1, "month")

        monthly_sum = (
            chirps_daily.filterDate(month_start, month_end)
            .sum()
            .rename("precipitation")
            .set("system:time_start", month_start.millis())
            .set("month", month_start.get("month"))
            .set("year", month_start.get("year"))
        )
        return monthly_sum

    monthly_ic = ee.ImageCollection.fromImages(months.map(aggregate_month))
    return monthly_ic


def get_aoi_features():
    """
    Get admin 1 features for all AOI regions.

    Returns FeatureCollection filtered to AOI regions.
    """
    adm1_fc = ee.FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level1")

    # Build filter for all countries and regions
    filters = []
    for country, regions in AOI_CONFIG.items():
        country_filter = ee.Filter.And(
            ee.Filter.eq("ADM0_NAME", country),
            ee.Filter.inList("ADM1_NAME", regions),
        )
        filters.append(country_filter)

    combined_filter = ee.Filter.Or(*filters)
    fc_filtered = adm1_fc.filter(combined_filter)

    return fc_filtered


def print_available_admin1_names():
    """
    Helper function to print available admin 1 names for our target countries.
    Useful for finding exact spelling/accents in GAUL dataset.
    """
    adm1_fc = ee.FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level1")

    for country in AOI_CONFIG.keys():
        country_fc = adm1_fc.filter(ee.Filter.eq("ADM0_NAME", country))
        names = country_fc.aggregate_array("ADM1_NAME").getInfo()
        print(f"\n{country} admin1 names:")
        for name in sorted(names):
            print(f"  - {name}")


def pivot_to_long(df):
    """
    Pivot wide zonal stats DataFrame to long format.
    """
    id_cols = ["ADM0_NAME", "ADM1_CODE", "ADM1_NAME", "Shape_Area"]
    data_pattern = re.compile(r"^\d{6}_")
    data_cols = [c for c in df.columns if data_pattern.match(c)]

    df_long = df[id_cols + data_cols].melt(
        id_vars=id_cols,
        value_vars=data_cols,
        var_name="band_name",
        value_name="value",
    )

    # Parse date from band_name (e.g., '198101_precipitation')
    df_long["date"] = pd.to_datetime(
        df_long["band_name"].str[:6], format="%Y%m"
    )
    df_long["parameter"] = df_long["band_name"].str[7:]

    # Reorder columns
    df_long = df_long[
        [
            "ADM0_NAME",
            "ADM1_CODE",
            "ADM1_NAME",
            "Shape_Area",
            "band_name",
            "date",
            "parameter",
            "value",
        ]
    ]

    return df_long


def main():
    print("Setting up CHIRPS extraction for Central America AOIs...")

    # Get AOI features
    fc_filtered = get_aoi_features()
    n_features = fc_filtered.size().getInfo()

    # Calculate expected number of features
    expected_count = sum(len(regions) for regions in AOI_CONFIG.values())
    print(f"Admin 1 regions selected: {n_features} (expected: {expected_count})")

    # Print selected features
    feature_names = fc_filtered.aggregate_array("ADM1_NAME").getInfo()
    feature_countries = fc_filtered.aggregate_array("ADM0_NAME").getInfo()
    print("Selected regions:")
    for country, name in zip(feature_countries, feature_names):
        print(f"  - {country}: {name}")

    # Validate all regions were found
    if n_features != expected_count:
        print("\nERROR: Not all regions were matched!")
        expected_regions = {
            (country, region)
            for country, regions in AOI_CONFIG.items()
            for region in regions
        }
        found_regions = set(zip(feature_countries, feature_names))
        missing = expected_regions - found_regions
        print("Missing regions:")
        for country, region in sorted(missing):
            print(f"  - {country}: {region}")
        print("\nAvailable names in GAUL for reference:")
        print_available_admin1_names()
        return None

    # Get monthly CHIRPS data
    print("\nAggregating CHIRPS daily to monthly...")
    ic_monthly = get_monthly_chirps()
    n_images = ic_monthly.size().getInfo()
    print(f"Monthly images to process: {n_images}")

    # Convert ImageCollection to multi-band image
    # Add year-month prefix to band names
    def add_date_prefix(img):
        date = ee.Date(img.get("system:time_start"))
        year = date.get("year").format("%04d")
        month = date.get("month").format("%02d")
        prefix = year.cat(month).cat("_")
        return img.regexpRename("^", prefix)

    ic_renamed = ic_monthly.map(add_date_prefix)
    img_bands = ic_renamed.toBands()

    # Clean up band names (remove numeric prefix added by toBands)
    band_names = img_bands.bandNames()
    clean_names = band_names.map(
        lambda name: ee.String(name).replace("^\\d+_", "", "g")
    )
    img_bands = img_bands.rename(clean_names)

    n_bands = img_bands.bandNames().size().getInfo()
    print(f"Total bands: {n_bands}")

    # Run zonal statistics
    print("\nRunning zonal statistics (this may take several minutes)...")
    result_fc = geemap.zonal_stats(
        in_value_raster=img_bands,
        in_zone_vector=fc_filtered,
        stat_type="MEAN",
        scale=SCALE,
        tile_scale=4,
        return_fc=True,
        verbose=True,
        timeout=600,
    )

    # Convert to DataFrame
    print("\nConverting to DataFrame...")
    features = result_fc.getInfo()["features"]
    rows = [f["properties"] for f in features]
    df = pd.DataFrame(rows)
    print(f"Wide format shape: {df.shape}")

    # Pivot to long format
    print("Pivoting to long format...")
    df_long = pivot_to_long(df)
    print(f"Long format shape: {df_long.shape}")
    print(f"Countries: {df_long['ADM0_NAME'].unique().tolist()}")
    print(f"Regions: {df_long['ADM1_NAME'].unique().tolist()}")
    print(f"Date range: {df_long['date'].min()} to {df_long['date'].max()}")

    # Upload to blob
    print(f"\nUploading to blob: {BLOB_NAME}")
    stratus.upload_parquet_to_blob(
        df=df_long,
        blob_name=BLOB_NAME,
        stage="dev",
        container_name="projects",
    )
    print("Upload complete!")

    return df_long


if __name__ == "__main__":
    df = main()
