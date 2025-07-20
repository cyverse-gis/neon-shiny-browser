# NEON Spatial Data Management System

The NEON Shiny Browser now includes an automated spatial data management system that downloads, caches, and updates NEON spatial datasets from the official NEON Science website.

## Overview

The spatial data system automatically manages five key NEON spatial datasets:

1. **Field Site Boundaries** - Terrestrial field site sampling boundaries
2. **TOS Sampling Locations** - Terrestrial Observation System plot locations  
3. **Domain Polygons** - NEON domain boundary polygons
4. **Flight Boundaries** - AOP (Airborne Observation Platform) flight box boundaries
5. **Aquatic Watersheds** - Watersheds for NEON aquatic sites

## How It Works

### Automatic Updates
- On app startup, the system checks for cached spatial data
- If data is older than 30 days, it automatically checks for updates
- Downloads are only performed if newer versions are available
- All data is cached locally in the `NEON-data/` directory

### Manual Management
- New **"Spatial Data"** tab in the app provides management interface
- View status of all cached datasets
- Update individual datasets or force update all
- Monitor download progress and file sizes

## File Structure

```
NEON-data/
├── field_boundaries/
│   ├── field_boundaries_version.json
│   ├── Field_Sampling_Boundaries_202503.zip
│   └── field_boundaries/
│       └── [extracted shapefiles]
├── tos_plots/
│   ├── tos_plots_version.json
│   ├── All_NEON_TOS_Plots_V11.zip
│   └── tos_plots/
│       └── [extracted files]
├── domains/
├── flight_boundaries/
├── aquatic_watersheds/
└── [legacy files...]
```

## Usage

### In the App

1. **Automatic Operation**: 
   - Spatial data loads automatically on app start
   - Updates checked every 30 days
   - No user intervention required for normal operation

2. **Manual Control**:
   - Go to "Spatial Data" tab in the app
   - View current data status
   - Update individual datasets as needed
   - Force re-download if necessary

### Programmatic Access

```r
# Load the spatial data system
source('Functions/spatial_data_updater.R')
source('Functions/load_spatial_data.R')

# Initialize all spatial data
load_neon_spatial_data()

# Check data status
status <- get_spatial_data_status()

# Update specific dataset
update_spatial_dataset("field_boundaries")

# Force update all datasets
update_all_spatial_data(force_update = TRUE)

# Load specific datasets
boundaries <- load_field_boundaries()
plots <- load_tos_plots()
domains <- load_domain_polygons()
flights <- load_flight_boundaries()
watersheds <- load_aquatic_watersheds()
```

## Configuration

Spatial datasets are configured in `Functions/spatial_data_updater.R`:

```r
NEON_SPATIAL_CONFIG <- list(
  field_boundaries = list(
    name = "Field Site Boundaries",
    url = "https://www.neonscience.org/sites/default/files/Field_Sampling_Boundaries_202503.zip",
    cache_dir = "NEON-data/field_boundaries/",
    # ... other settings
  ),
  # ... other datasets
)
```

## Data Sources

All spatial data is sourced from the official NEON Science website:
- **Base URL**: https://www.neonscience.org/data-samples/data/spatial-data-maps
- **Data Format**: Zipped shapefiles and GeoJSON
- **Coordinate System**: Geographic WGS 84
- **Update Frequency**: As published by NEON (varies by dataset)

## Version Tracking

Each dataset includes version tracking:
- Download date and time
- Source URL and filename  
- File size information
- Cache location
- Extraction status

Version information is stored in JSON files:
```json
{
  "dataset": "Field Site Boundaries",
  "url": "https://www.neonscience.org/sites/default/files/Field_Sampling_Boundaries_202503.zip",
  "filename": "Field_Sampling_Boundaries_202503.zip", 
  "download_date": "2024-01-15 10:30:00",
  "file_size": 2548736,
  "extract_dir": "NEON-data/field_boundaries/field_boundaries"
}
```

## Performance Considerations

- **Disk Space**: Spatial datasets can be 10-50 MB each (50-250 MB total)
- **Download Time**: Initial download may take 5-15 minutes depending on connection
- **Cache Benefits**: Subsequent app starts are much faster using cached data
- **Update Frequency**: Only downloads when updates are available

## Troubleshooting

### Common Issues

1. **Download Failures**:
   - Check internet connection
   - NEON website may be temporarily unavailable
   - Try updating individual datasets instead of all at once

2. **Cache Corruption**:
   - Delete the problematic cache directory
   - Force update the specific dataset

3. **Disk Space**:
   - Spatial datasets require ~250 MB of disk space
   - Clean old versions from cache directories if needed

### Manual Cache Management

```bash
# View cache directories
ls -la NEON-data/*/

# Clear specific cache
rm -rf NEON-data/field_boundaries/

# Clear all spatial cache
rm -rf NEON-data/field_boundaries/ NEON-data/tos_plots/ NEON-data/domains/ NEON-data/flight_boundaries/ NEON-data/aquatic_watersheds/
```

### Testing

Test the spatial data system:

```bash
# Run test script
Rscript test_spatial_update.R

# Test in R console
source("Functions/spatial_data_updater.R")
get_spatial_data_status()
```

## Migration from Legacy System

The new system maintains backward compatibility:

- **Legacy flight data**: Still supported if new system fails
- **Existing cache**: Old `NEON-data/` files are preserved
- **Gradual transition**: New data supplements existing rather than replacing

Legacy flight data directories:
- `NEON-data/Flightdata/Flight_boundaries_2016/`
- `NEON-data/Flightdata/Flight_boundaries_2017/`

These will be used as fallback if the new flight boundary system fails.

## Future Enhancements

Potential improvements:
- **Incremental updates**: Only download changed files
- **Compression**: Reduce cache size with better compression
- **Background updates**: Update data while app is running
- **Metadata integration**: Include NEON metadata with spatial data
- **User notifications**: Alert users when updates are available