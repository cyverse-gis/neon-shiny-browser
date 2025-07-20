# Testing Guide for NEON Shiny Browser

This document describes how to test the updated NEON Shiny Browser application after the major refactoring to replace the deprecated `nneo` package with `neonUtilities` equivalents.

## Prerequisites

### R Version
- **Minimum**: R 4.0.0 or higher
- **Recommended**: R 4.4.x (latest stable)

### Required Packages
Run the installation script to install all dependencies:
```r
source("Install.R")
```

This will install:
- Core Shiny packages: `shiny`, `shinythemes`, `shinyWidgets`, `shinyBS`, `shinyjs`
- Mapping: `leaflet`, `leaflet.extras` 
- Data processing: `neonUtilities`, `sf`, `geosphere`, `jsonlite`, `dplyr`, `DT`
- HTTP: `crul`, `httr`
- Testing: `testthat`

## Test Suite

### Running Tests

#### Option 1: Using the Test Runner
```r
source("run_tests.R")
```

#### Option 2: Manual Test Execution
```r
library(testthat)
test_dir("tests/testthat", reporter = "detailed")
```

#### Option 3: Individual Test Files
```r
# Test neonUtilities replacement functions
test_file("tests/testthat/test-neonUtilities-replacements.R")

# Test app functionality  
test_file("tests/testthat/test-app-functionality.R")

# Test download integration
test_file("tests/testthat/test-download-integration.R")
```

### Test Categories

#### 1. API Replacement Tests (`test-neonUtilities-replacements.R`)
- **Purpose**: Verify that the new neonUtilities-based functions return data in the same format as the deprecated nneo functions
- **Key Tests**:
  - `nneo_products_replacement()` returns correct tibble structure with list columns
  - `nneo_data_replacement()` returns nested list with `$data$files` structure  
  - `nneo_site_replacement()` returns list with `$dataProducts` data.frame
  - API token handling works correctly
  - Error handling for invalid parameters

#### 2. App Functionality Tests (`test-app-functionality.R`)
- **Purpose**: Test overall app initialization and core functionality
- **Key Tests**:
  - Global variables load correctly
  - Filter functions work with new data structures
  - Download functions accept API tokens
  - Data structure compatibility with existing UI code
  - App launches without errors

#### 3. Download Integration Tests (`test-download-integration.R`)
- **Purpose**: Test the complete download workflow including API integration
- **Key Tests**:
  - `getPackage()` and `byFileAOP()` functions accept token parameters
  - Download size calculations use HTTPS endpoints
  - Data availability checking works correctly
  - Error handling for invalid inputs
  - Performance and timeout handling

### Test Configuration

Tests are configured to:
- **Skip on CRAN**: Many tests are marked `skip_on_cran()` since they require internet access
- **Skip if API down**: Tests check if NEON API is responding before running
- **Use test data**: Tests use known product/site combinations that should have data
- **Handle network issues gracefully**: Network failures are expected and handled

### Test Data Used

The tests use these known-good combinations:
- **Product**: `DP1.00002.001` (Single aspirated air temperature)  
- **Site**: `HARV` (Harvard Forest)
- **Date**: `2018-07` (Summer 2018, likely to have data)

## Manual Testing

### 1. Basic App Launch Test
```r
# Source global environment
source("Global.R")

# Check that key variables are loaded
exists("FieldSite_point")
exists("FieldSite_abbs") 
exists("NEONproducts_product")  # This should load from API

# Launch app
library(shiny)
runApp()
```

### 2. API Token Testing
1. Go to [NEON Data Portal](https://data.neonscience.org/myaccount)
2. Create account and generate API token
3. In the app, enter token in the "NEON API Token" field
4. Try downloading data - should be faster with token

### 3. Download Testing
1. **Browse by Site**:
   - Select site (e.g., "HARV - Harvard Forest")  
   - Choose a data product with available data
   - Go to Download tab, select dates
   - Test download (may take time)

2. **Browse by Product**:
   - Select product (e.g., "DP1.00002.001")
   - Choose site and dates
   - Test download

3. **AOP Downloads**:
   - Select AOP product (e.g., "DP3.30018.001")
   - Choose site and year
   - Test size calculation
   - Test download (these are large!)

### 4. Error Handling Testing
Test these error conditions:
- Invalid API tokens
- Products with no available data
- Network connectivity issues  
- Invalid product/site combinations

## Expected Behavior Changes

### ✅ What Should Work the Same
- All UI functionality and appearance
- Map browsing and filtering
- Product search and selection
- Download workflows
- File stacking and processing

### 🆕 What's New/Improved
- **HTTPS**: All API calls now use secure HTTPS
- **API Tokens**: Optional token support for faster downloads
- **R 4.4 Compatibility**: Works with modern R versions
- **Better Error Handling**: More informative error messages
- **Modern Dependencies**: Updated package versions

### ⚠️ Potential Issues
- **First Load**: May be slower as it fetches fresh data from API instead of using cached nneo results
- **Network Dependency**: More dependent on NEON API availability
- **Token Requirement**: Some functionality may require API token during high-traffic periods

## Troubleshooting

### Common Issues

#### "Cannot reach NEON API"
- Check internet connection
- NEON API may be down for maintenance
- Try again later

#### "R version too old"
- Update to R 4.0 or higher
- Run `R.version` to check current version

#### Tests failing with network errors
- Expected if NEON API is unavailable
- Tests marked with `skip_if_offline()` should skip automatically

#### App takes long time to load
- First load fetches all products from API (can be slow)
- Consider getting API token for better performance
- Check NEON API status at [https://data.neonscience.org/](https://data.neonscience.org/)

### Performance Tips

1. **Use API Token**: Get token from NEON account for faster API access
2. **Local Data**: App still caches some static data locally in `NEON-data/` directory
3. **Stable Network**: Ensure stable internet connection for API calls

## Regression Testing Checklist

When testing the updated app, verify:

- [ ] App launches successfully
- [ ] Map displays field sites correctly  
- [ ] Site filtering works (by state, type, habitat)
- [ ] Product browsing works (by site and by product)
- [ ] Keyword/theme filtering works
- [ ] Product details display correctly
- [ ] Download size calculation works
- [ ] Regular downloads work
- [ ] AOP downloads work  
- [ ] API token input accepts tokens
- [ ] Download progress bars work
- [ ] Error messages are informative
- [ ] Download summaries are created
- [ ] Files are stacked correctly

## Development Testing

For developers making changes:

### Adding New Tests
1. Create test file in `tests/testthat/`
2. Follow naming convention: `test-[feature].R`
3. Use appropriate `skip_*()` functions for network/API tests
4. Include both positive and negative test cases

### Running Continuous Integration
The test suite is designed to work with CI/CD systems:
```bash
# In CI environment
Rscript -e "testthat::test_dir('tests/testthat')"
```

### Mock Testing
For testing without network access, consider mocking API responses:
```r
# Example mock setup
mock_products <- tibble(
  productCode = "DP1.00001.001",
  productName = "Mock Product", 
  siteCodes = list(data.frame(siteCode = c("HARV", "BART")))
)
```