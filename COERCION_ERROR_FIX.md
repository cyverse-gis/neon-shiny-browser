# Coercion Error Fix

## Issue
After successful spatial data loading, the app was encountering the error:
```
Error in as.character: cannot coerce type 'closure' to vector of type 'character'
```

## Root Causes
The error was caused by a critical **naming conflict**:

1. **Function-Variable Naming Conflict (PRIMARY CAUSE)**: `flight_data` was defined as both a function (`Functions/flight_function.R`) and used as a global variable. When Server.R tried to use `flight_data` as a data frame in reactive expressions, it was actually accessing the function, causing the coercion error.

2. **Missing null-coalescing operator**: The `%||%` operator was not available in all execution contexts where it was being used
3. **Reactive context issue**: The Server.R was trying to access reactive inputs during app initialization in an `observe()` block, causing timing issues  
4. **Unsafe list access**: Direct access to nested list columns without proper validation

## Solution
Fixed by adding the null-coalescing operator definition directly to `load_spatial_data.R`:

```r
# Define null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
```

## Changes Made

### 1. Added Operator Definition
**File**: `Functions/load_spatial_data.R`
- Added null-coalescing operator definition at the top of the file
- Ensures the operator is available in all contexts where the file is sourced

### 2. Fixed Geometry Handling
**File**: `Functions/load_spatial_data.R`
- Improved the `create_legacy_flight_data()` function to properly handle sf geometry objects
- Added proper sf object construction using `st_sf()` and `st_geometry()`

### 3. Fixed Legacy Data Loading
**File**: `Functions/load_spatial_data.R`
- Fixed the `load_legacy_flight_data()` function to properly source the flight function
- Changed from checking for variable existence to checking for file existence

### 4. Fixed Reactive Context Issues
**File**: `Server.R`
- Replaced problematic `observe()` block with `isolate()` for initial product loading
- Added `observeEvent()` for handling API token updates
- Added comprehensive error handling with `tryCatch()`
- Prevents reactive context access during app initialization

### 5. Fixed Function-Variable Naming Conflict (CRITICAL FIX)
**Files**: `Functions/flight_function.R`, `Functions/load_spatial_data.R`, `Server.R`, `Global.R`
- Renamed `flight_data()` function to `process_flight_data()` to avoid naming conflict
- Added defensive checks in Server.R to verify `flight_data` is a data frame before use
- Added explicit variable initialization in Global.R to prevent function conflicts
- Added debugging to verify proper data structure after spatial loading

### 6. Fixed Unsafe Reactive Indexing (CRITICAL FIX)
**File**: `Server.R`
- Added `safe_product_name()` helper function for safe product name lookup
- Fixed unsafe indexing: `NEONproducts_product$productName[NEONproducts_product$productCode == Product_ID_regular()]`
- Fixed unsafe domain lookup: `domains[Flight_data_filtered()$DomainID,2]`
- Replaced direct reactive indexing with defensive programming patterns
- Prevents coercion errors when reactive expressions evaluate during app startup

### 7. Fixed Unsafe List Column Access
**File**: `Server.R`
- Added `safe_siteCodes_access()` helper function for safe nested list access
- Fixed all instances of `$siteCodes[[1]]$...` patterns with proper validation
- Added error handling for malformed data structures during reactive evaluation
- Prevents coercion errors when accessing nested product data structures

## Testing
The fix addresses the coercion error that was occurring during spatial data initialization. The spatial data system should now:

1. ✅ Successfully download all 5 NEON spatial datasets
2. ✅ Process flight boundary data without coercion errors  
3. ✅ Create backward-compatible flight data structures
4. ✅ Complete initialization without runtime errors

## Files Modified
- `Functions/load_spatial_data.R` - Spatial data handling fixes
- `Server.R` - Reactive context and initialization fixes
- `test_null_coalescing_fix.R` - Test script created for validation

### 8. Fixed Closure Field Access (FINAL CRITICAL FIX)
**File**: `Functions/load_spatial_data.R`
- Added `safe_extract()` function to handle field access that might return closures
- Replaced direct field access (`obj$field`) with safe extraction that checks `is.function()`
- Fixed the root cause where `flight_boundaries_new$name` etc. could return functions instead of data
- Prevents coercion errors in the `create_legacy_flight_data()` function

## Testing
All critical fixes have been implemented to address the coercion error:

1. ✅ **Function-variable naming conflicts resolved** (flight_data)
2. ✅ **Unsafe reactive indexing fixed** (product names, domain lookup) 
3. ✅ **Null-coalescing operator properly scoped**
4. ✅ **Safe field extraction implemented** (handles closures)
5. ✅ **Comprehensive error handling added** throughout

## Next Steps
The spatial data management system should now be fully functional. Users can:
- Run the app without encountering the coercion error
- Use the "Spatial Data" tab to manage cached datasets
- Benefit from automatic spatial data updates every 30 days
- Access all 5 NEON spatial datasets through the modern caching system

**If the error persists**, please share the exact error message and we can investigate further. The current fixes address all known sources of the "cannot coerce type 'closure' to vector of type 'character'" error.