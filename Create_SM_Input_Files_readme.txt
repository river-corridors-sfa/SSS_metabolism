1 Nov 2024
Summary of the script from AI Incubator with edits from Brieanne Forbes


# Stream Metabolizer Input File Creation Script

This script processes sensor data from the SSS data package published at [ESS-DIVE](https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1969566) to create input files for the Stream Metabolizer tool.

## Script Description

1. **Setup and Initialization:**
   - Removes all existing objects in the environment.
   - Loads necessary libraries.
   - Sets the working directory to the location of the current script.

2. **File Discovery:**
   - Identifies relevant data files (Dissolved Oxygen, metadata, barometric pressure, depth) from the SSS data package directories.

3. **Plot Theme Setup:**
   - Defines a common theme for all generated plots.

4. **Empty Tibble Creation:**
   - Creates an empty tibble to store time differences between transect depths and sensor measurement times.

5. **Data Processing Loop:**
   - Iterates through Dissolved Oxygen files, performing the following steps for each file:
     - Reads the data file and extracts relevant columns.
     - Subsets the data to 15-minute intervals.
     - Cleans the data using the `tsrobprep` library to remove outliers and fix missing values.
     - Adjusts data for specific sample days based on metadata.
     - Ensures cleaned values are within expected ranges, reverting to original data if necessary.

6. **Biofouling Removal:**
   - Removes data points affected by biofouling based on pre-identified dates for specific sites:
      - `SSS005`: Remove everything after 8/15
      - `SSS016`: Remove everything after 8/23
      - `SSS028`: Remove 8/22-8/23 and 8/26-8/27
      - `SSS046`: Remove everything after 8/26

7. **Input File Creation:**
   - Merges Dissolved Oxygen data with barometric and hobo (depth) data.
   - Calculates water depth from pressure using site-specific adjustments and linear models for missing data.
   - Handles missing depth data for specific sites as follows:
     - **`SSS024` (W20):** Interpolates pressure and temperature from nearby site `SSS036` (W10) due to missing hobo data.
     - **`SSS013` (T07):** Uses USGS discharge and a rating curve to estimate depth due to missing hobo data for the beginning of the time series.
   - Computes average depth and time series depth with site-specific offsets for other Parent IDs with special handling of calculation timing:
     - **`SSS003`, `SSS005`, `SSS014`, `SSS015`, `SSS016`, `SSS017`, `SSS024`, `SSS011`:** Finds the closest time AFTER the transect depth was taken.
     - **`SSS010`, `SSS023`, `SSS036`:** Finds the closest time BEFORE the transect depth was taken.

8. **Output Files:**
   - Creates cleaned input files with necessary columns for the Stream Metabolizer tool.
   - Saves the input files to `./Stream_Metabolizer/Inputs/Sensor_Files/`.

9. **Plot Generation:**
   - Generates interactive plots for Dissolved Oxygen, Temperature, Pressure, and Depth.
   - Saves the plots as HTML files in `./Stream_Metabolizer/Inputs/Sensor_Files/Plots/`.

10. **Time Difference Calculation:**
   - Computes the time differences between transect depth measurements and hobo data for specific sites.

## Output

- **Cleaned Input Files:** Located in `./Stream_Metabolizer/Inputs/Sensor_Files/`.
- **Interactive Plots:** Saved in `./Stream_Metabolizer/Inputs/Sensor_Files/Plots/`.
