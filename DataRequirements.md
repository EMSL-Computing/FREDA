# Data Requirements

Analysis with FREDA requires uploading two .csv files

### File 1: e_data

This file contains the quantified data for each mass (rows) and sample (columns)

- The first row of the file is the header and contains names for each column.

- One of the columns must contain the unique mass identifiers, and this column name must be specified after uploading the e_data file on the Upload tab.

- All remaining columns contain the peak heights associated with each mass for the samples in the dataset. The column names for these remaining columns are the sample identifiers.

### File 2: e_meta

This file contains the molecular formula and/or elemental counts for each mass, and optionally an indicator for Carbon13

- The first row is the header and contains names for each column.

- One of the columns must contain the unique mass identifiers, and this column name must match the unique mass identifier column name in the e_data file.

- Additional column(s) contain

      - Either the molecular formula or the counts for C, H, O, N, S, and P
      
      - An optional indicator for Carbon13
      
      - Other optional information that is specific to each mass

### Example Files

Download example e_data and e_meta files for use with FREDA

