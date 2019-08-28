# Data Requirements

***

FREDA begins analysis after molecular composition (formulae or elemental counts) have been assigned to observed peaks, where possible. Analysis with FREDA requires uploading two .csv files

### File 1: Data File

***

This file contains the quantified peak intensity for each mass (rows) and sample (columns)

- The first row of the file is the header and contains names for each column.

- One of the columns must contain the unique mass identifiers (i.e. mass values which will be used to calculate Kendrick mass and defect, if requested), and this column name must be specified after uploading the data file on the Upload tab.

- All remaining columns contain the peak intensities associated with each mass for the samples in the dataset. The column names for these remaining columns are the sample identifiers.

### File 2: Molecular Identification File

***

This file contains the molecular formula and/or elemental counts for each mass, and optionally column indicating isotopic peaks

- The first row is the header and contains names for each column.

- One of the columns must contain the unique mass identifiers.  This column name must match the unique mass identifier column name in the data file, and all masses found in the Data File must also be in this file.

- Additional column(s) contain

      - Either the molecular formula or the counts for C and H (required) and O, N, S, and P (optional)
      
      - An optional indicator for isotopic peaks
      
      - Other optional information that is specific to each mass

***

### Example Files

Download example data and molecular identification files for use with FREDA

