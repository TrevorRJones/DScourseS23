-- Create a table to store the data
CREATE TABLE florida_insurance (
	policyID INTEGER,
	statecode TEXT,
	county TEXT,
	eq_site_limit REAL,
	hu_site_limit REAL,
	fl_site_limit REAL,
	fr_site_limit REAL,
	tiv_2011 REAL,
	tiv_2012 REAL,
	eq_site_deductible REAL,
	hu_site_deductible REAL,
	fl_site_deductible REAL,
	fr_site_deductible REAL,
	point_latitude REAL,
	point_longitude REAL,
	line TEXT,
	construction TEXT,
	point_granularity INTEGER
);

-- Import the data from the CSV file into the table
.mode csv
.import FL_insurance_sample.csv florida_insurance

-- Print out the first 10 rows of the data set
SELECT * FROM florida_insurance LIMIT 10;

-- List which counties are in the sample (i.e. list unique values of the county variable)
SELECT DISTINCT county FROM florida_insurance;

-- Compute the average property appreciation from 2011 to 2012 (i.e. compute the mean of tiv_2012 - tiv_2011)
SELECT AVG(tiv_2012 - tiv_2011) FROM florida_insurance;

-- Create a frequency table of the construction variable to see what fraction of buildings are made out of wood or some other material
SELECT construction, COUNT(*) AS frequency
FROM florida_insurance
GROUP BY construction;

