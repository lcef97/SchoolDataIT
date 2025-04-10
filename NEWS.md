# SchoolData 0.2.5

* Major changes: fixed error in Set_DB function not allowing to run 'nstud_filter_by_grade'; 
allows to insert grouped 'input_nstud' input in Set_DB; allows 'Util_Invalsi_filter' to filter multiple years; 
allows 'Get_AdmUnNames' to work with any generic past or present date (function call input has changed from c(year, "mm_dd") to "yyyy-mm-dd")



# SchoolDataIT 0.2.4

* Major changes: allows 'Util_nstud_wide' to both filter schools with class size in an order-specific interval,
and to filter at the level of school grade rather than at the school level
* Minor changes: fixed some urls in .Rd files
* Fixed 'Get_nteachers_prov' to correctly download 2015/16 data


# SchoolDataIT 0.2.3

* Adapted 'Get_Invalsi_IS' to the new format of the input DB (experimental)

* 'Map_Invalsi' can now work with input data from 'Util_Invalsi_filter'

# SchoolDataIT 0.2.2

* Enforced gentle failure

* Changed default palette to 'viridis'

* Possible to plug numeric and grouped school buildings data into 'Set_DB'

* Changed source URL format in Get_Invalsi_IS description

# SchoolDataIT 0.2.1

* Experimental features: integrated the counts of students by school running time in the 'nstud' workflow; the user can now include safety certifications in the school buldings DB (not active by default since it requires extra computational time other than downloading time)

* Minor changes: function 'Get_Shapefile' can now provide either the boundaries or the centroids of NUTS-3 and LAU administrative units; fixed warning message in 'Group_nstud';
'Map_DB' allows to map municipality data at province level


# SchoolDataIT 0.2.0

* Removed 'Get_RiskMap' because the input dataset does not appear to be currently available.

* Redirected 'Get_AdmUnNames' to a specific repository of input datasets due to changes in provider website.

* Removed 'readxl' from dependencies since no xls input files are scraped anymore.


# SchoolDataIT 0.1.3
* Fixed bug due to geometry field in functions 'Map_DB' and 'Map_SchoolBuildings'; corrected argument 'col.rev' to 'col_rev' in function 'Map_Invalsi'

# SchoolDataIT 0.1.2

* Minor changes: fixed 'Set_DB' for the case in which Invalsi Data must not be included; 
extended internal function 'School.order' to work with the schools in the Valle d'Aosta region; renamed 'data' the input dataframe of internal function 'Group_BroadBand';
renamed 'Registry_from_registry' and 'Registry_from_buildings' the arguments previously referred to as 'Registry2' and 'Registry'.

# SchoolDataIT 0.1.1
 
* Implemented gentle failure methods in html reading.

# SchoolDataIT 0.1.0

* Initial CRAN submission.

