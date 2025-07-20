1. ALL_NEON_TOS Shapefiles and kmzs
Shapefiles for active sampling locations of the Terrestrial Observation System (TOS). 
The sampling locations were created by stratifying vegetation types from the National Land Cover Database (NLCD). 
Points were distributed in each vegetation type using a spatially balanced system implemented in GIS called the Reversed Randomized Quadrant Recursive Raster (RRQRR) technique. 
Subsequently, the points went through a remote sensing and ground-truth procedures to validate the vegetation type and NEON criteria. 
For more information, please contact the Permitting Department at NEON, contact information is available at www.neonscience.org. 
More information about the TOS Spatial Design is available in NEON.DOC.004243 and information about the plot establishment method is in NEON.DOC.001025.  

Centroids
One record per plot, spatial information is for the plot centroid.
Points
Records for each sampling point within a plot.  
Polygons
One record per plot, spatial information is for the plot centroid.
Subplots
One record per subplot.  The spatial information is for the subplot centroid. 


2. nlcdCoverBySite.csv
The nlcdCoverBySite.csv describes the cover classes and associated area at each NEON site as described by the National Land Cover Database (NLCD) when plot establishment occurred. 
Site Characterization Reports found in the NEON Document Library contain the same information in portal document format (pdf). 
The NLCD area by site are integral to the allocation (target cover types greater than 5% of the area are sampled) and stratification (plots are proportional to area for most sample designs) of the Terrestrial Observation System (TOS) spatial sampling design, and can facilitate design-based inference of the data. 
For more information, see the TOS Science Design for Spatial Sampling (NEON.DOC.000913) and site-specific Site Characterization Reports.


3. TOS_decommissionedPlots.csv
Table of TOS plots where sampling no longer occurs. 
Plots are established with the intention of long-term sampling but changes in logistics, safety, or the site sampling design can result in plots being dropped and replaced throughout time.  


4. TOS_versionedPoints.csv
Table of TOS points that have changed locations. 
Point movement occurs when a plot marker is missing and cannot be replaced following the instructions in SOP F of the NEON.DOC.001025:TOS Protocol and Procedure: Plot Establishment or if the plot marker was moved to better align the plot shape.  
Only the older versions of the point locations are in this file.  
The current point location can be found in the ALL_NEON_TOS_Plot_Points shapefile, or searching the geoNEON R package or the locations endpoint of the API.    Point history can be also be viewed in the API by using “history=true”.  For example, https://data.neonscience.org/api/v0/locations/JERC_051.basePlot.vst.23?history=true.  Updates to point locations due to improved post processing techniques are not versioned. 