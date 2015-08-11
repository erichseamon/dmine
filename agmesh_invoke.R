#------------------------------------------------------------------------#
# TITLE:        agmesh_invoke.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Raster Generation - Stage 1
#
# COMMENTS:     This is the agmesh invocation script.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. The 
#               agmesh invoke script invokes all the stages of the agmesh
#               framework in a sequential order. (Stage 1 - Stage 4)
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#


rm(list=ls())   # Clear workspace (remove variables)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt


#----------funs cross correlations function for a dataset----------#
#-------------- Set script location -------------------#
options(warn=-1)
source ("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_subsetaggregate.R")
source ("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_setup.R")
source ("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_analysis_ET.R")
source ("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_calculation_ET.R")
source ("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_plotting_ET.R")
