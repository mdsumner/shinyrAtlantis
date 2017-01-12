#' @title A package for visualising Atlantis input data
#'
#' @description
#' A set of shiny applications for visualising and generating Atlantis parameterisation files.
#' 
#' There are two types of functions: (1) make functions that collect relevant data from 
#' Atlantis parameter files and convert them into list objects, and (2) shiny
#' applications that take the list objects and allow the user to view the data
#' in terms of tables and maps. 
#' 
#' A two step approach is adopted as the list objects can take a few minutes
#' to generate and may be stored for later repeated use.
#'
#' @details
#' There are four shiny applications.
#' 
#' \code{\link{sh.prm}}: Displays data stored in an Atlantis parameter file.
#' 
#' \code{\link{sh.init}}: Displays data stored in an Atlantis initial conditions file.
#' 
#' \code{\link{sh.forcings}}: Displays flow exhanges, salinity, and termperature data used to force an Atlantis run.
#' 
#' @importFrom shiny a  
#'  \code{\link{sh.dist}}: Generates horizontal probability distributions that can be cut-and-pasted into an Atlantis parameter file.
#' @name shinyrAtlantis
#' @docType package
NULL
