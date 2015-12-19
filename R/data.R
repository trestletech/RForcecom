#' Metadata Type Inputs Table
#'
#' This data.frame contains a list of different data types that 
#' can be submitted via the Metadata API and subelements to create
#' or update metadata.
#' 
#' \itemize{
#'   \item data_type: the name of the type, not necessarily metadata, but can be submitted
#'   as part of a metadata request to create or update.
#'   \item element: the name of the element defined as a valid input to the data_type
#' }
#'
#' @docType data
#' @keywords datasets
#' @name metadata_inputs
#' @usage metadata_inputs
#' @format a \code{data.frame} with 2,781 rows and 2 variables
#' @examples
#' \dontrun{
#' data(metadata_inputs)
#' 
#' # get a list of the inputs to create "CustomTab" metadata
#' metadata_inputs[metadata_inputs$data_type=='CustomTab',]
#'  data_type          input
#' CustomTab       fullName
#' CustomTab  auraComponent
#' CustomTab   customObject
#' CustomTab    description
#' CustomTab      flexiPage
#' CustomTab    frameHeight
#' CustomTab     hasSidebar
#' CustomTab           icon
#' CustomTab          label
#' CustomTab    mobileReady
#' CustomTab          motif
#' CustomTab           page
#' CustomTab       scontrol
#' CustomTab splashPageLink
#' CustomTab            url
#' CustomTab urlEncodingKey
#' 
#' }
NULL