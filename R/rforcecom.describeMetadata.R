#' Describe the Metadata in an Organization
#' 
#' This function returns details about the organization metadata
#' 
#' @usage rforcecom.describeMetadata(session, verbose=FALSE)
#' @concept describe metadata salesforce api
#' @importFrom plyr llply ldply
#' @importFrom XML newXMLNode xmlInternalTreeParse xmlChildren
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param verbose a boolean indicating whether to print the XML request
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' 
#' # describe metadata for the organization associated with the session
#' metadata_info <- rforcecom.describeMetadata(session)
#' 
#' }
#' @export
rforcecom.describeMetadata <- function(session, verbose=FALSE){
    
    # create XML for describeMetadata node
    root <- newXMLNode("describeMetadata", 
                       namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
    addChildren(root, newXMLNode('apiVersion', session['apiVersion']))
    
    URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
    
    if(verbose) {
      print(URL)
      print(root)
    }
    
    x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                   URL, root, SOAPAction='describeMetadata')
    
    # Check whether it success or not
    errorcode <- NA
    errormessage <- NA
    
    # check for api fault
    response <- xmlChildren(xmlChildren(xmlRoot(x.root))$Body)
    try(errorcode <- iconv(xmlValue(response$Fault[['faultcode']]), from="UTF-8", to=""), TRUE)
    try(errormessage <- iconv(xmlValue(response$Fault[['faultstring']]), from="UTF-8", to=""), TRUE)
    if(!is.na(errorcode) && !is.na(errormessage)){
      stop(paste(errorcode, errormessage, sep=": "))
    }
    
    # check for request fault
    response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['describeMetadataResponse']])
    try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
    try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
    if(!is.na(errorcode) && !is.na(errormessage)){
      stop(paste(errorcode, errormessage, sep=": "))
    }
    
    metadataobject_df <- ldply(response$result[grepl('metadataObjects', names(response$result))],
                               .fun=function(x){
                                 x <- xmlToList(x)
                                 x[sapply(x, is.null)] <- NA
                                 x <- as.data.frame(x, stringsAsFactors=F)
                                 return(x)
                                }, .id=NULL)
    
    summary <- llply(response$result[!grepl('metadataObjects', names(response$result))],
                       .fun=function(x){
                         x <- xmlToList(x)
                         return(x)
                       })
    summary$metadataObjects <- metadataobject_df
    
    return(summary)
}
