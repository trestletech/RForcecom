#' Rename Metadata Elements in Salesforce
#' 
#' This function takes an old and new name for a 
#' metadata element in Salesforce and applies the new name
#'
#' @usage rforcecom.renameMetadata(session, 
#'                                 metadata_type, 
#'                                 oldFullname, newFullname, verbose=FALSE)
#' @concept rename metadata salesforce api
#' @importFrom plyr ldply
#' @importFrom XML newXMLNode xmlInternalTreeParse xmlChildren
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param metadata_type a character string on what type of metadata to create
#' @param oldFullname a character string corresponding to the fullName of the element you would 
#' like to rename
#' @param newFullname a character string corresponding to the new fullName you would like 
#' to apply the targeted element
#' @param verbose a boolean indicating whether to print the XML request
#' @return A \code{data.frame} containing the creation result for each submitted metadata component
#' @examples
#' \dontrun{
#' 
#' renamed_custom_object <- rforcecom.renameMetadata(session, 
#'                                                   metadata_type='CustomObject', 
#'                                                   oldFullname='Custom_Account23__c', 
#'                                                   newFullname='Custom_Account24__c', verbose=FALSE)
#' 
#' }
#' @export
rforcecom.renameMetadata <- 
  function(session, 
           metadata_type, 
           oldFullname, newFullname, verbose=FALSE){
    
    #construct XML
    root <- newXMLNode("renameMetadata", 
                       namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
    addChildren(root, newXMLNode('type', metadata_type))
    addChildren(root, newXMLNode('oldFullname', oldFullname))
    addChildren(root, newXMLNode('newFullname', newFullname))
    
    URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
    
    if(verbose) {
      print(URL)
      print(root)
    }
    
    x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                   URL, root, SOAPAction='renameMetadata')
    
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
    response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['renameMetadataResponse']])
    try(errorcode <- iconv(xmlValue(response$errors[['statusCode']]), from="UTF-8", to=""), TRUE)
    try(errormessage <- iconv(xmlValue(response$errors[['message']]), from="UTF-8", to=""), TRUE)
    if(!is.na(errorcode) && !is.na(errormessage)){
      stop(paste(errorcode, errormessage, sep=": "))
    }
    
    result_body <- ldply(response[grepl('result', names(response))],
                         .fun=function(x){
                           x <- as.data.frame(xmlToList(x), stringsAsFactors=F)
                           return(x)
                         }, .id=NULL)
    
    return(result_body)
  }
