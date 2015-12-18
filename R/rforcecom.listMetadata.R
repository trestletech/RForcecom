#' List All Objects of a Certain Metadata Type in Salesforce
#' 
#' This function takes a query of metadata types and returns a 
#' summary of all objects in salesforce of the requested types
#'
#' @usage rforcecom.listMetadata(session, 
#'                               queries, 
#'                               asOfVersion=NULL, 
#'                               verbose=FALSE)
#' @concept list metadata salesforce api
#' @importFrom plyr ldply
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param queries a \code{list} of \code{list}s with each element consisting of 2 components: 1) 
#' the metadata type being requested and 2) the folder associated with the type that required for types 
#' that use folders, such as Dashboard, Document, EmailTemplate, or Report.
#' @param asOfVersion a numeric specifying the API version for the metadata 
#' listing request. If you don't specify a value in this field, it defaults to the API 
#' version specified when you logged in. This field allows you to override the default 
#' and set another API version so that, for example, you could list the metadata for a 
#' metadata type that was added in a later version than the API version specified when you logged in. 
#' @param verbose a boolean indicating whether to print messages during metadata creation
#' @return A \code{data.frame} containing the queried metadata types
#' @note Only 3 queries can be specifed at one time, so the list length must not exceed 3.
#' @examples
#' \dontrun{
#' 
#' # pull back a list of all Custom Objects and Email Templates
# myqueries <- list(list(type='CustomObject'), 
#                   list(type='EmailTemplate', 
#                        folder='unfiled$public'))
# metadata_info <- rforcecom.listMetadata(session, 
#                                         queries=myqueries)
#' }
#' @export
rforcecom.listMetadata <- 
  function(session, 
           queries, 
           asOfVersion=NULL, verbose=FALSE){
    
    # create XML for readMetadata node
    root <- newXMLNode("listMetadata", 
                       namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
    if(typeof(queries[[1]]) != "list"){
      queries <- list(queries)
    }
    metadataListToXML(root=root, sublist=queries, metatype='ListMetadataQuery')
    if(!is.null(asOfVersion)){
      stopifnot(is.numeric(asOfVersion))
      addChildren(root, newXMLNode('asOfVersion', asOfVersion))
    }
    
    #build soapBody
    soapBody <- paste0('<?xml version="1.0" encoding="UTF-8"?>
                       <env:Envelope xmlns:env="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                       <env:Header>
                       <SessionHeader xmlns="http://soap.sforce.com/2006/04/metadata">
                       <sessionId>', session['sessionID'], '</sessionId>
                       </SessionHeader>
                       </env:Header>
                       <env:Body>',
                       as(root, 'character'),
                       '</env:Body>
                       </env:Envelope>')
    
    # perform request
    # HTTP POST
    h <- basicHeaderGatherer()
    t <- basicTextGatherer()
    httpHeader <- c("SOAPAction"="listMetadata", 'Content-Type'="text/xml")
    curlPerform(url=paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion'])), 
                postfields=soapBody, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
    
    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(x.root) }
    # END DEBUG
    
    x.root <- xmlRoot(xmlInternalTreeParse(t$value(), asText=T))
    
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
    response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['listMetadataResponse']])
    try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
    try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
    if(!is.na(errorcode) && !is.na(errormessage)){
      stop(paste(errorcode, errormessage, sep=": "))
    }
    
    result_body <- ldply(response[grepl('result', names(response))],
                         .fun=function(x){
                           x <- xmlToList(x)
                           x[sapply(x, is.null)] <- NA
                           x <- as.data.frame(x, stringsAsFactors=F)
                           return(x)
                         }, .id=NULL)
    
    return(result_body)
}
