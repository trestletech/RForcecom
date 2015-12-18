#' Delete Object or Field Metadata in Salesforce
#' 
#' This function takes a a request of named elements in Salesforce and 
#' deletes them
#'
#' @usage rforcecom.deleteMetadata(session, 
#'                               metadata_type=c('CustomObject', 'CustomField'), 
#'                               object_names, 
#'                               verbose=FALSE)
#' @concept delete metadata salesforce api
#' @importFrom plyr ldply
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param metadata_type a character string on what type of metadata that is being read
#' @param object_names a character vector of names that we wish to read metadata for
#' @param verbose a boolean indicating whether to print messages during metadata creation
#' @return A \code{data.frame} containing the creation result for each submitted metadata component
#' @seealso \link{rforcecom.listMetadata}
#' @examples
#' \dontrun{
#' 
#' metadata_info <- rforcecom.deleteMetadata(session, 
#'                                           metadata_type='CustomObject', 
#'                                           object_names=c('Custom_Account23__c'))
#' }
#' @export
rforcecom.deleteMetadata <- 
  function(session, 
           metadata_type=c('CustomObject', 'CustomField'), 
           object_names, verbose=FALSE){
    
    stopifnot(all(is.character(object_names)))
    
    # format names into list
    object_list <- as.list(object_names)
    names(object_list) <- rep('fullNames', length(object_list))
    
    # create XML for readMetadata node
    root <- newXMLNode("deleteMetadata", 
                       namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
    # add metadata type
    root2 <- newXMLNode("metadataType", metadata_type, parent=root)
    # create full xml onto the root
    metadataListToXML(root=root, sublist=object_list, metatype=NULL)
    
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
    httpHeader <- c("SOAPAction"="deleteMetadata", 'Content-Type'="text/xml")
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
    response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['deleteMetadataResponse']])
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
