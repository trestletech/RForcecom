#' Make A Request to Retrieve the Metadata
#' 
#' This function makes a request to retrieve metadata 
#' as a package XML files that can be modified and later
#' deployed into an environment 
#' 
#' @usage rforcecom.retrieveMetadata(session, retrieveRequest, verbose=FALSE)
#' @concept retrieve metadata salesforce api
#' @importFrom plyr llply ldply
#' @importFrom XML newXMLNode
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_retrieve.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param retrieveRequest a \code{list} of parameters defining what XML file representations
#' should be returned
#' @param verbose a boolean indicating whether to print the XML request used
#' @return A \code{list} of details from the created retrieve request
#' @note See the Salesforce documentation for the proper arguments to create a 
#' retrieveRequest. Here is a link to that documentation: 
#' \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_retrieve_request.htm}
#' @examples
#' \dontrun{
#' 
#' retrieveRequest <- list(unpackaged=list(types=list(members='*', name='CustomObject')))
#' 
#' retrieve_info <- rforcecom.retrieveMetadata(session, retrieveRequest)
#' 
#' }
#' @export
rforcecom.retrieveMetadata <- function(session, retrieveRequest, verbose=FALSE){
  
  # create XML for retrieve node
  root <- newXMLNode("retrieve", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  request_root <- newXMLNode('retrieveRequest', attrs = c(`xsi:type`='RetrieveRequest'), suppressNamespaceWarning=T)
  metadataListToXML(root=request_root, sublist=retrieveRequest, metatype=NULL)
  addChildren(root, request_root)
  
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
  httpHeader <- c("SOAPAction"="retrieve", 'Content-Type'="text/xml")
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['retrieveResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
    stop(paste(errorcode, errormessage, sep=": "))
  }
  
  summary <- xmlToList(response$result)

  return(summary)
}


#' Check on Retrieve Calls and Get Contents If Available
#' 
#' This function returns details about an initiated retrieveMetadata requset
#' and saves the results into a zip file
#' 
#' @usage rforcecom.checkRetrieveStatusMetadata(session, id, 
#'                                              includeZip=c('true', 'false'), 
#'                                              filename='package.zip')
#' @concept retrieve metadata salesforce api
#' @importFrom plyr llply ldply
#' @importFrom RCurl base64Decode
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_checkretrievestatus.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param id a character string id returned from \link{rforcecom.retrieveMetadata}
#' @param includeZip a boolean Set to false to check the status of the retrieval without 
#' attempting to retrieve the zip file. If omitted, this argument defaults to true, 
#' which means that the zip file is retrieved on the last call to checkRetrieveStatus() 
#' when the retrieval has finished.
#' @param filename a file path to save the zip file in the event that it is downloaded. The 
#' name must have a .zip extension. The default behavior will be to save in the current 
#' working directory as package.zip
#' @return A \code{list} of the response
#' @examples
#' \dontrun{
#' 
#' retrieveRequest <- list(unpackaged=list(types=list(members='*', name='CustomObject')))
#' retrieve_info <- rforcecom.retrieveMetadata(session, retrieveRequest)
#' 
#' # check on status, this will automatically download the contents to package.zip if they are ready
#' retrieve_status <- rforcecom.checkRetrieveStatusMetadata(session, retrieve_info$id)
#' 
#' }
#' @export
rforcecom.checkRetrieveStatusMetadata <- function(session, 
                                                  id, 
                                                  includeZip=c('true','false'), 
                                                  filename='package.zip'){
  
  stopifnot(grepl('\\.zip$', filename))
  
  # default to true if not provided since that is salesforce default
  includeZip <- match.arg(includeZip)
  
  # create XML for checkRetrieveStatus node
  root <- newXMLNode("checkRetrieveStatus", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  addChildren(root, newXMLNode('asyncProcessId', id))
  addChildren(root, newXMLNode('includeZip', includeZip))
  
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
  httpHeader <- c("SOAPAction"="checkRetrieveStatus", 'Content-Type'="text/xml")
  curlPerform(url=paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(apiVersion)), 
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['checkRetrieveStatusResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
    stop(paste(errorcode, errormessage, sep=": "))
  }
  
  fileProperties_df <- ldply(response$result[grepl('fileProperties', names(response$result))],
                             .fun=function(x){
                               x <- xmlToList(x)
                               x[sapply(x, is.null)] <- NA
                               x <- as.data.frame(x, stringsAsFactors=F)
                               return(x)
                             }, .id=NULL)
  
  summary <- llply(response$result[!grepl('fileProperties', names(response$result))],
                   .fun=function(x){
                     x <- xmlToList(x)
                     return(x)
                   })
  summary <- summary[!grepl('zipFile', names(summary))]
  summary$fileProperties<- fileProperties_df
  
  if(summary$done=='true' & 
     summary$status=='Succeeded' & 
     summary$success=='true' & 
     xmlValue(response$result[['zipFile']])!='' & 
     includeZip == 'true'){
    # save the zip file
    decoded_dat <- base64Decode(xmlValue(response$result[['zipFile']]), "raw")
    writeBin(decoded_dat, filename)
    message(paste0('Package Manifest Files Saved at: ', filename))
  }

  return(summary)
}
