#' Update Object or Field Metadata in Salesforce
#' 
#' This function takes a list of Metadata components and sends them 
#' to Salesforce to update an object that already exists
#'
#' @usage rforcecom.updateMetadata(session, 
#'                                 metadata_type=c('CustomObject', 'CustomField'), 
#'                                 metadata, verbose=FALSE)
#' @concept update metadata salesforce api
#' @importFrom plyr ldply
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param metadata_type a character string on what type of metadata to create
#' @param metadata a list of metadata components to be created formatted as XML before being sent via API
#' @param verbose a boolean indicating whether to print messages during metadata creation
#' @return A \code{data.frame} containing the creation result for each submitted metadata component
#' @note The update key is based on the fullName parameter of the metadata, so updates are triggered
#' when an existing Salesforce element matches the metadata type and fullName.
#' @examples
#' \dontrun{
#' 
#' # read the metadata of the existing Account object
#' # we will use this object as a template to create a custom version
#' metadata_info <- rforcecom.readMetadata(session, 
#'                                         metadata_type='CustomObject', 
#'                                         object_names=c('Account'))
#' 
#' custom_metadata <- metadata_info$records
#' 
#' # make some adjustments to customize the object
#' custom_metadata$fullName <- 'Custom_Account23__c'
#' # specify a plural label
#' custom_metadata$pluralLabel <- 'Custom_Account23s'
#' # specify a name field
#' custom_metadata$nameField <- list(displayFormat='AN-{0000}', 
#'                                   label='Account Number', 
#'                                   type='AutoNumber')
#' # remove default actionOverrides, this cannot be set during creation
#' custom_metadata[which(names(custom_metadata) %in% c("actionOverrides"))] <- NULL
#' 
#' # set the deployment status, this must be set before creation
#' custom_metadata$deploymentStatus <- 'Deployed' 
#' 
#' # make a description to identify this easily in the UI setup tab
#' custom_metadata$description <- 'created by the Metadata API'
#' 
#' new_custom_object <- rforcecom.createMetadata(session, 
#'                                               metadata_type='CustomObject', 
#'                                               metadata=custom_metadata)
#' 
#' # specify a plural label
#' update_metadata <- list(list(fullName='Custom_Account23__c', 
#'                              label='New Label Custom_Account23',
#'                              pluralLabel='Custom_Account23s', 
#'                              nameField=list(displayFormat='AN-{0000}',
#'                                             label='Account Number',
#'                                             type='AutoNumber'), 
#'                              deploymentStatus='Deployed', 
#'                              sharingModel='ReadWrite'))
#' updated_custom_object <- rforcecom.updateMetadata(session, 
#'                                                    metadata_type='CustomObject', 
#'                                                    metadata=upsert_metadata)
#' 
#' }
#' @export
rforcecom.updateMetadata <- 
  function(session, 
           metadata_type=c('CustomObject', 'CustomField'), 
           metadata, verbose=FALSE){
    
    stopifnot(length(metadata) > 0)
    stopifnot(is.list(metadata) | is.data.frame(metadata))
    
    # convert data.frame inputs to list
    if(is.data.frame(metadata)){
      metadata <- split(metadata, seq(nrow(metadata)))
    }
    
    #construct XML
    root <- newXMLNode("updateMetadata", 
                       namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
    if(typeof(metadata[[1]]) != "list"){
      metadata <- list(metadata)
    }
    metadataListToXML(root=root, sublist=metadata, metatype=metadata_type)
    
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
    httpHeader <- c("SOAPAction"="updateMetadata", 'Content-Type'="text/xml")
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
    response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['updateMetadataResponse']])
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
