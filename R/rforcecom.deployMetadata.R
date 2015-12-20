#' Deploy Metadata to an Environment
#' 
#' This function deploys metadata 
#' as a package XML files to a target environment.
#' 
#' @usage rforcecom.deployMetadata(session, zipFile, deployOptions=NULL)
#' @concept deploy metadata salesforce api
#' @importFrom plyr llply ldply
#' @importFrom XML newXMLNode 
#' @importFrom RCurl base64Encode
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_deploy.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param deployOptions a \code{list} of parameters defining the deployment
#' @param zipFile a file path to a .zip containing files for deployment
#' @return A \code{list} of details from the created retrieve request
#' @note See the Salesforce documentation for the accepted deployOptions parameters.
#' Here is a link to that documentation: 
#' \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_deploy.htm#deploy_options}
#' @examples
#' \dontrun{
#' 
#' deployOptions <- list(allowMissingFiles='true', performRetrieve='false')
#' 
#' deploy_info <- rforcecom.deployMetadata(session, deployOptions, 'package.zip')
#' 
#' }
#' @export
rforcecom.deployMetadata <- function(session, zipFile, deployOptions=NULL) {
  
  stopifnot(grepl("\\.zip$", zipFile))
  
  # create XML for deploy node
  root <- newXMLNode("deploy", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  # add the encoded zip file
  packageZip <- readBin(zipFile, "raw", file.info(zipFile)[1, "size"])
  encoded_zip <- base64Encode(packageZip, "character")
  addChildren(root, newXMLNode('ZipFile', encoded_zip))
  # add the deploy options
  if (!is.null(deployOptions)){
    deploy_node <- newXMLNode("deployOptions", attrs = c(`xsi:type`='DeployOptions'), parent=root, suppressNamespaceWarning=T)
    metadataListToXML(root=deploy_node, sublist=deployOptions, metatype=NULL)
  }
  
  URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
  
  x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                 URL, root, SOAPAction='deploy')
  
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['deployResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
    stop(paste(errorcode, errormessage, sep=": "))
  }
  
  summary <- xmlToList(response$result)
  
  return(summary)
}


#' Check on Deploy Calls and Get Contents If Available
#' 
#' This function returns details about an initiated deployMetadata requset
#' and saves the results into a zip file
#' 
#' @usage rforcecom.checkDeployStatusMetadata(session, 
#'                                            id,
#'                                            includeDetails=c('false', 'true'),
#'                                            filename='deployed_package.zip', 
#'                                            verbose=FALSE)
#' @concept deploy metadata salesforce api
#' @importFrom plyr ldply
#' @importFrom RCurl base64Decode
#' @importFrom XML removeChildren 
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_checkdeploystatus.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param id a character string id returned from \link{rforcecom.retrieveMetadata}
#' @param includeDetails a boolean. Set to false to check the status of the deploy without 
#' attempting to deploy details portion of the response. See the url for what is 
#' contained in the DeployDetails portion:
#' \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_deployresult.htm#deploy_details_title}
#' @param filename a file path to save the zip file in the event that it is downloaded. The 
#' name must have a .zip extension. The default behavior will be to save in the current 
#' working directory as package.zip
#' @param verbose a boolean indicating whether to print messages during metadata creation
#' @return A \code{list} of the response
#' @examples
#' \dontrun{
#' 
#' deployOptions <- list(allowMissingFiles='true', performRetrieve='false')
#' deploy_info <- rforcecom.deployMetadata(session, 'package.zip', deployOptions)
#' 
#' # check on deploy status
#' deploy_status <- rforcecom.checkDeployStatusMetadata(session, deploy_info$id)
#' 
#' }
#' @export
rforcecom.checkDeployStatusMetadata <- function(session, 
                                                id, 
                                                includeDetails=c('false','true'), 
                                                filename='deployed_package.zip', 
                                                verbose=FALSE){
  
  stopifnot(is.character(id), nchar(id)=='18')
  stopifnot(grepl('\\.zip$', filename))
  
  # default to false if not provided
  includeDetails <- match.arg(includeDetails)
  
  # create XML for retrieveMetadata node
  root <- newXMLNode("checkDeployStatus", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  addChildren(root, newXMLNode('id', id))
  addChildren(root, newXMLNode('includeDetails', includeDetails))
  
  URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
  
  if(verbose) {
    print(URL)
    print(root)
  }
  
  x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                 URL, root, SOAPAction='checkDeployStatus')
  
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['checkDeployStatusResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
    stop(paste(errorcode, errormessage, sep=": "))
  }
  
  componentFailures_df <- ldply(response$result[['details']][grepl('componentFailures', names(response$result[['details']]))],
                             .fun=function(x){
                               x <- xmlToList(x)
                               x[sapply(x, is.null)] <- NA
                               x <- as.data.frame(x, stringsAsFactors=F)
                               return(x)
                             }, .id=NULL)
  componentSuccesses_df <- ldply(response$result[['details']][grepl('componentSuccesses', names(response$result[['details']]))],
                                .fun=function(x){
                                  x <- xmlToList(x)
                                  x[sapply(x, is.null)] <- NA
                                  x <- as.data.frame(x, stringsAsFactors=F)
                                  return(x)
                                }, .id=NULL)
  
  if(xmlValue(response$result[['done']])=='true' & 
     !is.na(xmlValue(response$result[['details']][['retrieveResult']][['zipFile']]))){
    # save the zip file
    decoded_dat <- base64Decode(xmlValue(response$result[['details']][['retrieveResult']][['zipFile']]), "raw")
    filename <- 'deployed_package.zip'
    writeBin(decoded_dat, filename)
    message(paste0('Deployed Package Manifest Files Saved at: ', filename))
  }
  
  # set those to null
  try(removeChildren(response$result[['details']], 'componentFailures'), silent=T)
  try(removeChildren(response$result[['details']], 'componentSuccesses'), silent=T)
  try(removeChildren(response$result[['details']][['retrieveResult']], 'zipFile'), silent=T)
  
  summary <- xmlToList(response$result)
  summary$componentFailures<- componentFailures_df
  summary$componentSuccesses<- componentSuccesses_df
  
  return(summary)
}

#' Cancel a Prior Call to Deploy Metadata
#' 
#' This function cancels a deployment of metadata 
#' as a package XML files to a target environment.
#' 
#' @usage rforcecom.cancelDeployMetadata(session, id, verbose=FALSE)
#' @concept cancel deploy metadata salesforce api
#' @importFrom plyr ldply
#' @importFrom XML newXMLNode addChildren
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_canceldeploy.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param id a character string id returned from \link{rforcecom.deployMetadata}
#' @param verbose a boolean indicating whether to print the XML request
#' @examples
#' \dontrun{
#' 
#' deployOptions <- list(allowMissingFiles='true', performRetrieve='false')
#' deploy_info <- rforcecom.deployMetadata(session, 'package.zip', deployOptions)
#' 
#' # cancel deployment
#' deploy_cancel_info <- rforcecom.cancelDeployMetadata(session, deploy_info$id)
#' 
#' }
#' @export
rforcecom.cancelDeployMetadata <- function(session, id, verbose=FALSE) {
  
  stopifnot(is.character(id), nchar(id)=='18')
  
  # create XML for deploy node
  root <- newXMLNode("cancelDeploy", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  addChildren(root, newXMLNode('id', id))
  
  URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
  
  if(verbose) {
    print(URL)
    print(root)
  }
  
  x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                 URL, root, SOAPAction='cancelDeploy')
  
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['cancelDeployResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
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


#' ReDeploy a Prior Call to Deploy Metadata
#' 
#' This function redeploys a deployment of metadata 
#' to production in less time by skipping the execution of Apex tests.
#' 
#' @usage rforcecom.deployRecentValidation(session, validationID)
#' @concept deploy recent metadata salesforce api
#' @importFrom plyr ldply
#' @importFrom XML newXMLNode xmlInternalTreeParse xmlChildren
#' @include rforcecom.utils.R
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_deployRecentValidation.htm}
#' @param session a named character vector defining parameters of the api connection as 
#' returned by \link{rforcecom.login}
#' @param validationID a character string id returned from \link{rforcecom.deployMetadata}
#' @note Before you call this function your organization must have a validation that was 
#' recently run. You can run a validation on a set of components by calling \link{rforcecom.deployMetadata} 
#' with the checkOnly property of the deployOptions parameter set to true. 
#' @examples
#' \dontrun{
#' 
#' # Before you call rforcecom.deployRecentValidation(), 
#' # your organization must have a validation that was 
#' # recently run. You can run a validation on a set of 
#' # components by calling rforcecom.deployMetadata() 
#' # with the checkOnly property of the deployOptions 
#' # parameter set to true. 
#' 
#' deployOptions <- list(allowMissingFiles='true', 
#'                       checkOnly='true', 
#'                       performRetrieve='false')
#' deploy_info <- rforcecom.deployMetadata(session, 'package.zip', deployOptions)
#' 
#' # redeploy deployment
#' redeploy_info <- rforcecom.deployRecentValidation(session, deploy_info$id)
#' 
#' }
#' @export
rforcecom.deployRecentValidation <- function(session, validationID) {
  
  stopifnot(is.character(validationID), nchar(validationID)=='18')
  
  if(as.numeric(session['apiVersion']) < 33) stop("Only available in API version 33.0 and later")
  
  # create XML for deploy node
  root <- newXMLNode("deployRecentValidation", 
                     namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata'))
  addChildren(root, newXMLNode('validationID', validationID))
  
  URL <- paste0(session['instanceURL'], rforcecom.api.getMetadataEndpoint(session['apiVersion']))
  
  x.root <- metadata_curl_runner(unname(session['sessionID']), 
                                 URL, root, SOAPAction='deployRecentValidation')
  
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
  response <- xmlChildren(xmlChildren(xmlChildren(xmlRoot(x.root))$Body)[['deployRecentValidationResponse']])
  try(errorcode <- iconv(xmlValue(response$result[['errors']][['statusCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(response$result[['errors']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
    stop(paste(errorcode, errormessage, sep=": "))
  }
  
  summary <- xmlToList(response$result)
  
  return(summary)
}
