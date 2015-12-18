# #' Delete Object or Field Metadata in Salesforce
# #' 
# #' This function takes a list of Metadata component names and deletes them 
# #' to Salesforce. Only one specific type of metadata can be deleted each function call.
# #'
# #' @usage rforcecom.dataMetadata(session, metadata_type=c('CustomObject', 'CustomField'), full_names)
# #' @concept delete metadata salesforce api
# #' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
# #' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
# #' @param metadata_type a character string on what type of metadata to create
# #' @param full_names a character vector of strings that must match one or more full names
# #' returned by the \link{rforcecom.listMetadata} call
# #' @return A \code{list} containing a response for each submitted element of the \code{full_names} argument
# #' that contains elements errors, fullName and success of the delete call.
# #' @seealso \link{rforcecom.listMetadata}
# #' @examples
# #' \dontrun{
# #' 
# #' my_metadata <- rforcecom.listMetadata()
# #' metadata_info <- rforcecom.deleteMetadata(session, 
# #'                                           metadata_type='CustomObject', 
# #'                                           full_names=my_names)
# #' }
# #' @export
# rforcecom.deleteMetadata <- 
#   function(session, metadata_type=c('CustomObject', 'CustomField'), full_names){
#     
#     stopifnot(length(full_names) > 0)
#       
#     # batch the data if necessary (only 10 components can be sent at a time)
#     batches_quotient <- seq.int(nrow(data)) %/% batchSize
#     batches_remainder <- seq.int(nrow(data)) %% batchSize
#     split_ind <- batches_quotient + 1
#     split_ind[batches_remainder == 0] <- split_ind[batches_remainder == 0] - 1
#     
#     temp_file_list <- lapply(seq.int(max(split_ind)), FUN=function(x){
#       f <- tempfile()
#       rforcecom.write.csv(x=data[split_ind == x, , drop=FALSE], file=f)
#       f
#     })
#     
#     # request parameters
#     endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
#     URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch', sep="")
#     OAuthString <- unname(session['sessionID'])
#     
#     batch_info <- lapply(temp_file_list, FUN=function(x){
#       
#       # cleanup the temp file
#       on.exit(expr={unlink(x, force=TRUE)})
#       
#       #make request
#       res <- httr::POST(URL, config = httr::add_headers('X-SFDC-Session'=OAuthString,
#                                                         'Accept'="application/xml", 
#                                                         'Content-Type'="text/csv; charset=UTF-8"),
#                         body = httr::upload_file(path=x, type='text/csv'))
#       closeAllConnections()
#       # Parse XML 
#       x.root <- xmlRoot(content(res, as='parsed'))
#       
#       # BEGIN DEBUG
#       if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
#       if(exists("rforcecom.debug") && rforcecom.debug){ message(x.root) }
#       # END DEBUG
#       
#       return(xmlToList(x.root))
#     })
#     
#     return(batch_info)
#   }
