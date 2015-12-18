# #' Create Object or Field Metadata in Salesforce
# #' 
# #' This function takes a list of Metadata components and sends them 
# #' to Salesforce
# #'
# #' @usage rforcecom.createMetadata(session, metadata_type=c('CustomObject', 'CustomField'), metadata)
# #' @concept create metadata salesforce api
# #' @include rforcecom.metadataFactory.R
# #' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/}
# #' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
# #' @param metadata_type a character string on what type of metadata to create
# #' @param metadata a list of metadata components to be created formatted as XML before being sent via API
# #' @param verbose a boolean indicating whether to print messages during metadata creation
# #' @return A \code{list} containing a response for each submitted metadata component
# #' @examples
# #' \dontrun{
# #' 
# #' my_metadata <- data.frame(Name=paste('New Record:', 1:n), stringsAsFactors=FALSE)
# #' metadata_info <- rforcecom.createMetadata(session, 
# #'                                           metadata_type='CustomObject', 
# #'                                           metadata=my_metadata)
# #' }
# #' @export
# rforcecom.createMetadata <- 
#   function(session, metadata_type=c('CustomObject', 'CustomField'), metadata){
#     
#     stopifnot(length(metadata) > 0)
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
#     # Retrieve createMetadataResponse result
#     x.root <- xmlInternalTreeParse(t$value(), asText=T)
#     result_body <- xmlToList(x.root)[['Body']][['createMetadataResponse']]
#     
#     return(result_body)
#   }
