# metadataFactory.list <- function(metadata){}
# metadataFactory.data.frame <- function(metadata){}
# 
#   
# 
# 
# library(XML)
# df <- data.frame(fullName=c('Account.TestField1__c', 'Account.TestField2__c'), 
#               label=c('Test Field1', 'Test Field2'), 
#               length=c(44,45), 
#               type=c('Text', 'Text'))
# x <- split(df, seq(nrow(df)))
# 
# metadataListToXML <- function(root, sublist, metatype=NULL){
#   for(i in 1:length(sublist)){
#     if (!is.null(metatype)){
#       this <- newXMLNode("Metadata", attrs = c(`xsi:type`=metatype), parent=root, suppressNamespaceWarning=T)
#     } else {
#       this <- newXMLNode(names(sublist)[i], parent=root)
#     }
#     if (typeof(sublist[[i]]) == "list"){
#       listToXML(this, sublist[[i]], metatype=NULL)
#     }
#     else{
#       xmlValue(this) <- sublist[[i]]
#     }
#   }
#   return(root)
# }
# 
# metadataListToXML(root=newXMLNode("createMetadata", 
#                           namespaceDefinitions=c('http://soap.sforce.com/2006/04/metadata')),
#           sublist=x, metatype='CustomField')
# 
# 
# 
# ##' Convert List to XML
# ##'
# ##' Can convert list or other object to an xml object using xmlNode
# ##' @title List to XML
# ##' @param item 
# ##' @param tag xml tag
# ##' @return xmlNode
# ##' @export
# ##' @author David LeBauer, Carl Davidson, Rob Kooper
# 
# 
# 
#   
#   # create the node
#   if (identical(names(item), c("text", ".attrs"))) {
#     # special case a node with text and attributes
#     xml <- xmlNode(tag, item[['text']])
#   } else {
#     # node with child nodes
#     xml <- xmlNode(tag)
#     for(i in 1:length(item)) {
#       if (names(item)[i] != ".attrs") {
#         xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
#       }
#     }    
#   }
#   
#   # add attributes to node
#   attrs <- item[['.attrs']]
#   for (name in names(attrs)) {
#     xmlAttrs(xml)[[name]] <- attrs[[name]]
#   }
#   return(xml)
# }  
#   
