
context("Testing Metadata API CRUD Operations")

# This testing suite will perform a set of 6 operations that
# proceed to build up and teardown a set of metadata via the API
# Those 6 operations are:
#  1. rforcecom.readMetadata
#  2. rforcecom.createMetadata
#  3. rforcecom.updateMetadata
#  4. rforcecom.renameMetadata
#  5. rforcecom.upsertMetadata
#  6. rforcecom.deleteMetadata

username <- Sys.getenv("RFORCECOM_EMAIL")
password <- Sys.getenv("RFORCECOM_PASSWORD")
instanceURL <- "https://na34.salesforce.com/"
apiVersion <- "35.0"
session <- rforcecom.login(username, password, instanceURL, apiVersion)

# read the metadata of the existing Account object
# we will use this object as a template to create a custom version
read_custom_object_result <- rforcecom.readMetadata(session, 
                                        metadata_type='CustomObject', 
                                        object_names=c('Account'))

custom_object <- read_custom_object_result$records

# make some adjustments to customize the object

custom_object$fullName <- 'Custom_Account1__c'
# specify a plural label
custom_object$pluralLabel <- 'Custom_Account1s'
# specify a name field
custom_object$nameField <- list(displayFormat='AN-{0000}', 
                                label='Account Number', 
                                type='AutoNumber')
# remove default actionOverrides, this cannot be set during creation
custom_object[which(names(custom_object) %in% c("actionOverrides"))] <- NULL
# set the deployment status, this must be set before creation
custom_object$deploymentStatus <- 'Deployed' 
# make a description to identify this easily in the UI setup tab
custom_object$description <- 'created by the Metadata API'

# send the request
custom_object_result <- rforcecom.createMetadata(session, 
                                                 metadata_type='CustomObject', 
                                                 metadata=custom_object)

# adding custom fields 
# input formatted as a list
custom_fields1 <- list(list(fullName='Custom_Account1__c.CustomField1__c',
                            label='Test Field1',
                            length=100,
                            type='Text'), 
                       list(fullName='Custom_Account1__c.CustomField2__c',
                            label='Test Field2',
                            length=100,
                            type='Text'))
create_fields_result1 <- rforcecom.createMetadata(session, 
                                                  metadata_type='CustomField', 
                                                  metadata=custom_fields1)

# input formatted as a data.frame
custom_fields2 <- data.frame(fullName=c('Custom_Account1__c.CustomField3__c', 
                                        'Custom_Account1__c.CustomField4__c'), 
                             label=c('Test Field3', 'Test Field4'), 
                             length=c(44,45), 
                             type=c('Text', 'Text'))
create_fields_result2 <- rforcecom.createMetadata(session, 
                                                  metadata_type='CustomField', 
                                                  metadata=custom_fields2)


# specify a new plural label
update_metadata <- list(list(fullName='Custom_Account1__c', 
                             label='New Label Custom_Account1',
                             pluralLabel='Custom_Account1s_new', 
                             nameField=list(displayFormat='AN-{0000}',
                                            label='Account Number',
                                            type='AutoNumber'), 
                             deploymentStatus='Deployed', 
                             sharingModel='ReadWrite'))
updated_custom_object_result <- rforcecom.updateMetadata(session, 
                                                  metadata_type='CustomObject', 
                                                  metadata=update_metadata)

renamed_custom_object_result <- rforcecom.renameMetadata(session, 
                                                  metadata_type='CustomObject', 
                                                  oldFullname='Custom_Account1__c', 
                                                  newFullname='Custom_Account2__c')

upsert_metadata <- update_metadata
upserted_custom_object_result <- rforcecom.upsertMetadata(session, 
                                                   metadata_type='CustomObject', 
                                                   metadata=upsert_metadata)

deleted_custom_object_result <- rforcecom.deleteMetadata(session, 
                                          metadata_type='CustomObject', 
                                          object_names=c('Custom_Account1__c', 
                                                         'Custom_Account2__c'))

test_that("rforcecom.readMetadata", {
  expect_is(read_custom_object_result, "list")
  expect_true(all(c('fullName', 'fields', 'searchLayouts', 'sharingModel', 'label') %in% names(read_custom_object_result[[1]])))
})

test_that("rforcecom.createMetadata", {
  expect_is(custom_object_result, "data.frame")
  expect_equal(names(custom_object_result), c('fullName', 'success'))
  expect_equal(nrow(custom_object_result), 1)
  expect_equal(custom_object_result$success, 'true')
  
  expect_is(create_fields_result1, "data.frame")
  expect_equal(names(create_fields_result1), c('fullName', 'success'))
  expect_equal(nrow(create_fields_result1), 2)
  expect_true(all(create_fields_result1$success=='true'))
  
  expect_is(create_fields_result2, "data.frame")
  expect_equal(names(create_fields_result2), c('fullName', 'success'))
  expect_equal(nrow(create_fields_result2), 2)
  expect_true(all(create_fields_result2$success=='true'))
})

test_that("rforcecom.updateMetadata", {
  expect_is(updated_custom_object_result, "data.frame")
  expect_equal(names(updated_custom_object_result), c('fullName', 'success'))
  expect_equal(nrow(updated_custom_object_result), 1)
  expect_equal(updated_custom_object_result$success, 'true')
})

test_that("rforcecom.renameMetadata", {
  expect_is(updated_custom_object_result, "data.frame")
  expect_equal(names(updated_custom_object_result), c('fullName', 'success'))
  expect_equal(nrow(updated_custom_object_result), 1)
  expect_equal(updated_custom_object_result$success, 'true')
})

test_that("rforcecom.upsertMetadata", {
  expect_is(upserted_custom_object_result, "data.frame")
  expect_equal(names(upserted_custom_object_result), c('created', 'fullName', 'success'))
  expect_equal(nrow(upserted_custom_object_result), 1)
  expect_equal(upserted_custom_object_result$success, 'true')
  expect_equal(upserted_custom_object_result$created, 'true')
})

test_that("rforcecom.deleteMetadata", {
  expect_is(deleted_custom_object_result, "data.frame")
  expect_equal(names(deleted_custom_object_result), c('fullName', 'success'))
  expect_equal(nrow(deleted_custom_object_result), 2)
  expect_true(all(deleted_custom_object_result$success=='true'))
})
