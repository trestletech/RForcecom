
context("Testing login features")

test_that("Checking login and logout", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  session <- NULL
  
  # test login
  tryCatch(session <- rforcecom.login(username, password))
  expect_true(length(session) > 0)
  
  # test logout
  tryCatch(logout_result <- rforcecom.logout(session))
  expect_true(logout_result)
})