
context("Testing login features")

test_that("Checking login and logout", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  session <- NULL
  
  # test successful login
  tryCatch(session <- rforcecom.login(username, password))
  expect_true(length(session) > 0)
  
  # test successful logout
  tryCatch(logout_result <- rforcecom.logout(session))
  expect_true(logout_result)
  
  # test unsuccessful login
  expect_error(rforcecom.login(username='FailingTest', password='WrongPassword'))
  
  # test unsuccessful logout
  expect_error(rforcecom.logout(session='FailingTest'))
})