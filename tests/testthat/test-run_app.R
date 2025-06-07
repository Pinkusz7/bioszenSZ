test_that("run_app launches without error", {
  skip_on_cran()
  expect_error(MiApp::run_app(test.mode = TRUE), NA)
})
