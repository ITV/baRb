test_that("barb_get_spots(async = FALSE) returns data", {
  expect_true(nrow(barb_get_spots(min_transmission_date = '2024-01-01', max_transmission_date = '2024-01-01', advertiser_name = 'HAYS TRAVEL', async = FALSE)) > 10)
})
test_that("barb_get_spots(async = TRUE) returns data", {
  expect_true(nrow(barb_get_spots(min_transmission_date = '2024-01-01', max_transmission_date = '2024-01-01', advertiser_name = 'HAYS TRAVEL', async = TRUE)) > 10)
})
