test_that("get_power returns correct structure", {
  skip_if_not(
    tryCatch({spacyr::spacy_initialize(); TRUE}, error = function(e) FALSE),
    "spaCy not available"
  )

  result <- get_power("Malawi", "We will help to improve relations.")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("P", "OP") %in% names(result)))
})

test_that("get_complex returns correct structure", {
  skip_if_not(
    tryCatch({spacyr::spacy_initialize(); TRUE}, error = function(e) FALSE),
    "spaCy not available"
  )

  result <- get_complex("We should perhaps consider this option.")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("HC", "LC") %in% names(result)))
})
