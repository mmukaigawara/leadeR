test_that("exported functions exist", {
  expect_true(is.function(get_oca))
  expect_true(is.function(get_power))
  expect_true(is.function(get_dist))
  expect_true(is.function(get_nat))
  expect_true(is.function(get_aff))
  expect_true(is.function(get_ctrl))
  expect_true(is.function(get_complex))
  expect_true(is.function(get_task))
  expect_true(is.function(get_conf))
  expect_true(is.function(get_lta))
})

test_that("LTA functions have correct parameters", {
  # Functions requiring own_entity
  for (fn_name in c("get_power", "get_dist", "get_nat", "get_aff", "get_ctrl")) {
    fn <- get(fn_name)
    expect_true("own_entity" %in% names(formals(fn)),
                info = paste(fn_name, "should have own_entity param"))
    expect_true("text" %in% names(formals(fn)),
                info = paste(fn_name, "should have text param"))
    expect_true("bootstrap" %in% names(formals(fn)),
                info = paste(fn_name, "should have bootstrap param"))
  }

  # Functions NOT requiring own_entity
  for (fn_name in c("get_complex", "get_task", "get_conf")) {
    fn <- get(fn_name)
    expect_true("text" %in% names(formals(fn)),
                info = paste(fn_name, "should have text param"))
    expect_true("bootstrap" %in% names(formals(fn)),
                info = paste(fn_name, "should have bootstrap param"))
  }

  # OCA function
  expect_true("own_entity" %in% names(formals(get_oca)))
  expect_true("text" %in% names(formals(get_oca)))

  # LTA wrapper
  expect_true("own_entity" %in% names(formals(get_lta)))
  expect_true("text" %in% names(formals(get_lta)))
  expect_true("bootstrap" %in% names(formals(get_lta)))
  expect_true("B" %in% names(formals(get_lta)))
})
