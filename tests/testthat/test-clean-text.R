test_that("clean_text removes square brackets", {
  expect_equal(clean_text("hello [applause] world"), "hello world")
})

test_that("clean_text removes parentheses", {
  expect_equal(clean_text("hello (laughter) world"), "hello world")
})

test_that("clean_text removes curly braces", {
  expect_equal(clean_text("hello {stage direction} world"), "hello world")
})

test_that("clean_text handles nested brackets", {
  expect_equal(clean_text("a [b [c] d] e"), "a d e")
  expect_equal(clean_text("a [b] [c] e"), "a e")
})

test_that("clean_text removes fullwidth brackets", {
  expect_equal(clean_text("text \uff3bnote\uff3d more"), "text more")
})

test_that("clean_text removes fullwidth parentheses", {
  expect_equal(clean_text("text \uff08note\uff09 more"), "text more")
})

test_that("clean_text converts factor input", {
  expect_equal(clean_text(factor("hello [x] world")), "hello world")
})

test_that("clean_text collapses double em-dashes", {
  expect_equal(clean_text("word \u2014 \u2014 word"), "word\u2014word")
})

test_that("clean_text normalizes whitespace", {
  expect_equal(clean_text("too   many    spaces"), "too many spaces")
})

test_that("clean_text leaves clean text unchanged", {
  expect_equal(clean_text("nothing to clean here"), "nothing to clean here")
})

test_that("clean_text removes stray unmatched brackets", {
  expect_equal(clean_text("stray ] bracket [ here"), "stray bracket here")
})

test_that("clean_text works on character vectors", {
  result <- clean_text(c("first [a] text", "second (b) text"))
  expect_equal(result, c("first text", "second text"))
})
