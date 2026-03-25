## Code to prepare the `jfk19571101` dataset

jfk19571101 <- readLines("data-raw/jfk19571101.txt", warn = FALSE)
jfk19571101 <- paste(jfk19571101, collapse = "\n")

usethis::use_data(jfk19571101, overwrite = TRUE)
