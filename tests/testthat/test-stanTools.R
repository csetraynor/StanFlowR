# ===========================================================================
# Tests for write_stan_json()
# ===========================================================================

test_that("write_stan_json writes scalar values correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(theta = 0.5, N = 10L), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(result$theta, 0.5)
  expect_equal(result$N, 10L)

  raw <- paste(readLines(tmp), collapse = "")
  expect_false(grepl('"theta"\\s*:\\s*\\[', raw))
  expect_false(grepl('"N"\\s*:\\s*\\[', raw))
})

test_that("write_stan_json writes integer vectors correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(y = c(1L, 2L, 3L, 4L, 5L)), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(result$y, c(1L, 2L, 3L, 4L, 5L))

  raw <- paste(readLines(tmp), collapse = "")
  expect_false(grepl("1\\.0", raw))
})

test_that("write_stan_json writes real vectors correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(x = c(1.5, 2.7, 3.14)), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(result$x, c(1.5, 2.7, 3.14))
})

test_that("write_stan_json writes matrices in row-major order", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  # R matrix: 2 rows, 3 cols — column-major storage: 1,2,3,4,5,6
  # Row 1: [1, 3, 5], Row 2: [2, 4, 6]
  X <- matrix(1:6, nrow = 2, ncol = 3)
  write_stan_json(list(X = X), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(nrow(result$X), 2)
  expect_equal(ncol(result$X), 3)
  expect_equal(result$X[1, ], c(1, 3, 5))
  expect_equal(result$X[2, ], c(2, 4, 6))
})

test_that("write_stan_json writes 3D arrays correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  arr <- array(1:12, dim = c(2, 3, 2))
  write_stan_json(list(arr = arr), file = tmp)

  result <- jsonlite::fromJSON(tmp)
  expect_equal(dim(result$arr), c(2, 3, 2))
})

test_that("write_stan_json converts logicals to integers", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(flag = TRUE, flags = c(TRUE, FALSE, TRUE)), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(result$flag, 1L)
  expect_equal(result$flags, c(1L, 0L, 1L))
})

test_that("write_stan_json converts factors to integers", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  f <- factor(c("a", "b", "c", "a"))
  write_stan_json(list(group = f), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(result$group, c(1L, 2L, 3L, 1L))
})

test_that("write_stan_json converts data.frames via data.matrix", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = c(1.0, 2.0), b = c(3L, 4L))
  write_stan_json(list(df = df), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(nrow(result$df), 2)
  expect_equal(ncol(result$df), 2)
})

test_that("write_stan_json handles empty vectors", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(empty = integer(0)), file = tmp)
  result <- jsonlite::fromJSON(tmp)

  expect_equal(length(result$empty), 0)
})

test_that("write_stan_json returns file path invisibly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  out <- write_stan_json(list(x = 1), file = tmp)
  expect_equal(out, tmp)
})

test_that("write_stan_json produces valid JSON", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  set.seed(42)
  write_stan_json(list(
    N = 100L,
    theta = 0.5,
    y = c(1L, 0L, 1L, 1L, 0L),
    X = matrix(rnorm(12), nrow = 3, ncol = 4),
    flag = TRUE
  ), file = tmp)

  expect_no_error(jsonlite::fromJSON(tmp))
})

test_that("write_stan_json: scalar unboxed, 1x1 matrix nested", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  mat <- matrix(5, nrow = 1, ncol = 1)
  write_stan_json(list(scalar = 5, mat = mat), file = tmp)

  raw <- paste(readLines(tmp), collapse = "")
  expect_true(grepl('"scalar"\\s*:\\s*5', raw))
  expect_true(grepl('"mat"\\s*:\\s*\\[\\s*\\[\\s*5\\s*\\]\\s*\\]', raw))
})


# ===========================================================================
# Tests for new_stanrdump() bug fix (regex issue)
# ===========================================================================

test_that("new_stanrdump rdump works at width 271 (original bug)", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$alpha <- 1.5
  e$beta <- c(1.0, 2.0, 3.0)
  e$X <- matrix(1:6, nrow = 2)

  expect_no_error(
    new_stanrdump(c("alpha", "beta", "X"), file = tmp, envir = e,
                  width = 271, format = "rdump")
  )

  content <- readLines(tmp)
  expect_true(length(content) > 0)
  expect_true(any(grepl("alpha", content)))
})

test_that("new_stanrdump rdump works with default width", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$theta <- 0.5
  e$y <- c(1L, 2L, 3L)

  expect_no_error(
    new_stanrdump(c("theta", "y"), file = tmp, envir = e, format = "rdump")
  )

  content <- readLines(tmp)
  expect_true(any(grepl("theta", content)))
  expect_true(any(grepl("0.5", content)))
})

test_that("new_stanrdump rdump works with very large width", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  set.seed(1)
  e <- new.env()
  e$x <- rnorm(100)

  expect_no_error(
    new_stanrdump("x", file = tmp, envir = e, width = 10000, format = "rdump")
  )
})

test_that("new_stanrdump rdump works with width = 80", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$long_vector <- seq(1, 100, by = 0.1)

  expect_no_error(
    new_stanrdump("long_vector", file = tmp, envir = e, width = 80, format = "rdump")
  )
})

test_that("new_stanrdump rdump output is readable by R source()", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$a <- 42L
  e$b <- c(1.1, 2.2, 3.3)
  e$M <- matrix(1:4, nrow = 2)

  new_stanrdump(c("a", "b", "M"), file = tmp, envir = e, format = "rdump")

  e2 <- new.env()
  source(tmp, local = e2)
  expect_equal(e2$a, 42L)
  expect_equal(e2$b, c(1.1, 2.2, 3.3))
  expect_equal(e2$M, matrix(1:4, nrow = 2))
})


# ===========================================================================
# Tests for new_stanrdump() JSON format
# ===========================================================================

test_that("new_stanrdump defaults to JSON format", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$N <- 10L
  e$theta <- 0.5

  new_stanrdump(c("N", "theta"), file = tmp, envir = e)

  expect_no_error(jsonlite::fromJSON(tmp))
  parsed <- jsonlite::fromJSON(tmp)
  expect_equal(parsed$N, 10L)
  expect_equal(parsed$theta, 0.5)
})

test_that("new_stanrdump json changes .R extension to .json", {
  tmp <- tempfile(fileext = ".R")
  json_tmp <- sub("\\.R$", ".json", tmp)
  on.exit(unlink(c(tmp, json_tmp)), add = TRUE)

  e <- new.env()
  e$x <- 1.0

  new_stanrdump("x", file = tmp, envir = e, format = "json")

  expect_true(file.exists(json_tmp))
})

test_that("new_stanrdump warns about missing objects in JSON mode", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$x <- 1.0

  expect_warning(
    new_stanrdump(c("x", "nonexistent"), file = tmp, envir = e, format = "json"),
    "not found"
  )
})

test_that("new_stanrdump warns about illegal Stan names in JSON mode", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e[["for"]] <- 1.0

  expect_warning(
    new_stanrdump("for", file = tmp, envir = e, format = "json"),
    "not allowed in Stan"
  )
})

test_that("new_stanrdump JSON handles all supported types", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$scalar_int <- 5L
  e$scalar_real <- 3.14
  e$vec_int <- c(1L, 2L, 3L)
  e$vec_real <- c(1.1, 2.2, 3.3)
  e$mat <- matrix(1:6, nrow = 2)
  e$logical_val <- TRUE
  e$logical_vec <- c(TRUE, FALSE, TRUE)
  e$fac <- factor(c("a", "b", "a"))

  expect_no_error(
    new_stanrdump(ls(e), file = tmp, envir = e, format = "json")
  )

  result <- jsonlite::fromJSON(tmp)
  expect_equal(result$scalar_int, 5)
  expect_equal(result$scalar_real, 3.14)
  expect_equal(result$vec_int, c(1, 2, 3))
  expect_equal(result$mat[1, ], c(1, 3, 5))
  expect_equal(result$logical_val, 1)
  expect_equal(result$fac, c(1, 2, 1))
})


# ===========================================================================
# Tests for new_stanrdump() existing behavior preservation
# ===========================================================================

test_that("new_stanrdump warns about non-existent objects (rdump)", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$x <- 1.0

  expect_warning(
    new_stanrdump(c("x", "y_missing"), file = tmp, envir = e, format = "rdump"),
    "not found"
  )
})

test_that("new_stanrdump handles empty vector in rdump", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$empty <- integer(0)

  expect_no_error(
    new_stanrdump("empty", file = tmp, envir = e, format = "rdump")
  )

  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("integer\\(0\\)", content))
})

test_that("new_stanrdump handles scalar in rdump", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()
  e$a <- 42

  new_stanrdump("a", file = tmp, envir = e, format = "rdump")

  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("a <- 42", content))
})

test_that("new_stanrdump quiet = TRUE suppresses warnings", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  e <- new.env()

  expect_no_warning(
    new_stanrdump("nonexistent", file = tmp, envir = e,
                  quiet = TRUE, format = "rdump")
  )
})


# ===========================================================================
# Tests for is_legal_stan_vname()
# ===========================================================================

test_that("is_legal_stan_vname rejects Stan keywords", {
  expect_false(is_legal_stan_vname("for"))
  expect_false(is_legal_stan_vname("while"))
  expect_false(is_legal_stan_vname("int"))
  expect_false(is_legal_stan_vname("real"))
  expect_false(is_legal_stan_vname("model"))
  expect_false(is_legal_stan_vname("data"))
})

test_that("is_legal_stan_vname rejects C++ keywords", {
  expect_false(is_legal_stan_vname("class"))
  expect_false(is_legal_stan_vname("return"))
  expect_false(is_legal_stan_vname("namespace"))
})

test_that("is_legal_stan_vname rejects names with dots", {
  expect_false(is_legal_stan_vname("my.var"))
})

test_that("is_legal_stan_vname rejects names starting with digit", {
  expect_false(is_legal_stan_vname("1abc"))
})

test_that("is_legal_stan_vname rejects names ending with __", {
  expect_false(is_legal_stan_vname("myvar__"))
})

test_that("is_legal_stan_vname accepts valid names", {
  expect_true(is_legal_stan_vname("theta"))
  expect_true(is_legal_stan_vname("my_var"))
  expect_true(is_legal_stan_vname("x1"))
  expect_true(is_legal_stan_vname("alpha_beta"))
})


# ===========================================================================
# Tests for write_stan_json() edge cases and CmdStan compatibility
# ===========================================================================

test_that("write_stan_json handles typical CmdStan init list", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  inits <- list(
    log_ke = -1.5,
    log_V = 3.2,
    sigma = 0.1,
    eta = c(0.01, -0.02, 0.03, -0.01, 0.02)
  )

  write_stan_json(inits, file = tmp)

  result <- jsonlite::fromJSON(tmp)
  expect_equal(result$log_ke, -1.5)
  expect_equal(result$log_V, 3.2)
  expect_equal(result$sigma, 0.1)
  expect_equal(result$eta, c(0.01, -0.02, 0.03, -0.01, 0.02))
})

test_that("write_stan_json handles large matrices", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  set.seed(7)
  big_mat <- matrix(rnorm(1000), nrow = 50, ncol = 20)

  expect_no_error(write_stan_json(list(X = big_mat), file = tmp))

  result <- jsonlite::fromJSON(tmp)
  expect_equal(dim(result$X), c(50, 20))
  expect_equal(result$X[1, 1], big_mat[1, 1], tolerance = 1e-10)
})

test_that("write_stan_json handles negative values and extreme floats", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(
    neg = -5.5,
    neg_vec = c(-1.0, -2.0, -3.0),
    zero = 0.0,
    small = 1e-300,
    large = 1e300
  ), file = tmp)

  result <- jsonlite::fromJSON(tmp)
  expect_equal(result$neg, -5.5)
  expect_equal(result$neg_vec, c(-1.0, -2.0, -3.0))
  expect_equal(result$zero, 0.0)
  expect_equal(result$small, 1e-300)
  expect_equal(result$large, 1e300)
})

test_that("write_stan_json errors on non-list input", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_error(write_stan_json(c(1, 2, 3), file = tmp))
  expect_error(write_stan_json("not a list", file = tmp))
})

test_that("write_stan_json errors on unnamed list", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_error(write_stan_json(list(1, 2, 3), file = tmp))
})

test_that("write_stan_json handles .Machine$integer.max", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_stan_json(list(big = .Machine$integer.max), file = tmp)
  result <- jsonlite::fromJSON(tmp)
  expect_equal(result$big, .Machine$integer.max)
})


# ===========================================================================
# Tests for colVars()
# ===========================================================================

test_that("colVars computes column variances correctly", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  result <- colVars(mat)

  expect_equal(result[1], var(c(1, 2, 3)))
  expect_equal(result[2], var(c(4, 5, 6)))
  expect_length(result, 2)
})

test_that("colVars handles single column matrix", {
  mat <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
  result <- colVars(mat)

  expect_equal(result, var(c(1, 2, 3, 4, 5)))
  expect_length(result, 1)
})


# ===========================================================================
# Tests for real_is_integer()
# ===========================================================================

test_that("real_is_integer identifies whole numbers", {
  expect_true(real_is_integer(c(1.0, 2.0, 3.0)))
  expect_true(real_is_integer(c(-1.0, 0.0, 100.0)))
  expect_true(real_is_integer(integer(0)))
})

test_that("real_is_integer rejects non-integers", {
  expect_false(real_is_integer(c(1.5, 2.0)))
  expect_false(real_is_integer(c(Inf)))
  expect_false(real_is_integer(c(NaN)))
})
