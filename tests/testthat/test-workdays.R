library(tibble)

test_that("计算工作日时，普通情况", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1:100000,
    "2023-1-5", "2023-1-10", 1:10000,
    "2023-1-1", "2023-1-10", 1:10000,
    "2023-1-11", "2023-1-21", 1:10000) |>
    tidyr::unnest(z)
  resp <- add_workdays(d, "x", "y")
  resp$工作日数[[1]] |> testthat::expect_equal(6)
})

test_that("计算工作日时，根据ID合并工作日数", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1,
    "2023-1-1", "2023-1-5", 2,
    "2023-1-6", "2023-1-10", 2,
    "2023-1-8", "2023-1-21", 3,
    "2023-1-8", "2023-1-15", 4,
    "2023-1-11", "2023-1-21", 4)
  resp <- add_workdays_by_id(d, "x", "y", id = "z")
  resp$工作日数[[1]] |> testthat::expect_equal(resp$工作日数[[2]])
  resp$工作日数[[3]] |> testthat::expect_equal(resp$工作日数[[4]])
})

test_that("计算工作日时，不计算开始日期", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1,
    "2023-1-5", "2023-1-10", 2,
    "2023-1-1", "2023-1-10", 3,
    "2023-1-11", "2023-1-21", 4)
  add_workdays(d, "x", "y")$工作日数 |>
    testthat::expect_equal(c(6, 4, 6, 8))
  add_workdays(d, "x", "y", ignoreFrom = T)$工作日数 |>
    testthat::expect_equal(c(6, 3, 6, 7))
  add_workdays(d, "x", "y", ignoreTo = T)$工作日数 |>
    testthat::expect_equal(c(5, 3, 5, 8))
  add_workdays(d, "x", "y", ignoreFrom = T, ignoreTo = T)$工作日数 |>
    testthat::expect_equal(c(5, 2, 5, 7))
})

test_that("计算工作日时，开始日期和结束日期相同", {
})

test_that("计算工作日时，开始日期为假日，结束为工作日", {
})

test_that("计算工作日时，开始日期为假日，结束为假日", {
})

test_that("计算工作日时，开始日期为工作日，结束为工作日", {
})

test_that("计算工作日时，开始日期为工作日，结束为假日", {
})

test_that("计算工作日时，过程中包含要排除的时段", {
})
