library(tibble)
library(dplyr)

test_that("普通情况", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1:100000,
    "2023-1-5", "2023-1-10", 1:10000,
    "2023-1-1", "2023-1-10", 1:10000,
    "2023-1-11", "2023-1-21", 1:10000) |>
    tidyr::unnest(z)
  resp <- workdays_count(d, "x", "y", fromIgnore = T, toIgnore = F)
  resp$工作日数[[1]] |> testthat::expect_equal(6)
})

test_that("根据ID合并工作日数", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1,
    "2023-1-1", "2023-1-5", 2,
    "2023-1-6", "2023-1-10", 2,
    "2023-1-8", "2023-1-21", 3,
    "2023-1-8", "2023-1-15", 4,
    "2023-1-11", "2023-1-21", 4)
  resp <- workdays_count(d, "x", "y", idColumn = "z", fromIgnore = F, toIgnore = F)
  resp$工作日数[[1]] |> testthat::expect_equal(resp$工作日数[[2]])
  resp$工作日数[[3]] |> testthat::expect_equal(resp$工作日数[[4]])
})

test_that("不计算开始日期或结束日期", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1,
    "2023-1-5", "2023-1-10", 2,
    "2023-1-1", "2023-1-10", 3,
    "2023-1-11", "2023-1-21", 4)
  workdays_count(d, "x", "y", fromIgnore = T, toIgnore = F)$工作日数 |> testthat::expect_equal(c(6, 3, 6, 7))
  workdays_count(d, "x", "y", fromIgnore = F, toIgnore = T)$工作日数 |> testthat::expect_equal(c(5, 3, 5, 8))
  workdays_count(d, "x", "y", fromIgnore = T, toIgnore = T)$工作日数 |> testthat::expect_equal(c(5, 2, 5, 7))
  
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-10", 1,
    "2023-1-1", "2023-1-5", 2,
    "2023-1-6", "2023-1-10", 2,
    "2023-1-8", "2023-1-21", 3,
    "2023-1-9", "2023-1-15", 4,
    "2023-1-11", "2023-1-21", 4)
  resp <- workdays_count(d, "x", "y", idColumn = "z", fromIgnore = T, toIgnore = F)
  resp$工作日数 |> testthat::expect_equal(c(6, 5, 10, 9))
})

test_that("开始日期和结束日期相同", {
  d <- tribble(
    ~x, ~y, ~z,
    "2023-1-1", "2023-1-1", 1,
    "2023-1-5", "2023-1-5", 2,
    "2023-1-15", "2023-1-15", 3,
    "2023-1-21", "2023-1-21", 4)
  workdays_count(d, "x", "y", fromIgnore = T, toIgnore = T)$工作日数 |> testthat::expect_equal(c(0, 0, 0, 0))
  workdays_count(d, "x", "y", fromIgnore = T, toIgnore = F)$工作日数 |> testthat::expect_equal(c(0, 0, 0, 0))
  workdays_count(d, "x", "y", fromIgnore = F, toIgnore = T)$工作日数 |> testthat::expect_equal(c(0, 0, 0, 0))
})

test_that("性能测试", {
  skip("测试性能")
  d <- arrow::read_feather(testthat::test_path("data/网办件时间样例1e5.fea")) |>
    mutate(收件日 = lubridate::as_date(收件时间)) |>
    mutate(办结日 = lubridate::as_date(办结时间)) |>
    filter(!is.na(收件日) & !is.na(办结日) & 收件日 <= 办结日) |>
    mutate(`@from` = 收件日, `@to` = 办结日)
  workdays_count(d, "办结日", "收件日")
  
  d2 <- arrow::read_feather(testthat::test_path("data/环节时间样例1e5.fea")) |>
    semi_join(d, by = "收件编号") |>
    filter(业务状态 %in% c("补齐补正", "挂起"))
  workdays_count(d2, "接收时间", "提交时间", idColumn = "收件编号")
})
