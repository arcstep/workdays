#' @title 计算时间段内包含的工作日数量
#' @param d 要补充字段的数据集
#' @param fromColumn 开始日期字段名
#' @param toColumn 截止日期字段名
#' @param newName 工作日字段命名
#' @export
add_workdays <- function(d, from, to, newName = "工作日数") {
  stopifnot("data.frame" %in% class(d))
  ## 构造需要处理的数据集
  d0 <- d |>
    mutate(`@from` = lubridate::as_date(d[[from]])) |>
    mutate(`@to` = lubridate::as_date(d[[to]]))
  ds <- d0 |>
    distinct(`@from`, `@to`) |>
    mutate(`@from` = lubridate::as_date(`@from`)) |>
    mutate(`@to` = lubridate::as_date(`@to`))
  wd <- workdays_zh_CN(
    fromDay = ds$`@from` |> purrr::discard(~ is.na(.x)) |> min(),
    toDay = ds$`@to` |> purrr::discard(~ is.na(.x)) |> max())
  ## 生成日期序列数据集
  ds0 <- ds |>
    dplyr::left_join(wd |> select(日期, 累计工作日数), by = c("@from"="日期")) |>
    rename(`@seq_from` = 累计工作日数) |>
    dplyr::left_join(wd |> select(日期, 累计工作日数), by = c("@to"="日期")) |>
    rename(`@seq_to` = 累计工作日数) |>
    mutate(`@workdays` = purrr::map2(
      `@seq_from`,
      `@seq_to`,
      function(x, y) {
        q <- seq(x, y)
        工作日 <- q[q != 0]
        list(工作日序列 = 工作日, 工作日数 = length(工作日))
      }),
      `工作日数` = `@workdays`) |>
    mutate(工作日数 = purrr::map_int(`@workdays`, ~ .x$工作日数))
  ds0[[newName]] <- ds0$工作日数
  ##
  x_by <- "@from"
  y_by <- "@to"
  names(x_by) <- from
  names(y_by) <- to
  d0 |>
    left_join(ds0 |> select(`@from`, `@to`, !!sym(newName)), by = c("@from", "@to")) |>
    select(-`@from`, -`@to`)
  ##
}