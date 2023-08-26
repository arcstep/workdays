##
distinct0 <- function(d, columns) {
  d |> distinct(!!!dplyr::syms(columns[!is.null(columns)]))
}

#' @title 计算时间段内包含的工作日序列
#' @description
#' 如果需要按照ID排除工作日数量，则要求调用该函数。
#' 
#' @param d 要处理的数据集
#' @param fromColumn 开始日期字段名
#' @param toColumn 截止日期字段名
#' @param idColumn 合并工作日依据的ID字段名称
#' @param fromIgnore 计算工作日时忽略开始列
#' @param toIgnore 计算工作日时忽略结束列
#' @param path 指定节假日设定文件
#' @export
workdays_seq <- function(d, fromColumn, toColumn, idColumn = NULL,
                         fromIgnore = TRUE, toIgnore = TRUE, path = NULL, withRaw = FALSE) {
  if(withRaw) {
    d0 <- d
  } else {
    d0 <- d |>
      mutate(`@from` = lubridate::as_date(!!dplyr::sym(fromColumn))) |>
      mutate(`@to` = lubridate::as_date(!!dplyr::sym(toColumn))) |>
      filter(!is.na(`@from`) & !is.na(`@to`))
  }
  ##
  if(fromIgnore || toIgnore) {
    days1 <- d0
    # days1 <- d0 |> filter(`@from` < `@to`)
  } else {
    days1 <- d0
  }
  ##
  fromDay <- (days1 |> arrange(`@from`))$`@from`[[1]]
  toDay <- (days1 |> arrange(desc(`@to`)))$`@to`[[1]]
  wd <- workdays_zh_CN(fromDay, toDay, path) |> filter(工作日标记)
  resp <- days1 |>
    distinct0(c("@from", "@to", idColumn)) |>
    mutate(`@seq` = purrr::map2(`@from`, `@to`, ~ .x:.y |> lubridate::as_date())) |>
    tidyr::unnest(cols = c(`@seq`)) |>
    semi_join(wd, by = c("@seq"="日期"))
  ##
  if(fromIgnore) {
    dFrom <- resp$`@seq` != resp$`@from`
  } else {
    dFrom <- TRUE
  }
  if(toIgnore) {
    dTo <- resp$`@seq` != resp$`@to`
  } else {
    dTo <- TRUE
  }
  resp |> filter(dFrom & dTo)
}

#' @title 计算时间段内包含的工作日数量
#' @description
#' 支持按照ID字段合并
#' 
#' @param d 要处理的数据集
#' @param fromColumn 开始日期字段名
#' @param toColumn 截止日期字段名
#' @param idColumn 合并工作日依据的ID字段名称
#' @param newName 工作日字段命名
#' @param fromIgnore 计算工作日时忽略开始列
#' @param toIgnore 计算工作日时忽略结束列
#' @param path 指定节假日设定文件
#' @export
workdays_count <- function(d, fromColumn, toColumn, idColumn = NULL, newName = "工作日数",
                           fromIgnore = FALSE, toIgnore = FALSE, path = NULL, withRaw = FALSE) {
  if(withRaw) {
    d0 <- d
  } else {
    d0 <- d |>
      mutate(`@from` = lubridate::as_date(!!dplyr::sym(fromColumn))) |>
      mutate(`@to` = lubridate::as_date(!!dplyr::sym(toColumn))) |>
      filter(!is.na(`@from`) & !is.na(`@to`))
  }
  if(is.null(idColumn)) {
    resp <- d0 |>
      left_join(
        workdays_seq(d0, fromColumn, toColumn, idColumn, fromIgnore, toIgnore, path, withRaw = TRUE) |>
          count(`@from`, `@to`, name = "@n"),
        by = c("@from", "@to")) |>
      select(-`@from`, -`@to`)
  } else {
    resp <- workdays_seq(d0, fromColumn, toColumn, idColumn, fromIgnore, toIgnore) |>
      distinct0(c(idColumn, "@seq")) |>
      count(!!dplyr::sym(idColumn), name = "@n")
  }
  resp |>
    tidyr::replace_na(list("@n" = 0L)) |>
    rename(!!dplyr::sym(newName) := `@n`)
}


