##
add_workdays0 <- function(d, from, to, newName = "工作日数",
                          path = NULL, ignoreFrom = FALSE, ignoreTo = FALSE) {
  stopifnot("data.frame" %in% class(d))
  ## 构造需要处理的数据集
  d0 <- d |>
    mutate(`@from` = lubridate::as_date(d[[from]])) |>
    mutate(`@to` = lubridate::as_date(d[[to]]))
  ## 复杂性降低后的数据集
  ds <- d0 |>
    distinct(`@from`, `@to`) |>
    mutate(`@from` = lubridate::as_date(`@from`)) |>
    mutate(`@to` = lubridate::as_date(`@to`))
  ##
  fromDay <- ds$`@from` |> purrr::discard(~ is.na(.x)) |> min()
  toDay <- ds$`@to` |> purrr::discard(~ is.na(.x)) |> max()
  wd <- workdays_zh_CN(fromDay, toDay, path) |>
    mutate(工作日标记 = ifelse(工作日标记, 累计工作日数, 0L))
  ## 生成日期序列数据集
  ds0 <- ds |>
    left_join(wd, by = c("@from"="日期")) |>
    rename(`@seq_from` = 累计工作日数, `@flag_from` = 工作日标记) |>
    left_join(wd, by = c("@to"="日期")) |>
    rename(`@seq_to` = 累计工作日数, `@flag_to` = 工作日标记) |>
    mutate(
      `@workdays` = purrr::pmap(
        list(`@seq_from`, `@seq_to`, `@flag_from`, `@flag_to`),
        function(seq_from, seq_to, flag_from, flag_to) {
          ignoreSeqs <- 0L
          if(ignoreFrom) { ignoreSeqs <- c(ignoreSeqs, flag_from) }
          if(ignoreTo) { ignoreSeqs <- c(ignoreSeqs, flag_to) }
          q <- seq(seq_from, seq_to)
          q[!(q %in% ignoreSeqs)] + as.integer(fromDay)
        })) |>
    mutate(!!dplyr::sym(newName) := purrr::map_int(`@workdays`, ~ .x |> length()))
  ##
  x_by <- "@from"
  y_by <- "@to"
  names(x_by) <- from
  names(y_by) <- to
  d0 |>
    left_join(ds0 |> select(`@from`, `@to`, `@workdays`, !!dplyr::sym(newName)), by = c("@from", "@to")) |>
    select(-`@from`, -`@to`)
}

#' @title 计算时间段内包含的工作日数量
#' @param d 要补充字段的数据集
#' @param from 开始日期字段名
#' @param to 截止日期字段名
#' @param newName 工作日字段命名
#' @param path 指定节假日设定文件
#' @param ignoreFrom 计算工作日时忽略开始列
#' @param ignoreTo 计算工作日时忽略结束列
#' @param withSeq 返回工作日的标记序列，以便后续计算
#' @export
add_workdays <- function(d, from, to, newName = "工作日数",
                         path = NULL, ignoreFrom = FALSE, ignoreTo = FALSE, withSeq = FALSE) {
  resp <- add_workdays0(d, from, to, newName, path, ignoreFrom, ignoreTo)
  if(withSeq) {
    resp
  } else {
    resp |> select(-`@workdays`)
  }
}

#' @title 计算时间段内包含的工作日数量，按照ID合并
#' @param d 要补充字段的数据集
#' @param from 开始日期字段名
#' @param to 截止日期字段名
#' @param idName 合并工作日依据的ID字段名称
#' @param newName 工作日字段命名
#' @param path 指定节假日设定文件
#' @param ignoreFrom 计算工作日时忽略开始列
#' @param ignoreTo 计算工作日时忽略结束列
#' @param withSeq 返回工作日的标记序列，以便后续计算
#' @export
add_workdays_by_id <- function(d, from, to, idName, newName = "工作日数",
                               path = NULL, ignoreFrom = FALSE, ignoreTo = FALSE, withSeq = FALSE) {
  resp <- add_workdays(d, from, to, newName, path, ignoreFrom, ignoreTo, withSeq = T) |>
    group_by(!!!dplyr::syms(idName)) |>
    reframe(`@workdays` = list(unlist(`@workdays`) |> unique()), `@count` = length(`@workdays`[[1]])) |>
    rename(!!dplyr::sym(newName) := `@count`)
  if(withSeq) {
    resp
  } else {
    resp |> select(-`@workdays`)
  }
}

