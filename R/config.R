##
workdays.Env <- new.env(parent = emptyenv())
workdays.Env$holidays <- c()
workdays.Env$workdays <- c()

## 增加假日调整
holiday_assign <- function(from, to = from) {
  workdays.Env$holidays <- c(
    workdays.Env$holidays,
    lubridate::as_date(from):lubridate::as_date(to) |> lubridate::as_date()
  )
}

## 增加工作日调整
workday_assign <- function(from, to = from) {
  workdays.Env$workdays <- c(
    workdays.Env$workdays,
    lubridate::as_date(from):lubridate::as_date(to) |> lubridate::as_date()
  )
}

## 节假日定义
## 补充定义周六、周日之外的节假日调整
## 和周一至周五之外的工作日调整
holidaysDefine <- function() {
  list(
    "2023年度" = list(
      "元旦" = list(
        desc = "2022年12月31日至2023年1月2日放假，共3天。（1月3日周二上班）",
        holidays = c("2022-12-31", "2023-1-1", "2023-1-2"),
        workdays = c("2023-1-3")
      ),
      "春节" = list(
        desc = "1月21日至27日放假调休，共7天。（1月28日和29日周六周日上班）",
        holidays = c("2023-1-21", "2023-1-22", "2023-1-23", "2023-1-24", "2023-1-25", "2023-1-26", "2023-1-27"),
        workdays = c("2023-1-28", "2023-1-29")
      ),
      "清明节" = list(
        desc = "4月5日放假，共1天。（4月6日周四上班）",
        holidays = c("2023-4-5"),
        workdays = c("2023-4-6")
      ),
      "端午节" = list(
        desc = "6月22日至24日放假公休，共3天。（6月25日周日上班）",
        holidays = c("2023-6-22", "2023-6-23", "2023-6-24"),
        workdays = c("2023-6-25")
      ),
      "中秋节、国庆节" = list(
        desc = "9月29日至10月6日放假调休，共8天。（10月7日和8日周五周六上班）",
        holidays = c("2023-9-29", "2023-9-30", "2023-10-1", "2023-10-2", "2023-10-3", "2023-10-4", "2023-10-5", "2023-10-6"),
        workdays = c("2023-10-7", "2023-10-8")
      )
    )
  )
}

#' @title 打印默认的节假日调整清单
#' @export
yaml_cat <- function() {
  holidaysDefine() |> yaml::as.yaml() |> cat()
}

#' @title 按LIST格式查看默认的节假日调整清单
#' @export
yaml_list <- function() {
  holidaysDefine()
}

#' @title 导出默认的节假日调整清单
#' @export
yaml_write <- function(path = "./") {
  holidaysDefine() |> yaml::write_yaml(path)
}

#' @title 加载节假日定义
#' 
#' @export
yaml_load <- function(path = NULL) {
  if(is.null(path)) {
    all <- yaml_list()
  } else {
    all <- yaml::read_yaml(path)
  }
  yearsDef <- names(all)
  yearsDef |> purrr::walk(function(y) {
    daysDef <- names(all[y])
    daysDef |> purrr::walk(function(d) {
      all[y][d]$holidays |> purrr::walk(holiday_assign)
      all[y][d]$workdays |> purrr::walk(workday_assign)
    })
  })
}

## 读取默认的节假日定义
workdays_zh_CN_marker <- function(fromDay, toDay, path) {
  yaml_load(path)
  ## 生成全年的时间序列
  days <- lubridate::as_date(fromDay):lubridate::as_date(toDay) |> lubridate::as_date()
  ## 生成全年的工作日标记序列
  ## 默认情况下，周一至周五为工作日，周六、周日为假日
  workmarkers <- days |> purrr::map_lgl(
    ~ lubridate::wday(.x, label = F) %in%  c(2:6))
  ## 按国务院发布的节假日，修改为节日放假
  workmarkers[days %in% workdays.Env$holidays] <- F
  ## 按国务院发布的节假日，调整为工作日
  workmarkers[days %in% workdays.Env$workdays] <- T
  ## 返回数据框
  tibble("day" = days |> lubridate::as_date(), "workday" = workmarkers)
}

#' @title 工作日清单
#' @description
#' 默认为周一至周五为工作日，再根据中国国务院发布的工作日调整政策修正
#'
#' @param fromDay 开始日期
#' @param toDay 截止日期
#' @family workday-funcs
#' @export
workdays_zh_CN <- function(fromDay = "2023-01-01", toDay = "2023-12-31", path = NULL) {
  workdays_zh_CN_marker(fromDay, toDay, path) |>
    mutate("flag_day" = ifelse(workday, 1L, 0L)) |>
    mutate("累计工作日数" = cumsum(flag_day)) |>
    select(-flag_day) |>
    rename(日期 = day, 工作日标记 = workday)
}
