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

#' @title 按LIST格式查看默认的节假日调整清单
#' @description
#' 节假日定义，补充定义周六、周日之外的节假日调整和周一至周五之外的工作日调整。
#' 
#' @export
yaml_list <- function() {
  list(
    "2018年度" = list(
      "元旦" = list(
        desc = "2018年1月1日放假，与周末连休。",
        holidays = c("2018-1-1"),
        workdays = c()
      ),
      "春节" = list(
        desc = "2018年2月15日至2月21日放假调休，共7天。2月11日(星期日)、2月24日(星期六)上班。",
        holidays = c("2018-2-15", "2018-2-16", "2018-2-17", "2018-2-18", "2018-2-19", "2018-2-20", "2018-2-21"),
        workdays = c("2018-2-11", "2018-2-24")
      ),
      "清明节" = list(
        desc = "2018年4月5日至4月7日放假调休，共3天。4月8日(星期日)上班。",
        holidays = c("2018-4-5", "2018-4-7"),
        workdays = c("2018-4-8")
      ),
      "劳动节" = list(
        desc = "2018年4月29日至5月1日放假调休，共3天。4月28日(星期六)上班。",
        holidays = c("2018-4-29", "2018-5-1"),
        workdays = c("2018-4-28")
      ),
      "端午节" = list(
        desc = "2018年6月18日放假，与周末连休。",
        holidays = c("2018-6-18"),
        workdays = c()
      ),
      "中秋节" = list(
        desc = "2018年9月24日放假，与周末连休。",
        holidays = c("2018-9-24"),
        workdays = c()
      ),
      "国庆节" = list(
        desc = "2018年10月1日至10月7日放假调休，共7天。9月29日(星期六)、9月30日(星期日)上班。",
        holidays = c("2018-10-1", "2018-10-2", "2018-10-3", "2018-10-4", "2018-10-5", "2018-10-6", "2018-10-7"),
        workdays = c("2018-9-29", "2018-9-30")
      )
    ),
    "2019年度" = list(
      "元旦" = list(
        desc = "2018年12月30日至2019年1月1日放假3天。",
        holidays = c("2018-12-30", "2018-12-31", "2019-1-1"),
        workdays = c()
      ),
      "春节" = list(
        desc = "2019年2月4日至10日放假7天。",
        holidays = c("2019-2-4", "2019-2-5", "2019-2-6", "2019-2-7", "2019-2-8", "2019-2-9", "2019-2-10"),
        workdays = c()
      ),
      "清明节" = list(
        desc = "2019年4月4日至6日放假3天。",
        holidays = c("2019-4-4", "2019-4-5", "2019-4-6"),
        workdays = c()
      ),
      "劳动节" = list(
        desc = "5月1日放假1天。",
        holidays = c("2019-5-1"),
        workdays = c()
      ),
      "端午节" = list(
        desc = "2019年6月7日至6月9日放假3天。",
        holidays = c("2019-6-7", "2019-6-8", "2019-6-9"),
        workdays = c()
      ),
      "中秋节" = list(
        desc = "2019年9月13日至9月15日放假3天，9月19日和10月12日上班。",
        holidays = c("2019-9-13", "2019-9-14", "2019-9-15"),
        workdays = c("2019-9-29", "2019-10-12")
      ),
      "国庆节" = list(
        desc = "2019年10月1日至10月7日放假7天。",
        holidays = c("2019-10-1", "2019-10-2", "2019-10-3", "2019-10-4", "2019-10-5", "2019-10-6", "2019-10-7"),
        workdays = c()
      )
    ),
    "2020年度" = list(
      "元旦" = list(
        desc = "2020年1月1日放假，共1天。",
        holidays = c("2020-1-1"),
        workdays = c()
      ),
      "春节" = list(
        desc = "2020年1月24日至1月30日放假调休，共7天。1月19日(星期日)、2月1日(星期六)上班。",
        holidays = c("2020-1-24", "2020-1-25", "2020-1-26", "2020-1-27", "2020-1-28", "2020-1-29", "2020-1-30"),
        workdays = c("2020-1-19", "2020-2-1")
      ),
      "清明节" = list(
        desc = "2020年4月4日至4月6日放假调休，共3天。",
        holidays = c("2020-4-3", "2020-4-4", "2020-4-5"),
        workdays = c()
      ),
      "劳动节" = list(
        desc = "2020年5月1日至5月5日放假调休，共5天。4月26日(星期日)、5月9日(星期六)上班。",
        holidays = c("2020-5-1", "2020-5-2", "2020-5-3", "2020-5-4", "2020-5-5"),
        workdays = c("2020-4-26", "2020-5-9")
      ),
      "端午节" = list(
        desc = "2020年6月25日至6月27日放假调休，共3天。6月28日(星期日)上班。",
        holidays = c("2020-6-25", "2020-6-26", "2020-6-27"),
        workdays = c("2020-6-28")
      ),
      "中秋节、国庆节" = list(
        desc = "2020年10月1日至10月8日放假调休，共8天。9月27日(星期日)、10月10日(星期六)上班。",
        holidays = c("2020-10-1", "2020-10-2", "2020-10-3", "2020-10-4", "2020-10-5", "2020-10-6", "2020-10-7", "2020-10-8"),
        workdays = c("2020-9-27", "2020-10-10")
      )
    ),
    "2021年度" = list(
      "元旦" = list(
        desc = "2021年1月1日至1月3日放假，共3天。",
        holidays = c("2021-1-1", "2021-1-2", "2021-1-3"),
        workdays = c()
      ),
      "春节" = list(
        desc = "2021年2月11日至2月17日放假调休，共7天。2月7日（星期日）、2月20日（星期六）上班。",
        holidays = c("2021-2-11", "2021-2-12", "2021-2-13", "2021-2-14", "2021-2-15", "2021-2-16", "2021-2-17"),
        workdays = c("2021-2-7", "2021-2-20")
      ),
      "清明节" = list(
        desc = "2021年4月3日至5日放假调休，共3天。",
        holidays = c("2021-4-3", "2021-4-4", "2021-4-5"),
        workdays = c()
      ),
      "劳动节" = list(
        desc = "2021年5月1日至5月5日放假调休，共5天。4月25日（星期日）、5月8日（星期六）上班。",
        holidays = c("2021-5-1", "2021-5-2", "2021-5-3", "2021-5-4", "2021-5-5"),
        workdays = c("2021-4-25", "2021-5-8")
      ),
      "端午节" = list(
        desc = "2021年6月12日至6月14日放假，共3天。",
        holidays = c("2021-6-12", "2021-6-13", "2021-6-14"),
        workdays = c()
      ),
      "中秋节" = list(
        desc = "2021年9月19日至9月21日放假调休，共3天。9月18日（星期六）上班。",
        holidays = c("2021-9-19", "2021-9-20", "2021-9-21"),
        workdays = c("2021-9-18")
      ),
      "国庆节" = list(
        desc = "2021年10月1日至10月7日放假调休，共7天。9月26日（星期日）、10月9日（星期六）上班。",
        holidays = c("2021-10-1", "2021-10-2", "2021-10-3", "2021-10-4", "2021-10-5", "2021-10-6", "2021-10-7"),
        workdays = c("2021-9-26", "2021-10-9")
      )
    ),
    "2022年度" = list(
      "元旦" = list(
        desc = "2022年1月1日至1月3日放假，共3天。",
        holidays = c("2022-1-1", "2022-1-2", "2022-1-3"),
        workdays = c()
      ),
      "春节" = list(
        desc = "2022年1月31日至2月6日放假调休，共7天。1月29日（星期六）、1月30日（星期日）上班。",
        holidays = c("2022-1-31", "2022-2-1", "2022-2-2", "2022-2-3", "2022-2-4", "2022-2-5", "2022-2-6"),
        workdays = c("2022-1-29", "2022-1-30")
      ),
      "清明节" = list(
        desc = "2022年4月3日至4月5日放假调休，共3天。4月2日（星期六）上班。",
        holidays = c("2022-4-3", "2022-4-4", "2022-4-5"),
        workdays = c("2022-4-2")
      ),
      "劳动节" = list(
        desc = "2022年4月30日至5月4日放假调休，共5天。4月24日（星期日）、5月7日（星期六）上班。",
        holidays = c("2022-4-30", "2022-5-1", "2022-5-2", "2022-5-3", "2022-5-4"),
        workdays = c("2022-4-24", "2022-5-7")
      ),
      "端午节" = list(
        desc = "2022年6月3日至6月5日放假，共3天。",
        holidays = c("2022-6-3", "2022-6-4", "2022-6-5"),
        workdays = c()
      ),
      "中秋节" = list(
        desc = "2022年9月10日至9月12日放假，共3天。",
        holidays = c("2022-9-10", "2022-9-11", "2022-9-12"),
        workdays = c()
      ),
      "国庆节" = list(
        desc = "2022年10月1日至10月7日放假调休，共7天。10月8日（星期六）、10月9日（星期日）上班。",
        holidays = c("2022-10-1", "2022-10-2", "2022-10-3", "2022-10-4", "2022-10-5", "2022-10-6", "2022-10-7"),
        workdays = c("2022-10-8", "2022-10-9")
      )
    ),
    "2023年度" = list(
      "元旦" = list(
        desc = "2022年12月31日至2023年1月2日放假，共3天。（1月3日周二上班）",
        holidays = c("2022-12-31", "2023-1-1", "2023-1-2"),
        workdays = c("2023-1-3")
      ),
      "春节" = list(
        desc = "2023年1月21日至1月27日放假调休，共7天。（1月28日和29日周六周日上班）",
        holidays = c("2023-1-21", "2023-1-22", "2023-1-23", "2023-1-24", "2023-1-25", "2023-1-26", "2023-1-27"),
        workdays = c("2023-1-28", "2023-1-29")
      ),
      "清明节" = list(
        desc = "2023年4月5日放假，共1天。（4月6日周四上班）",
        holidays = c("2023-4-5"),
        workdays = c("2023-4-6")
      ),
      "端午节" = list(
        desc = "2023年6月22日至6月24日放假公休，共3天。（6月25日周日上班）",
        holidays = c("2023-6-22", "2023-6-23", "2023-6-24"),
        workdays = c("2023-6-25")
      ),
      "中秋节、国庆节" = list(
        desc = "2023年9月29日至10月6日放假调休，共8天。（10月7日和8日周五周六上班）",
        holidays = c("2023-9-29", "2023-9-30", "2023-10-1", "2023-10-2", "2023-10-3", "2023-10-4", "2023-10-5", "2023-10-6"),
        workdays = c("2023-10-7", "2023-10-8")
      )
    )
  )
}

#' @title 打印默认的节假日调整清单
#' @export
yaml_cat <- function() {
  yaml_list() |> yaml::as.yaml() |> cat()
}

#' @title 导出默认的节假日调整清单
#' @export
yaml_write <- function(path = "./") {
  yaml_list() |> yaml::write_yaml(path)
}

#' @title 补充节假日定义
#' @description
#' 更多节假日定义可以使用该函数补充。
#' 
#' 使用该函数不会影响默认或已补充过的节假日定义。
#' 
#' @export
yaml_patch <- function(path = NULL) {
  if(is.null(path)) {
    all <- yaml_list()
  } else {
    all <- yaml::read_yaml(path)
  }
  yearsDef <- names(all)
  yearsDef |> purrr::walk(function(y) {
    daysDef <- names(all[[y]])
    daysDef |> purrr::walk(function(d) {
      all[[y]][[d]]$holidays |> purrr::walk(holiday_assign)
      all[[y]][[d]]$workdays |> purrr::walk(workday_assign)
    })
  })
  message(
    "Workdays Define: ",
    "Min ", lubridate::as_date(workdays.Env$holidays |> min()),
    ", ",
    "Max ", lubridate::as_date(workdays.Env$holidays |> max()))
}

## 读取默认的节假日定义
workdays_zh_CN_marker <- function(fromDay, toDay) {
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
workdays_zh_CN <- function(fromDay = "2018-01-01", toDay = "2023-12-31") {
  workdays_zh_CN_marker(fromDay, toDay) |>
    rename(日期 = day, 工作日标记 = workday)
}
