% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateCarType.R
\name{carType.delete.callback}
\alias{carType.delete.callback}
\title{品牌车型的删除}
\usage{
carType.delete.callback(
  data,
  row,
  table = "t_md_carType",
  f = getCarType,
  id_var = "FCartypeName"
)
}
\arguments{
\item{data}{数据}

\item{row}{行}

\item{table}{表格}

\item{f}{查询函数}

\item{id_var}{变量表}
}
\value{
返回值
}
\description{
品牌车型的删除
}
\examples{
carType.delete.callback()
}
