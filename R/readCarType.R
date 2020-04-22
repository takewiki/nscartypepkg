#' 读取车型信息
#'
#' @param file_cartype  车型文件
#' @param heading 列名
#'
#' @return 返回数据
#' @import readxl
#' @export
#'
#' @examples
#' readCartype()
readCartype <- function(file_cartype,heading =c('品牌',	'车型',	'业务对象（近义词）',	'业务对象（同义词）'
)) {

  res<- read_excel(file_cartype)
  names(res) <- heading

  return(res)
}


#' 读取车型信息
#'
#' @param file_cartype  文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' readCartype_db()
readCartype_db <- function(file_cartype) {
  res <- readCartype(file_cartype,heading = c('FBrandName','FCartypeName','FBusiObj_like','FBusiOjb_same'))
  return(res)
}




#' 从数据库中读取数据
#'
#' @param conn 数据库连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' selectCarType()
selectCarType <- function(conn=conn_rds('nsic')) {
  sql <- 'select * from t_md_carType'
  res <-sql_select(conn,sql)
  return(res)

}




