


#' 获取链接
#'
#' @param table 表名
#'
#' @return 返回值
#' @import tsda
#' @import DTedit
#' @import shiny
#' @export
#'
#' @examples
#' getCarType
getCarType <- function(table='t_md_carType') {
  #建立链接
  conn <-conn_rds('nsic')
  sql <- sql_gen_select(conn,table = table)
  #print(sql)
  books <-sql_select(conn,sql)
  #print(books)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  print(fieldList)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    print(type)
    books[,i] <-format_to_dtedit(type)(books[,i])

  }

  return(books)
}


##### Callback functions.
#' 定义插入数据类型
#'
#' @param data 数据
#' @param row 行
#' @param table 表
#' @param f  函数
#' @param id_var  ID
#'
#' @return 返回值
#' @export
#'
#' @examples
#' carType.insert.callback()
carType.insert.callback <- function(data, row ,table='t_md_carType',f=getCarType,id_var='FCartypeName') {
  #建立链接
  conn <-conn_rds('nsic')
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']

      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])


  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

#' 更新数据类型
#'
#' @param data  数据
#' @param olddata 原始数据
#' @param row 行
#' @param table  数据表
#' @param f    函数
#' @param edit.cols 编辑列
#' @param id_var 代码
#'
#' @return 返回
#' @export
#'
#' @examples
#' carType.update.callback()
carType.update.callback <- function(data, olddata, row,
                                  table='t_md_carType',
                                  f=getCarType,
                                  edit.cols = c('FCartypeName','FBusiObj_like','FBusiOjb_same'),
                                  id_var='FCartypeName')
{
  #建立链接
  conn <-conn_rds('nsic')
  sql_header <- sql_gen_update(table);
  print(sql_header)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))


  }
  sql_body <- paste0(res,collapse = ',')
  print(sql_body)
  sql_tail <-paste0(' where ',id_var," = '",data[row,id_var],"'")
  query <- paste0(sql_header,sql_body,sql_tail)

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

#' 品牌车型的删除
#'
#' @param data 数据
#' @param row 行
#' @param table 表格
#' @param f  查询函数
#' @param id_var 变量表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' carType.delete.callback()
carType.delete.callback <- function(data, row ,table ='t_md_carType',f=getCarType,id_var='FCartypeName') {
  #建立链接
  conn <-conn_rds('nsic')
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',"'",data[row,id_var],"'")
  query <- paste0(sql_header,sql_tail)

  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}

