openSQLiteDB = function(database) {
  m = dbDriver("SQLite", max.con = 25)
  con = dbConnect(m, dbname=database)
  return( list(m=m, con=con) )
}

addDataFrameToSQLiteDB = function(con, tablename, df) {
  values.questionmarks = paste(paste(rep('?,', ncol(df)-1), collapse=''), '?', sep='')
  sql = paste('insert into ', tablename, ' values (', values.questionmarks, ')', sep='')

  dbBeginTransaction(con)
  dbGetPreparedQuery(con, sql, bind.data = df)
  dbCommit(con)
}

SQLiteQuery = function(con, sql) {
  rs = dbSendQuery(con, sql)
  fetch(rs)
}