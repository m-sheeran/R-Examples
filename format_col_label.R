# Remove spaces and convert character labels to lowercase for column headers

format_col_label = function(df)
{
  
  require(data.table)
  setnames(df, c(names(df)), c(make.names(tolower(names(df)))))
  
}