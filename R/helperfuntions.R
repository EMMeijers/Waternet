# Helper functions, private use within the Waternet lib.
`%not_in%` <- Negate(`%in%`)

.check_df_names <- function(df, headers) {
  pass <- T
  colnames(df) <- tolower(colnames(df))
  for (colname in headers) {
    if (colname %not_in% names(df)) {
      print(paste0("input dataframe is missing the <",colname,"> column"))
      pass <- F
    }
  }
  if (!pass) {stop("dataframe is missing correct headers")}
}

