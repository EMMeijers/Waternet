# Helper functions, private use within the Waternet lib.

.check_df_names <- function(df, headers) {
  `%not_in%` <- Negate(`%in%`)
  pass <- T
  for (colname in headers) {
    if (colname %not_in% names(df)) {
      print(paste0("input dataframe is missing the <",colname,"> column"))
      pass <- F
    }
  }
  if (!pass) {stop("dataframe is missing correct headers")}
}