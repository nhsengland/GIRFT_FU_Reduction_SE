
## List all objects in the global environment
all_objects <- ls()

## Filter out the non-data frames and non-spec_tbl_df objects
data_frames <- sapply(all_objects, function(x) {
  is.data.frame(get(x)) | inherits(get(x), "spec_tbl_df")
})

## Get the names of the data frames and spec_tbl_df objects
df_names <- all_objects[data_frames]

## Loop through the data frames and spec_tbl_df objects and set their column names to lower case
for (df_name in df_names) {
  df <- get(df_name)
  colnames(df) <- tolower(colnames(df))
  assign(df_name, df)
}

#clean up
rm(all_objects,
   data_frames,
   df_names,
   df_name,
   df)