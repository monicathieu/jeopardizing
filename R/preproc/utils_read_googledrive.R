# Basically downloads it silently, returns destination path for targets to track file
get_googledrive_csv <- function (drive_id, destination_path) {
  googledrive::drive_download(googledrive::as_id(drive_id),
                              path = destination_path,
                              type = "csv",
                              overwrite = TRUE)
  
  return (destination_path)
}
