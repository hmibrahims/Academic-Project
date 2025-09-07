# --- PERSONALIZED LANDSAT UNPACKING SCRIPT ---

# 1. SET YOUR WORKING DIRECTORY
# This is set to the path you provided. R uses forward slashes "/".
setwd("E:/Course&Class/SFRS")


# 2. GET A LIST OF YOUR LANDSAT FILES
# This pattern now looks for files ending in ".tar" based on your file list.
zip_files_list <- list.files(pattern = "\\.tar$")

# Check if any files were found
if (length(zip_files_list) == 0) {
  stop("No .tar files found. Please check that the files are in the 'E:/Course&Class/SFRS' directory.")
} else {
  print(paste("Found", length(zip_files_list), "files to process."))
}


# 3. LOOP THROUGH EACH FILE AND UNPACK IT
# This loop will run for every file found in the list.
for (zip in zip_files_list) {
  
  # Print which file is currently being processed
  cat("\nProcessing file:", zip, "\n")
  
  # Extract the base filename (removes the ".tar" part)
  filename <- substr(basename(zip), 1, nchar(basename(zip)) - 4)
  
  # --- This logic parses the filename to extract metadata ---
  year <- substr(filename, 18, 21)
  month <- substr(filename, 22, 23)
  day <- substr(filename, 24, 25)
  path <- substr(filename, 11, 13)
  row <- substr(filename, 14, 16)
  sat <- substr(filename, 1, 4)
  
  # Create a clean, descriptive name for the output folder
  outname <- paste0(year, "-", month, "-", day, "_", sat, "_", path, "_", row)
  
  # Unpack the .tar file into the new directory
  untar(zip, exdir = outname)
  
  # Print a success message for this file
  cat("Successfully unpacked to folder:", outname, "\n")
  
}
