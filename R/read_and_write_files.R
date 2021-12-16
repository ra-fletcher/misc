# Read and write files ----------------------------------------------------

# Pipe
`%>%` = magrittr::`%>%`

# Directories
read_dir = "Documents/tgi/projects/read_dir"
write_dir = "Documents/tgi/projects/write_dir"

# File names (example is for SAS files)
file_names = read_dir %>%
  list.files(full.names = FALSE) %>%
  str_remove_all("[.]sas7bdat$")

# File paths
file_paths = read_dir %>%
  list.files(full.names = TRUE)

# Function
read_and_write_files = function(x, y, output) {
  
  assign(y, haven::read_sas({{ x }}), envir = .GlobalEnv) %>%
    readr::write_rds(stringr::str_glue("{output}/{y}.rds"))
  
  # Remove objects from the Global Environment (to save memory)
  rm(list = y, envir = .GlobalEnv)
  gc()
}

# Loop for all files in the `read_dir`
purrr::walk2(
  file_paths,
  file_names,
  ~read_and_write_files(.x, .y, output = write_dir)
)
