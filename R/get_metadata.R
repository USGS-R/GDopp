#'@title get measurement details
#'@details a \code{GDopp} function for extracting sampling info from file.\cr 
#'@param deploy_nm a valid deployment name (string)
#'@param folder_nm the folder where the deployment files reside.
#'@return a list of measurement metadata
#'@keywords methods
#'@examples 
#'folder_nm <- system.file('extdata', package = 'GDopp') 
#'deploy_nm <- "ALQ102"
#'get_metadata(deploy_nm, folder_nm)
#'@export
get_metadata <- function(deploy_nm, folder_nm){
  
  metadata <- list('sample_rate' = c(), "nominal_velocity" = c(), "trans_matrix" = c(), "notes" = c())
  
  hdr_file <- paste0(deploy_nm,'.hdr')
  file <- file.path(folder_nm, hdr_file)
  con <- file(file, "r", blocking = FALSE)
  lines <- readLines(con, n = 11)
  metadata$sample_rate = peel_string(string = readLines(con, n = 1), as.numeric = T)
  metadata$nominal_velocity = peel_string(string = readLines(con, n = 1), as.numeric = T)
  
  lines <- readLines(con, n = 25)
  metadata$notes <- peel_string(string = readLines(con, n = 1), short = T)
  
  lines <- readLines(con, n = 59)
  metadata$trans_matrix <- get_matrix(readLines(con, n = 3))
  
  return(metadata)
}

peel_string <- function(string, as.numeric = TRUE, short = FALSE){
  
  last_val <- tail(strsplit(string, split='  ', fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]],1)
  if (short) return(last_val)
  choppy <- strsplit(last_val,split = ' ')[[1]]
  
  peel_out <- ifelse(length(choppy) > 2, choppy[2], choppy[1])
  if (as.numeric){
    peel_out <- as.numeric(peel_out)
  }
  return(peel_out)
}

get_matrix <- function(lines){
  return(matrix(c(peel(lines[1]),peel(lines[1]),peel(lines[1])), ncol = 3))
}

peel <- function(line){
  char_vec <- strsplit(peel_string(line, short = TRUE), ' ')
  cols <- as.numeric(char_vec[[1]][!char_vec[[1]]==''])
  return(cols)
}
