#' Creates an empty model file
#' 
#' @usage cogscimodel.skaleton(name = "nameWithoutSpaces")
#' @param name (optional) A string with the model name
#' @param predictfun (optional) Function name
#' @param environment (optional)
#' @param path (optional) A string, path where to create the file
#' @param force (optional, default \code{FALSE}), if \code{TRUE} an existing files named \code{name} is overwritten.
#' @param code_files
#' @export
model.skeleton <- function(name = "myNewModel", path = ".", force = FALSE) {    
  dir <- file.path(path)                                     
  if (file.exists(dir) && !force) {
    stop(gettextf("directory '%s' already exists", dir), domain = NA)
  }                                       
  .safe.dir.create(dir)          
  message("Creating model R file ...", domain = NA)
  description <- file.path(dir, paste0(name, ".R"))
  file.copy(from="template.R", to=description)

#   cat("#Model: ", name, "\n",
#       "Type: Package\n", "Title: What the 
# package does (short line)\n",                                        
#         "Version: 1.0\n", "Date: ", format(Sys.time(), format = "%Y-%m-%d"),                                                              
#         "\n", "Author: Who wrote it\n", "Maintainer: Who to complain 
# to <yourfault@somewhere.net>\n",                                     
#         "Description: More about what it does (maybe more than one line)\n",                                                              
#         "License: What license is it under?\n", if (usingS4)         
#             "Depends: methods\n", if (nzchar(encoding) && encoding !=                                                                     
#             "unknown")                                               
#             paste0("Encoding: ", encoding, "\n"),
#       file = description,               
#       sep = "")                                                    
#     close(description)
}