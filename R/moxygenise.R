# A function by Konrad Rudolph to create Rd files from source R with documentation.
# Usage: moxigenise("shiny", "man")
#
# The resulting Rd file can be viewed with
#  parse_Rd("man/get_metadata.Rd") %>% Rd2txt()

moxygenise <- function(codepath, manpath) {
  
  apply_at_level <- function(l, f, n, ...) {
    ## function to apply a function at specified level of a nested list
    if (n < 0) {
      stop("Invalid parameter - n should be integer >= 0 -- APPLY_AT_LEVEL")
    } else if (n==0) {
      return(l)
    } else if (n == 1) {
      return(lapply(l, f, ...))
    } else {
      return(lapply(l, function(x) {apply_at_level(x, f, n-1)}))
    }
  }
  
  list.files.paths <- function(path, pattern) {
    ## function to list absolute path of all files under specified path matching certain pattern
    path <- normalizePath(path)
    return(file.path(path, list.files(path=path, pattern=pattern)))
  }
  
  sourcefiles <- list.files.paths(codepath, "\\.R$")
  source_envs <- lapply(sourcefiles, roxygen2::env_file)
  rd_blockss <- mapply(roxygen2::parse_file, sourcefiles, source_envs)
  
  help_topicss <- mapply(function(rdblock, sourceenv, sourcefile) {
    return(roxygen2::roclet_process(
      roxygen2::rd_roclet(), 
      rdblock, sourceenv, 
      dirname(sourcefile)))},
    rd_blockss, source_envs, sourcefiles)
  
  rd_codes <- purrr::flatten(apply_at_level(help_topicss, format, 2))
  
  mapply(function(text, topic, outpath=manpath) {
    cat("Write", topic, "to", outpath, "\n")
    write(text, file=file.path(outpath, topic))
  }, rd_codes, names(rd_codes))
  return(NULL)
}