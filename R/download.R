#' @export
mcsim_download <- function() {

  version<-"6.1.0"
  URL <- sprintf('http://ftp.gnu.org/gnu/mcsim/mcsim-%s.tar.gz', version)
  tf <- tempfile()


  message(paste0("Downloading GNU MCSim " , sprintf('%s', version), " ..."))
  download.file(URL, tf, mode = "wb")

  utils::untar(tf, exdir = getwd())

  file <- paste0(getwd(), "/mcsim-6.1.0/sim/lsodes1.c")
  lsodes1.c <- readLines(file)
  new.mxstp0 <- paste0("mxstp0 = ", 5000)
  mxstp0 <- gsub("mxstp0 = 500", new.mxstp0, lsodes1.c)
  cat(mxstp0, file=file, sep="\n")

  cat("\n")
  if (file.exists("mcsim-6.1.0")){
    message(paste0("The GNU MCSim " , sprintf('%s', version), " has been successfully downloaded."))
  }
}
