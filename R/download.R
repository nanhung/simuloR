#' Compile \pkg{GNU MCSim} model file
#'
#' Download the latest or specific version of \pkg{GNU MCSim} from the official website
#' (\url{https://www.gnu.org/software/mcsim/}) and install it to the system directory.
#'
#' This function aims to help users install \pkg{GNU MCSim} more easily.
#' However, if you can not install it through this function.
#' The additional way is to follow the instruction and install it manually:
#' \url{https://www.gnu.org/software/mcsim/mcsim.html#Installation}
#'
#' The default \code{mxstp} is setting to 5000.
#' The user can increase \code{mxstp} to avoid possible error return.
#' If you meet any error when conduct sensitivity analysis,
#' you can this function to reinstall \pkg{GNU MCSim} and set the higher \code{mxstp}.
#' The default installed \code{directory} is under \code{/home/username} (Linux),
#' \code{/Users/username} (MacOS),
#' and \code{C:/Users/} (windows). To install \pkg{GNU MCSim} in Windows, be sure to install Rtools first.
#'
#' @references
#' Bois, F. Y., & Maszle, D. R. (1997).
#' MCSim: a Monte Carlo simulation program.
#' \emph{Journal of Statistical Software}, 2(9): 1–60.
#'
#' @rdname download
#'
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
