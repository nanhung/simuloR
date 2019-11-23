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
#' @param file a character of version number.
#' @param init a character to assign the installed directory.
#'
#' @rdname mcsim
#'
#' @export
makemcsim <- function(file, init = F){

  dir.pkg <- find.package("simuloR")
  dir.file <- dirname(file)

  dir.sim <- system.file('sim', package = 'simuloR')
  file.copy(dir.sim, getwd(),
            overwrite = T, recursive = T, copy.mode = T)

  mStr <- strsplit(file, "/")
  mName <- mStr[[1]][length(mStr[[1]])]
  exe_file <- paste0(dir.file, "/mcsim.", mName)

  if(.Platform$OS.type == "windows"){
    mod <- "mod.dll"
    dir.mod <- paste0(dir.pkg, "/libs/x64")
  } else {
    mod <- "mod.so"
    dir.mod <- paste0(dir.pkg, "/libs")
  }
  invisible(file.copy(from = paste0(dir.mod, "/",mod), to = paste0(getwd(), "/",mod)))

  if (init == T) {
    invisible(system(paste0("./", mod," -R ", file, " ", dir.file, "/",mName, ".c"), intern = T))
    system(paste0("R CMD SHLIB ", dir.file, "/", mName, ".c "))
    dyn.file <- paste0(dir.file, "/", mName, .Platform$dynlib.ext)
    if (file.exists(dyn.file)) dyn.load(dyn.file)
    R_file <- paste0(dir.file, "/", mName, "_inits.R")
    if (file.exists(R_file)) source(R_file)
    invisible(file.remove(paste0(dir.file, "/", mName, "_inits.R")))
    initStates <- initStates()
    initParms <- initParms()
    x <- list(initStates = initStates, initParms = initParms)
    invisible(file.remove(c(paste0(dir.file, "/", mName, ".c"),
                            paste0(dir.file, "/", mName, .Platform$dynlib.ext))))
  }

  system(paste0("./", mod," ", file, " ", dir.file, "/",mName, ".c"))
  message(paste0("* Creating executable program, pleas wait..."))
  #system(paste("gcc -O3 -I.. -I.", dir.sim, " -o ", dir.file, "/mcsim.", mName, " ", dir.file, "/", mName, ".c ", dir.sim, "/*.c -lm ", sep = ""))
  system(paste("gcc -O3 -I.. -I.", "/sim", " -o ", dir.file, "/mcsim.", mName, " ", dir.file, "/", mName, ".c ", "./sim", "/*.c -lm ", sep = ""))
  if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'."))

  file.o <- paste0(dir.file, "/", mName, ".o")
  if (file.exists(file.o)) invisible(file.remove(file.o))
  invisible(file.remove(c(paste0(getwd(), "/",mod),
                          paste0(dir.file, "/", mName, ".c"))))
  system(paste0("rm -r sim"))

  if (exists("x"))  return(x)
}

#' @export
mcsim <- function(model, input){

  mStr <- strsplit(model, "/")
  mName <- mStr[[1]][length(mStr[[1]])]
  makemcsim(model)

  cat("\n")
  message(paste("Executing..."))
  system(paste0("models/mcsim.", mName, " ", input))
  tryCatch(dat <- read.delim("sim.out"), error=function(e) NULL)
  if(!exists("dat")) dat <- read.delim("sim.out", skip = 1)

  cat("________________________________________\n\n")
  return(dat)
}



