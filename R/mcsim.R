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
makemcsim <- function(model, init = F){

  if (file.exist(model)==F){
   warning(paste("The", model, "not found!"))
  }

  dir.pkg <- find.package("simuloR")
  dir.file <- dirname(model)

  dir.sim <- system.file('sim', package = 'simuloR')
  file.copy(dir.sim, getwd(),
            overwrite = T, recursive = T, copy.mode = T)

  mStr <- strsplit(model, "/")
  mName <- mStr[[1]][length(mStr[[1]])]
  exe_file <- paste0(dir.file, "/mcsim.", mName)

  if(.Platform$OS.type == "windows"){
    mod <- "mod.dll"
    dir.mod <- paste0(dir.pkg, "/libs/x64")
    invisible(file.copy(from = paste0(dir.mod, "/",mod), to = paste0(getwd(), "/mod.exe")))
  } else {
    mod <- "mod.so"
    dir.mod <- paste0(dir.pkg, "/libs")
    invisible(file.copy(from = paste0(dir.mod, "/",mod), to = paste0(getwd(), "/",mod)))
  }

  if (init == T) {
    invisible(system(paste0("./", mod," -R ", model, " ", dir.file, "/",mName, ".c"), intern = T))
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

  if(.Platform$OS.type == "windows"){
    system(paste0("./mod.exe ", model, " ", dir.file, "/",mName, ".c"))
  } else system(paste0("./", mod," ", model, " ", dir.file, "/",mName, ".c"))

  message(paste0("* Creating executable program, pleas wait..."))
  #system(paste("gcc -O3 -I.. -I.", dir.sim, " -o ", dir.file, "/mcsim.", mName, " ", dir.file, "/", mName, ".c ", dir.sim, "/*.c -lm ", sep = ""))
  system(paste("gcc -O3 -I.. -I.", "/sim", " -o ", dir.file, "/mcsim.", mName, " ", dir.file, "/", mName, ".c ", "./sim", "/*.c -lm ", sep = ""))
  if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'."))

  file.o <- paste0(dir.file, "/", mName, ".o")
  if (file.exists(file.o)) invisible(file.remove(file.o))

  if(.Platform$OS.type == "windows"){
    invisible(file.remove(c(paste0(getwd(), "/mod.exe"),
                            paste0(dir.file, "/", mName, ".c"))))
  } else {
    invisible(file.remove(c(paste0(getwd(), "/",mod),
                            paste0(dir.file, "/", mName, ".c"))))
  }

  unlink("sim", recursive=TRUE)

  if (exists("x"))  return(x)
}

#' @export
mcsim <- function(model, input, parallel = F, compile = T){

  if (file.exist(model)==F){
    warning(paste("The", model, "not found!"))
  }
  if (file.exist(input)==F){
    warning(paste("The", input, "not found!"))
  }

  mStr <- strsplit(model, "/")
  mName <- mStr[[1]][length(mStr[[1]])]

  sub.folder <- paste(mStr[[1]][-length(mStr[[1]])],  sep = "/")

  tx  <- readLines(input)
  MCMC_line <- grep("MCMC \\(", x=tx)
  MonteCarlo_line <- grep("MonteCarlo \\(", x=tx)
  SetPoints_line <- grep("SetPoints \\(", x=tx)

  if (compile == T) {
    makemcsim(model = model)
    cat("\n")
    #system(paste0("models/mcsim.", mName, " ", input)) # need to consider other subfolder name
  }

  message(paste("Executing..."))
  if (length(MCMC_line) != 0){
    #file_defore <- list.files()
    RandomSeed <- exp(runif(1, min = 0, max = log(2147483646.0)))
    tx2 <- gsub(pattern = "10101010", replace = paste(RandomSeed), x = tx)
    checkfile <- "MCMC.check.out"

    if(file.exists(checkfile)){
      file.remove(checkfile)
    }

    if (parallel == T){
      i <- sample(1111:9999, 1)
      name <- gsub("\\..*", "", input)
      mcmc_input <- paste0(name, "_", i, ".in")
      mcmc_output <- paste0(name, "_", i, ".out")
      tx3 <- gsub(pattern = "MCMC.default.out", replace = mcmc_output, x = tx2)
      writeLines(tx3, con = mcmc_input)
      system(paste("./mcsim.", model, mcmc_input, sep = ""))

    } else{
      #tmp <- "tmp.mcmc.in"
      tmp <- tempfile()
      writeLines(tx2, con=tmp)
      system(paste0(sub.folder, "/mcsim.", mName, " ", tmp))
      outfile <- "MCMC.default.out"
      tx2 <- gsub(pattern = ",0,", replace = ",1,", x = tx)
      tx3 <- gsub(pattern = paste0("\"", outfile, "\",\"\""),
                  replace = paste0("\"", checkfile, "\",\"", outfile, "\""),
                  x = tx2)
      writeLines(tx3, con=tmp)

      system(paste0(sub.folder, "/mcsim.", mName, " ", tmp))
      file.remove(tmp)
    }

    if(file.exists(checkfile)){
      message(paste0("* Create '", checkfile, "' from the last iteration."))
    }

    if (parallel == T){
      dat <- read.delim(mcmc_output)
    } else {
      dat <- read.delim("MCMC.default.out")
    }

  } else if (length(MonteCarlo_line) != 0){
    #tmp <- "tmp.mtc.in"
    tmp <- tempfile()
    RandomSeed <- runif(1, 0, 2147483646)
    tx2 <- gsub(pattern = "10101010", replace = paste(RandomSeed), x = tx)
    writeLines(tx2, con=tmp)
    system(paste0(sub.folder, "/mcsim.", mName, " ", tmp))
    dat <- read.delim("simmc.out")
    file.remove(tmp)
  } else if (length(SetPoints_line) != 0){
    system(paste0(sub.folder, "/mcsim.", mName, " ", input))
    dat <- read.delim("simmc.out")
  } else {
    system(paste0(sub.folder, "/mcsim.", mName, " ", input))
    dat <- read.delim("sim.out", skip = 1)
  }


  #tryCatch(dat <- read.delim("sim.out"), error=function(e) NULL)
  #if(!exists("dat")) dat <- read.delim("sim.out", skip = 1)

  #cat("________________________________________\n\n")
  return(dat)
}

#' @export
switcheR <- function(){
  file <- rstudioapi::getSourceEditorContext()[['path']]
  fStr <- strsplit(file, "/")
  fName <- fStr[[1]][length(fStr[[1]])]
  mStr <- strsplit(fName, "\\.")

  extension <- mStr[[1]][length(mStr[[1]])]
  if(extension == "model" || extension == "in"){
    new_file <- paste0(file, ".r")
    file.rename(from = file, to = new_file)
  }
}

#' @export
mcmc_array <- function(data, start_sampling = 0){
  n_chains <- length(data)
  sample_number <- dim(data[[1]])[1] - start_sampling
  dim <- c(sample_number, n_chains, dim(data[[1]])[2])
  n_iter <- dim(data[[1]])[1]
  n_param <- dim(data[[1]])[2]
  x <- array(sample_number:(n_iter * n_chains * n_param), dim = dim)
  for (i in 1:n_chains) {
    x[, i, ] <- as.matrix(data[[i]][(start_sampling + 1):n_iter, ])
  }
  dimnames(x)[[3]] <- names(data[[1]])
  x
}

