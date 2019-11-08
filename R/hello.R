# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

makemcsim <- function(file){

  dir.pkg <- find.package("simuloR")
  dir.file <- dirname(file)
  dir.sim <- system.file('sim', package = 'simuloR')

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

  invisible(system(paste0("./", mod," -R ", file, " ", dir.file, "/",mName, ".c"), intern = T))
  system(paste0("R CMD SHLIB ", dir.file, "/", mName, ".c "))
  dyn.load(paste(dir.file, "/", mName, .Platform$dynlib.ext, sep=""))
  source(paste0(dir.file, "/", mName, "_inits.R"))

  initStates <- initStates()
  initParms <- initParms()

  system(paste0("./", mod," ", file, " ", dir.file, "/",mName, ".c"))
  message(paste0("* Creating executable program, pleas wait..."))
  system(paste("gcc -O3 -I.. -I.", dir.sim, " -o ", dir.file, "/mcsim.", mName, " ", dir.file, "/", mName, ".c ", dir.sim, "/*.c -lm ", sep = ""))
  if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'."))


  file.o <- paste0(dir.file, "/", mName, ".o")
  invisible(file.remove(c(paste0(getwd(), "/",mod),
                          paste0(dir.file, "/", mName, ".c"),
                          paste0(dir.file, "/", mName, .Platform$dynlib.ext),
                          paste0(dir.file, "/", mName, "_inits.R"))))
  if (file.exists(file.o)) invisible(file.remove(file.o))

  x <- list(initStates = initStates, initParms = initParms)
  return(x)
}

mods <- function(){
  system.file('models', package = 'simuloR')
}

