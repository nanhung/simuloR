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

makemcsim <- function(file, deSolve = F, dir = "modeling"){

  pkgwd <- find.package("simuloR")

  mStr <- strsplit(file, "/")
  mName <- mStr[[1]][length(mStr[[1]])]
  exe_file <- paste0("mcsim.", mName)
  #if(file.exists(exe_file)) stop(paste0("* '", exe_file, "' had been created."))

  if (deSolve == T){
    system(paste("./MCSim/mod.exe -R ", dir, "/", mName, " ", mName, ".c", sep = ""))
    system (paste0("R CMD SHLIB ", mName, ".c")) # create *.dll files
    dyn.load(paste(mName, .Platform$dynlib.ext, sep="")) # load *.dll
    source(paste0(mName,"_inits.R"))
  } else {

    if(.Platform$OS.type == "windows"){
      mod <- "mod.dll"
      mod.wd <- paste0(pkgwd, "/libs/x64")
    } else {
      mod <- "mod.so"
      mod.wd <- paste0(pkgwd, "/libs")
    }

    current.wd <- getwd()
    setwd(mod.wd)
    system(paste0("./", mod," ", file, " ", mName, ".c"))

    simwd <- system.file('sim', package = 'simuloR')

    message(paste0("* Creating executable program, pleas wait..."))
    system(paste("gcc -O3 -I.. -I.", simwd, " -o mcsim.", mName, " ", mName, ".c ", simwd, "/*.c -lm ", sep = ""))

    invisible(file.remove(paste0(mName, ".c")))
    if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "' at ", current.wd, "."))
    setwd(current.wd)
  }
}

mods <- function(){
  system.file('models', package = 'simuloR')
}

