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

makemcsim <- function(model, deSolve = F, dir = "modeling"){

  pkgwd <- find.package("simuloR")

  exe_file <- paste0("mcsim.", model)
  #if(file.exists(exe_file)) stop(paste0("* '", exe_file, "' had been created."))

  if (deSolve == T){
    system(paste("./MCSim/mod.exe -R ", dir, "/", model, " ", model, ".c", sep = ""))
    system (paste0("R CMD SHLIB ", model, ".c")) # create *.dll files
    dyn.load(paste(model, .Platform$dynlib.ext, sep="")) # load *.dll
    source(paste0(model,"_inits.R"))
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
    system(paste0("./", mod," ", current.wd, "/", model, " ", current.wd, "/", model, ".c"))
    setwd(current.wd)

    simwd <- system.file('sim', package = 'simuloR')

    message(paste0("* Creating executable program, pleas wait..."))
    system(paste("gcc -O3 -I.. -I.", simwd, " -o mcsim.", model, " ", model, ".c ", simwd, "/*.c -lm ", sep = ""))

    invisible(file.remove(paste0(model, ".c")))
    if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'."))
  }
}


