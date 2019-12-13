if (Sys.info()[['sysname']] == "Windows") {
  if(Sys.which("gcc") == ""){ # echo $PATH
    PATH = "c:/Rtools/mingw_32/bin"
    Sys.setenv(PATH = paste(PATH, Sys.getenv("PATH"), sep=";"))
  } # PATH=$PATH:/c/Rtools/mingw_32/bin; export PATH
} # PATH=$PATH:/c/MinGW/msys/1.0/local/bin

.onAttach <- function(...){
  packageStartupMessage("The package is powered by GNU MCSim v6.1.0.")
}
