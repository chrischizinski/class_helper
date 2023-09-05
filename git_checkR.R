check_pkgs <- function(){
  if (!require("usethis")){
    install.packages("usethis")
  }
  
  if (!require("cli")){
    install.packages("cli")
  }
  
  }


get_os <- function(){ # from conjugateprior.org
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
    } else if(os == 'Windows'){
      os <- "windows"
    }
    else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}







check_git_installation <- function(){
  check_pkgs() #check for packages
  os <- get_os()
  
  cli_div(theme = list(span.emph = list(color = "orange")))
  
  

  ## Checking for git installation
  cli_h1("Checking git installation")
  
  
  
  git_test <- tryCatch(system2("git", "--version", stdout=TRUE, stderr=TRUE), error=function(err) NA)
  
  if(is.na(git_test)){
    cli_alert_danger("Git is not installed on your system.") 
    if(os == "windows"){
      cli::cli_text("Follow the instructions for {.emph Option 1} at happygitwithR: {.url https://happygitwithr.com/install-git#install-git-windows}.")
    }
    if(os == "osx"){
      cli::cli_text("Follow the instructions for {.emph Option 1} at happygitwithR: {.url https://happygitwithr.com/install-git#macos}.")
    }
    
    
    }
  
  if(!is.na(git_test)){
    dash_v <- trimws(gsub("[^[:digit:]. ]", "", git_test))
    cli::cli_alert_success("Git version {dash_v} installed on your system")
  }
  
  ## Checking for git configuration
  cli::cli_h1("Checking your git configuration")
  
  git_config <- gert::git_config_global()
  
  if(length(git_config$name)< 2){
    cli::cli_alert_danger("It appears that you do not have a user.name and user.email set up to interact with github.")
    cli::cli_text("Use  `{.emph usethis::use_git_config()}` to set your user.name and user.email.")
    
  }else{
    cli::cli_alert_success("You have your git user.name and user.email set up")  
    }
  
}


check_git_installation()
