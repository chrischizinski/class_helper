check_pkgs <- function(){
  
  if (suppressPackageStartupMessages(!require("usethis"))){
    install.packages("usethis")
  }
  
  if (suppressPackageStartupMessages(!require("credentials"))){
    install.packages("credentials")
  }
  
  if (suppressPackageStartupMessages(!require("cli"))){
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



detect_token <- function() {
  
  token_test <- tryCatch(gitcreds::gitcreds_get(), error=function(err) NA)
  if(length(token_test) > 1){
    
    token_test <- ifelse(is.na(gitcreds::gitcreds_get()$password),0,1)
    
    
    }else{
      token_test <- 0
      
      }
  return(token_test)
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
    git_symb <- col_red(symbol$cross)
    if(os == "windows"){
      cli::cli_text("Follow the instructions for {.emph Option 1} at happygitwithR: {.url https://happygitwithr.com/install-git#install-git-windows}.")
    }
    if(os == "osx"){
      cli::cli_text("Follow the instructions for {.emph Option 1} at happygitwithR: {.url https://happygitwithr.com/install-git#macos}.")
    }
  }else if(!is.na(git_test)){
    dash_v <- trimws(gsub("[^[:digit:]. ]", "", git_test))
    cli::cli_alert_success("Git version {dash_v} installed on your system")
    git_symb <- col_green(symbol$tick)
  }
  
  ## Checking for git configuration
  cli::cli_h1("Checking your git configuration")
  
  git_config <- gert::git_config_global()
  
  
  if(!all(c("user.email","user.name") %in% git_config$name)){
    cli::cli_alert_danger("It appears that you do not have a user.name and user.email set up to interact with github.")
    cli::cli_text("Use  `{.emph usethis::use_git_config()}` to set your user.name and user.email.")
    user_symb <- col_red(symbol$cross)
    
  }else{
    cli::cli_alert_success("You have your git user.name and user.email set up")  
    user_symb <- col_green(symbol$tick)
  }
  
  ## Checking for valid tokens
  cli::cli_h1("Checking your tokens")
  
  git_tokenz <- detect_token()
  
  if(git_tokenz == 0){
    cli::cli_alert_danger("It appears that you do not have a current token set up")
    cli::cli_text("Use  `{.emph usethis::create_github_token()}` to help set up a token and then {.emph gitcreds::gitcreds_set()} to securely store your token.")
    
    token_symb <- col_red(symbol$cross)
    
  }else{
    cli::cli_alert_success("Your tokens are setup") 
    token_symb <- col_green(symbol$tick)
  }
  
  cli::cli_h2("Git check summary")
  cli_dl(c("Git installed" = "{git_symb}",
           "Username and email" = "{user_symb}",
           "Token" = "{token_symb}"))
  
}

