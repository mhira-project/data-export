loginModal <- function() {
  modalDialog(
    textInput(inputId = "username", "MHIRA username",  value = "shiny"),
    passwordInput(inputId = "password", label = "MHIRA Password", value = "Password@1"),
    
    textOutput("login_error_msg"),
    
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("submitPW", "Login")
    )
  )
}