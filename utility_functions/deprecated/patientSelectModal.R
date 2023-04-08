patientSelectModal <- function(token) {
  pat = getPatients(token)
  patChoice = set_names(as.list(pat$medicalRecordNo), paste(pat$firstName, pat$lastName, pat$medicalRecordNo))    
  modalDialog(
    title = "Please select a patient",
    selectizeInput(inputId = "patientId", label = "Please select a patient", 
                   choices = c( patChoice) , selected = NULL, multiple = FALSE),
    textOutput("patient_error_msg"),
    
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("submitPatient", "Select")
    )
  )
}