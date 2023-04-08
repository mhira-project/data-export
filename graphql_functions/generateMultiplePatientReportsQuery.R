generateMultiplePatientReportsQuery = function(patientIds){

GQL = 'query {
  generateMultiplePatientReports (ids: patientIds, assessmentStatus: "COMPLETED"){						
     patient{
      medicalRecordNo

    }
    answeredQuestionnaires{
      assessmentId
      _id
      name
      questionnaireFullName  
      language
      questions{
        variable
        type
        required
        questionGrouplabel
        label
        choices{
          label
          name
        }
      answer{
        textValue
        numberValue
        dateValue
        multipleChoiceValue
        createdAt
        updatedAt     
        }
      
      }

    }
    assessments{
      assessmentId
      deliveryDate
      submissionDate
      assessmentType{
        name}
      status
      emailStatus
    }
    questionnaireScripts{
      id
      name
      questionnaireId
      questionnaireName
      questionnaireLanguage
      scriptText
      version
    }
  }
}'


IDs = paste("[", paste(patientIds, collapse = ","), "]", sep = "")
GQL = sub("patientIds", IDs, GQL)   

return(GQL)  
  
}

# GQL = 'query {
#   generateMultiplePatientReports (ids: patientIds){							# Please use UUID - can be additional field in database. 
#      patient{
#       medicalRecordNo
#       firstName
#       middleName
#       lastName
#       birthDate
#       gender
#     }
#     answeredQuestionnaires{
#       assessmentId
#       _id
#       name
#       questionnaireFullName  
#       language
#       questions{
#         variable
#         type
#         required
#         questionGrouplabel
#         label
#         choices{
#           label
#           name
#         }
#       answer{
#         textValue
#         numberValue
#         dateValue
#         multipleChoiceValue
#         createdAt
#         updatedAt     
#         }
#       
#       }
# 
#     }
#     assessments{
#       assessmentId
#       assessmentType{
#         name}
#       status
#     }
#     questionnaireScripts{
#       id
#       name
#       questionnaireId
#       questionnaireName
#       questionnaireLanguage
#       scriptText
#       version
#     }
#   }
# }'