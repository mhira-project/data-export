generatePatientReportQuery = function(patientId){

GQL = 'query {
  generatePatientReport (id: patientId){							# Please use UUID - can be additional field in database. 
     patient{
      medicalRecordNo
      firstName
      middleName
      lastName
      birthDate
      gender
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
      assessmentType{
        name}
      status
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


GQL = sub("patientId", patientId, GQL)   

return(GQL)  
  
}