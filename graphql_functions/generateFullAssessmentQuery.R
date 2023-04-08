
generateFullAssessmentQuery = function(ID){

GQL = ' query {
    getFullAssessment(id: ID) {
      id
      date
      name
      status
      createdAt
      updatedAt
      deletedAt
      informant
      patientId
      clinicianId
      questionnaireAssessmentId
      questionnaireAssessment {
        _id
        status
        answers {
          question
          textValue
          multipleChoiceValue
          numberValue
          dateValue
          booleanValue
        }
        questionnaires(populate: true) {
          _id
          name
          status
          createdAt
          keywords
          copyright
          website
          license
          timeToComplete
          questionnaire {
            language
            abbreviation
          }
          questionGroups {
            questions {
              _id
              name
              label
              type
              hint
              relevant
              calculation
              constraint
              constraintMessage
              min
              max
              required
              requiredMessage
              image
              appearance
              default
              choices {
                name
                label
                image
              }
            }
          }
        }
      }
      clinician {
        id
        username
        active
        firstName
        middleName
        lastName
        email
        phone
        workID
        address
        gender
        birthDate
        nationality
        createdAt
        updatedAt
      }
      patient {
        id
        active
        medicalRecordNo
        firstName
        middleName
        lastName
        phone
        email
        address
        gender
        birthDate
        birthCountryCode
        nationality
        createdAt
        updatedAt
      }
    }
  }'


GQL = sub("ID",ID, GQL)   

return(GQL)  
  
}