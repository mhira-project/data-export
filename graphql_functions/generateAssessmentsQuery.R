
generateAssessmentsQuery = function(FLT){

GQL = 'query($paging: CursorPaging, $sorting: [AssessmentSort!]) {
    assessments(paging: $paging, filter: {FLT}, sorting: $sorting) {
      edges {
        cursor
        node {
          id
          date
          name
          patientId
          clinicianId
          status
          createdAt
          updatedAt
          deletedAt
          informant
        }
      }
    }
  }'

if (missing(FLT)){FLT = ""}
GQL = sub("FLT",FLT, GQL)   

return(GQL)  
  
}



# Could add ----
#clinician {
  #       id
  #       username
  #       active
  #       firstName
  #       middleName
  #       lastName
  #       email
  #       phone
  #       workID
  #       address
  #       gender
  #       birthDate
  #       nationality
  #       createdAt
  #       updatedAt
  #     }
  #     patient {
  #       id
  #       active
  #       medicalRecordNo
  #       firstName
  #       middleName
  #       lastName
  #       phone
  #       email
  #       address
  #       gender
  #       birthDate
  #       birthCountryCode
  #       nationality
  #       createdAt
  #       updatedAt
  #     }
  #   }
  # }
  # pageInfo {
  #   startCursor
  #   endCursor
  #   hasNextPage
  #   hasPreviousPage
  # }
