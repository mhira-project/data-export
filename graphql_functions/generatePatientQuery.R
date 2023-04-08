# generatePatientFilter = function(id = NULL,  firstName = NULL , lastName = NULL,  medicalRecordNo = NULL){
#   
# }



generatePatientQuery = function(FLT){

GQL = 'query($paging: CursorPaging, $sorting: [PatientSort!]) {
    patients(paging: $paging, filter:{FLT}, sorting: $sorting) {
      edges {
        node {
          id
          statusId
          medicalRecordNo
          firstName
          middleName
          lastName
          phone
          phone2
          email
          addressStreet
          addressNumber
          addressApartment
          addressPlace
          addressPostalCode
          addressCountryCode
          gender
          birthDate
          createdAt
          updatedAt
          caseManagers {
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
            deletedAt
          }
        
        }
        cursor
      }
    }
  }'

if (missing(FLT)){FLT = ""}
GQL = sub("FLT",FLT, GQL)   

return(GQL)  
  
}
