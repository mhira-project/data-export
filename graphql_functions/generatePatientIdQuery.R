# generatePatientFilter = function(id = NULL,  firstName = NULL , lastName = NULL,  medicalRecordNo = NULL){
#   
# }



generatePatientIdQuery = function(FLT){

GQL = 'query($paging: CursorPaging, $sorting: [PatientSort!]) {
    patients(paging: $paging, filter:{FLT}, sorting: $sorting) {
      edges {
        node {
          id
        }
        cursor
      }
    }
  }'

if (missing(FLT)){FLT = ""}
GQL = sub("FLT",FLT, GQL)   

return(GQL)  
  
}
