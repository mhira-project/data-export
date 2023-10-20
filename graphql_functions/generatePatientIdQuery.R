generatePatientIdQuery = function(paging_string = ""){
  GQL = paste0('query($sorting: [PatientSort!]) {
    patients(
      paging: { first: 50 ', paging_string, ' }, 
      sorting: $sorting
    ) {
      edges {
        node {
          id
          medicalRecordNo
          departments{
            name
          }
        }
        cursor
      }
      pageInfo {
        hasNextPage
      }
    }
  }')
  
  return(GQL)
}