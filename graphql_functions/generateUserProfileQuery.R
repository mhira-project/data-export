generateUserProfileQuery = function(){
  
  GQL = 'query{
  getUserProfile{
    username
    firstName
    lastName
    workID
    roles{
      name
    }    
  }  
}'
  
  return(GQL)  
  
}
