generateAuthQuery = function(id, password) {


auth_query <- 'mutation {
 login(identifier: "ID", password: "PW") {
    accessToken
  }
}'

auth_query = sub("ID",id, auth_query)
auth_query = sub("PW",password, auth_query)




}