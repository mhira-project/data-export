getToken = function(Username, Password, url){

source("graphql_functions/GQL2.R")
source("graphql_functions/generateAuthQuery.R")
#Username <- readline(prompt = "Enter MHIRA username: ")
#Password <- readline(prompt = "Enter MHIRA password: ")



# GraphQL get token ----
auth_query = generateAuthQuery(id = Username, password = Password)
res_auth <- GQL2(auth_query, .url = url)
res_auth <- fromJSON(res_auth)
token = res_auth$data$login$accessToken

return(token)

}

