

jsonlite::fromJSON("https://fantasy.premierleague.com/api/entry/6238967/transfers")

jsonlite::fromJSON("https://fantasy.premierleague.com/api/my-team/6238967/")
fromJSON()



key <- "ja11g14@soton.ac.uk"
secret <- "vonfoj-tyhby9-vyrrUf"

df <- list(
  password = secret,
  login = key,
  redirect_uri = 'https://fantasy.premierleague.com/a/login',
  app = 'plfpl-web'
)

req <- httr::GET(url = link,
                 body =  jsonlite::toJSON(df, pretty = T, auto_unbox = T)
                 )

content(req)




# import requests
# session = requests.session()
#
# url = 'https://users.premierleague.com/accounts/login/'
# payload = {
#   'password': 'pass',
#   'login': 'email',
#   'redirect_uri': 'https://fantasy.premierleague.com/a/login',
#   'app': 'plfpl-web'
# }
# session.post(url, data=payload)
#
# response = session.get('https://fantasy.premierleague.com/api/my-team/965197')
#
#
# print (response.json())
