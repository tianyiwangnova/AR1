require(RGoogleAnalytics)
token <- Auth("524840298457-eo9fd54chjaruua3a71mp8sld7b572ml.apps.googleusercontent.com","IJ5igRIAgvyWI3vNPNEOIlyO")
# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created
# and saved
save(token,file="./token_file")
# Save the token object for future sessions
ValidateToken(token)
# In future sessions it can be loaded by running load("./token_file")
# Build a list of all the Query Parameters
query.list <- Init(start.date = "2017-10-03",
                   end.date = "2017-10-03",
                   metrics = c("ga:pageViews","ga:pageValue"),
                   dimensions = c("ga:date","ga:pagePath","ga:source"),
                   sort="ga:date",
                   max.results = 10000,
                   table.id = "ga:65357171")

# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query, token)

index=NULL
for (i in 1:nrow(ga.data)){
  j1=grep("checkout",ga.data$pagePath[i])
  j2=grep("cart",ga.data$pagePath[i])
  if(length(j1)!=0|length(j2)!=0){index=c(index,i)}
}
ga.data=ga.data[-index,]

write.csv(ga.data,"2017_10_03.csv",row.names = FALSE)
