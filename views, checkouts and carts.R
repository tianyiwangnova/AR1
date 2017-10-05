setwd("C://angelaroi")
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
query.list <- Init(start.date = "2016-01-01",
                   end.date = "2017-09-25",
                   metrics = c("ga:productDetailViews","ga:productAddsToCart","ga:productCheckouts"),
                   dimensions = c("ga:date","ga:productName"),
                   max.results = 10000,
                   table.id = "ga:65357171"
                   )
# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query, token,paginate_query = T)

index=which(ga.data$productDetailViews==0)
ga.data=ga.data[-index,]

products=data.frame(unique(ga.data$productName))
write.csv(products,"product name list.csv")

product.names=c("Anne Shopper - Black","Anne Shopper - Russet","Cher Tote - Black","Cher Tote - Bordeaux",
                "Cher Tote - Cr¨¨me","Cher Tote - Light Gray","Eleanor Satchel - Black","Eleanor Satchel - Bordeaux","Grace Cross-body - Black",
                "Grace Cross-body - Navy","Grace Cross-body - Russet","Luna Cross-Body - Dark Green","Luna Cross-Body - Light Gray",
                "Luna Cross-Body - Pale Pink","Madeline AR Mini Bucket - Black","Madeline Mini Bucket - Bordeaux","Madeline Mini Bucket - Cloud",
                "Madison Backpack - Black","Madison Backpack - Dusty Rose","Madison Backpack - Mud Gray","Mia AR Mini Backpack - Black","Madeline Mini Bucket - Black",
                "Mia Mini Backpack - Black","Mia Mini Backpack - Cloud","Morning Cross-body -  Ivory","Morning Cross-body -  Mud Gray","Morning Cross-body -  Mustard",
                "Morning Cross-body - Black","Morning Cross-body - Bordeaux","Morning Cross-body - Cognac","Morning Cross-body - Dark Green",
                "Morning Cross-body - Dusty Rose","Morning Cross-body - Forest Green","Morning Cross-body - Powder Pink","Morning Cross-body - Scarlet",
                "Olivia E Wallet - Black","Olivia E Wallet - Bordeaux","Olivia E Wallet - Dusty Rose","Olivia E Wallet - Scarlet","Olivia Z Wallet - Black",
                "Olivia Z Wallet - Bordeaux","Olivia Z Wallet - Dusty Rose","Olivia Z Wallet - Scarlet","Sunday Mini - Black","Sunday Mini - Bordeaux","Sunday Mini - Cognac",
                "Sunday Mini - Dusty Rose","Sunday Mini - Light Gray","Sunday Mini - Mustard","Sunday Mini - Pink","Sunday Mini - Taupe","Sunday Tote Classic - Black",
                "Sunday Tote Classic - Light Gray","Sunday Tote Classic - Taupe","Jules Bucket Bag - Black","Morning Dark Green Cross-body","Morning Ivory Cross-body","Morning Mud Gray Cross-body",
                "Morning Mustard Cross-body","Sunday Tote II - Beige","Sunday Tote II - Black","Sunday Tote II - Light Gray","Elle Cosmetic Case Set - Black","Elle Cosmetic Case Set","Elle Cosmetic Case Set - Light Gray",
                "Elle Cosmetic Case Set - Plum","Sunday Tote II - Red Orang","Sunday Tote II - Dark Green","Anya Cross-body - Black","Anya Cross-body - Bordeaux","Anya Cross-body - Mud Gray","Sunday Mini - Black","Sunday Mini - Bordeaux",
                "Sunday Mini - Cognac","Sunday Mini - Dusty Rose","Sunday Mini - Light Gray","Sunday Mini - Mustard","Sunday Mini - Pink","Sunday Mini - Taupe","Jules Bucket Bag II - Light Gray","Jules Bucket Bag II - Bordeaux","Jules Bucket Bag II - Black"
)
name=rep("",nrow(ga.data))
match.names=function(x){
  for (j in 1:length(product.names)){
    match.result=""
    a=grep(product.names[j],ga.data$productName[i])
    if (length(a)!=0){match.result=product.names[j]} 
  }
  return(match.result)
  }
name=unlist()


for (i in 1:nrow()){
  for (j in 1:length(product.names)){
    a=grep(product.names[j],product.performance$productName[i])
    if (length(a)!=0){name[i]=product.names[j]}
  }
}
index1=which(name!="")