# Load {readxl}
library(readxl)

# Load IPIP data
ipip <- read.csv("ipip120.csv")

# Load key
key <- read_excel("./codebook.xlsx")

# Organize names
name <- key$Facet
name <- tolower(name)
name <- gsub("-", "_", name)
name <- gsub(" ", "_", name)
name <- paste0(
  gsub("([^A-Za-z])+",
       "",
       key$Key
  ), "_",
  name
)

# Initialize names
colnames(ipip)[11:ncol(ipip)] <- name

# Order names
ordered_names <- paste0(
  name[order(name)], "_", 1:4
)

# Order item descriptions
ordered_items <- key$Item[order(name)]

# Reorder IPIP items
ipip[,11:ncol(ipip)] <- ipip[,(order(name) + 10)]

# Assign final names
colnames(ipip)[11:ncol(ipip)] <- ordered_names

# Save IPIP data
save(ipip, file = "./ipip120.RData")

# Create simple codebook
simple <- cbind(
  ordered_names,
  ordered_items
)

# Save codebook
write.csv(
  simple, file = "simple_codebook.csv",
  row.names = FALSE
)

# Obtain extraversion only
extraversion <- ipip[
  , c(
    which(colnames(ipip) == c(
      "id", "sex", "age", "year", "country"
    )),
    grep("E_", colnames(ipip))
  )
]

# Save extraversion
save(
  extraversion,
  file = "ipip_extraversion.RData"
)

# Obtain frequencies of countries
frequencies <- table(extraversion$country)

# Obtain sample sizes greater than 300
greater <- frequencies[frequencies > 300]

# Create data lists
data_list <- lapply(seq_along(greater), function(i){
  
  # obtain data
  data <- extraversion[
    extraversion$country == names(greater)[i],
  ]
  
  # check if data is greater than 600 people
  if(nrow(data) > 600){
    
    new_size <- runif(1, min = 300, max = 600)
    
    data <- data[sample(1:nrow(data), round(new_size)),]
    
  }
  
  # Return data
  return(data)
  
  
})

count <- 0

# combine all datasets
for(i in seq_along(greater))
  for(j in seq_along(greater)){
    
    if(i < j){
      
      count <- count + 1
      
      extraversion <- rbind(
        data_list[[i]],
        data_list[[j]]
      )
      
      save(
        extraversion,
        file = paste0(
          "../",
          formatC(
            count, digits = 3,
            format = "d", flag = "0"
          ), ".RData"
        )
      )
      
      
    }
    
  }
