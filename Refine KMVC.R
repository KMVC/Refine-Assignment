#load relevant packages
#library(tidyverse)

#Step 0: load dataset
refine <- read.csv("refine_original.csv")


#Step 1: Clean up brand names
# use for loops to correct names in the "company" column

#1a for loop for philips
refine$company <- tolower(refine$company)
for (k in 1:25) {
  if(startsWith(refine$company[k], "ph")){
    refine$company[k] <- "philips"
  } 
}

#1b for loop for  akzo
for (k in 1:25) {
  if(startsWith(refine$company[k], "ak")){
    refine$company[k] <- "akzo"
  } 
}

#1c for loop for van houten
for (k in 1:25) {
  if(startsWith(refine$company[k], "van")){
    refine$company[k] <- "van houten"
  } 
}

#1d for loop for unilever
for (k in 1:25) {
  if(startsWith(refine$company[k], "uni")){
    refine$company[k] <- "unilever"
  } 
}

#use table function to check if all company names have been corrected
table(refine$company)

#1e for loop to correct last incorrect company name
for (k in 1:25) {
  if(startsWith(refine$company[k], "fi")){
    refine$company[k] <- "philips"
  } 
}

#Step 2: Separate product code and number, using separate function
refine<- separate(refine, "Product.code...number", c("product_code", "product_number"), sep = "-")

#Step 3: Add product categories, using mutate function
refine <- refine %>% 
  mutate("product category" = case_when(product_code == "p" ~ "Smartphone", product_code == "v" ~ "TV", 
                                        product_code == "x" ~ "Laptop", product_code == "q" ~ "Tablet"))

#Step 4: Add full address for geocoding, using unite function
refine <- unite(refine, "full_address", address, city, country, sep = "," )

#Step 5: Create dummy variables for company and product category, using mutate
refine <- refine %>% mutate("company_philips" = ifelse(company == "philips", 1,0))

refine <- refine %>% mutate("company_akzo" = ifelse(company == "akzo", 1,0))

refine <- refine %>% mutate("company_van_houten" = ifelse(company == "van houten", 1,0)) 

refine <- refine %>% mutate("company_unilever" = ifelse(company == "unilever", 1,0))

refine <- refine %>% mutate("product_smartphone" = ifelse(`product category` == "Smartphone", 1,0))

refine <- refine %>% mutate("product_tv" = ifelse(`product category` == "TV", 1,0))

refine <- refine %>% mutate("product_laptop" = ifelse(`product category` == "Laptop", 1,0))

refine <- refine %>% mutate("product_tablet" = ifelse(`product category` == "Tablet", 1,0))
View(refine)

#Save as refine_clean.csv
write.csv(refine, file = "refine_clean.csv")
