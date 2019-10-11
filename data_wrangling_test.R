# NAME: Fahimeh Baftizadeh
# DATE: 10/04/2019

##########################################################################################
####### ---- Task Cleaning and analyzing the "Real Property Taxes" Dataset ---- ##########
##########################################################################################
# Loading some useful libraries

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

##########################################################################################
############## 1. Read the Property Tax data into a variable called tax ##################
##########################################################################################
# Download the "real property taxes" dataset from the website (via OpenBaltimore):
# http://sisbid.github.io/Module1/data/Real_Property_Taxes.csv.gz

tax <- read.csv("~/Downloads/Real_Property_Taxes.csv.gz")
summary(tax)

##########################################################################################
#################### 2. How many addresses pay property taxes?  ##########################
##########################################################################################
# If we assume that property tax is the sum of State tax and City tax, then  the follwoing 
# is the answer of this question:

# First we obtain columns that have tax variable. 
colnames(tax)[grepl("tax", colnames(tax), ignore.case = TRUE)]

# Then we print these columns 
tax$CityTax
tax$StateTax

# Then we remove all the punctuation[such as $ and ,] from the numbers 
tax$CityTax <- as.numeric(gsub("[\\$,]", "", tax$CityTax))
tax$StateTax <- as.numeric(gsub("[\\$,]", "", tax$StateTax))

# Total number of addresses that pay City tax:
print(sum(!is.na(tax$CityTax)))
# Answer: 219178

# Total number of addresses that pay State tax:
print(sum(!is.na(tax$StateTax)))
# Answer: 219378

# If proprty tax is not related to state tax and city tax, then the above solution is not 
# correct. 

##########################################################################################
############# 3. What is the total dollar amount of city and state tax paid? #############
##########################################################################################

print(sum(tax$CityTax,na.rm = TRUE) + sum(tax$StateTax,na.rm = TRUE))
# Answer: 903619608

##########################################################################################
######### 4. Convert the 'lotSize' variable to a numeric square feet variable. ###########
##########################################################################################
# There are multiple patterns that must be extracted from the data to be  able to compute 
# the lot size in square feet. Therefore we define the following patterns:

# A. Pattern to find (xdim X ydim) for the lotsize
x_times_y_pattern <- "[^0-9]*([0-9|-]+X[0-9|-]+)[^0-9]*.*"

# B. Pattern to find the decimal and integer numbers  
numbers_pattern <- "[^0-9_.]*([.|0-9]+)[^0-9]*.*"

# C. Pattern to find non-numbers indicating square feet
sqft_indicators <- c("SQ FT", "SQFT", "CU FT", "S\\.F\\.", "S\\.F", "S\\. F\\.", 
                    "SQ\\. FT\\.", "  SQ\\.FT\\.","SF|SF\\.", "CUBIC F", "CU  FT",
                    "SQ\\.FT\\.")
SQFT_pattern <- paste0(sqft_indicators, collapse = "|")

# D. Pattern to find O-147 ACRES typos (i.e. instead of O-147 we should have 0.147) 
typos_pattern <- "^(O-[0-9]+)[ACRES|AC]*.*"

# E. Pattern to find acres
acres_pattern <- "ACRES"

# For example run the following line:
gsub(x_times_y_pattern , "\\1", "BLA BLA 10X90 BLA BLA")
gsub(numbers_pattern, "\\1", "BLA BLA 0.9 BLA BLA")
gsub(numbers_pattern, "\\1", "BLA BLA .9BLA BLA ")
gsub(numbers_pattern, "\\1", "BLA BLA 10BLA BLA ")
gsub(typos_pattern, "\\1","O-67 ACRES")
gsub(SQFT_pattern, "\\1", "1000,0 SQFT")

tax$LotSize <- as.character(tax$LotSize)

##########################################
# There are 49 rows with some typos, look below (i.e. we change O- to 0.):
tax$LotSize[grepl(typos_pattern,tax$LotSize)]

# We fix these typos first:
tax$LotSize <- ifelse(test = grepl(typos_pattern, tax$LotSize), 
                      yes = gsub("O-","0.",tax$LotSize), 
                      no = tax$LotSize)


##########################################
# There are some lotsize values that are very large and we are not sure what unit they have
# We are going to set them as NA as they are ambiguous. Look below:
tax[!is.na(as.numeric(as.character(tax$LotSize))) & 
        as.numeric(as.character(tax$LotSize)) > 1, "LotSize"]

tax[!is.na(as.numeric(as.character(tax$LotSize))) & 
        as.numeric(as.character(tax$LotSize)) > 1, "LotSize"] <- NA

##########################################
# We will introduce a new column based on some pattern matching. If we have x_times_y_pattern
# Then the value will be stored in xy_dim column. Otherwise xy_dim will be NA. Then we will
# split the inches and feets and they will be stored in lotx_feet and lotx_inches and ....

tax <- tax %>% 
    mutate(xy_dim = ifelse(test = grepl(x_times_y_pattern, LotSize), 
                     yes = gsub(x_times_y_pattern, "\\1", LotSize), 
                     no = NA)) %>%
    mutate(splits = ifelse(!is.na(xy_dim), strsplit(xy_dim, split = "X"), NA)) %>%
    rowwise() %>% 
    mutate(lotx = splits[1],loty = splits[2]) %>%
    mutate(splits = ifelse(!is.na(lotx), strsplit(lotx, split = "-"), NA)) %>%
    mutate(lotx_feet = splits[1],lotx_inches = splits[2]) %>%
    mutate(splits = ifelse(!is.na(loty), strsplit(loty, split = "-"), NA)) %>%
    mutate(loty_feet = splits[1], loty_inches = splits[2]) 

# This is the result:
head(tax[,c("LotSize", "xy_dim", "lotx_feet", "lotx_inches", "loty_feet", "loty_inches")])

##########################################
# Some of the properties have the lotsize in square feet already. For them we just need
# to extract the value from the lotsize column. However we should look for some patterns
# We extract those values and put them in a column called LotSize_SQFT

tax <- tax %>% 
    mutate(LotSize_SQFT = ifelse(test = grepl(SQFT_pattern, LotSize) & is.na(xy_dim),
                                     yes = gsub(numbers_pattern,"\\1", LotSize), 
                                     no = NA))
# This is the results:
tax[grepl(SQFT_pattern, tax$LotSize), c("LotSize", "LotSize_SQFT") ] %>%
    arrange(desc(LotSize_SQFT)) %>% head()

# We can not have very small LotSize_SQFT, therefore the following rows are set to NA
tax[grepl(SQFT_pattern, tax$LotSize) & grepl("^0", tax$LotSize_SQFT), "LotSize_SQFT"] <- NA

##########################################
# Some of the properties have the lotsize in acres. For them we just need
# to extract the value from the lotsize column and multiply it by 43560 to
# get the value in square feet

tax <- as.data.frame(tax)
selected.cols <- c("lotx", "lotx_feet", "lotx_inches", "loty", "loty_feet", 
                   "loty_inches", "LotSize_SQFT")
tax[, selected.cols] <- sapply(tax[, selected.cols],as.numeric)

tax <- tax %>% 
    mutate(acres_LotSize = ifelse(test = grepl(acres_pattern, LotSize),
                                 yes = gsub(numbers_pattern,"\\1", LotSize), 
                                 no = NA)) %>%
    mutate(acres_LotSize = ifelse(test = !grepl("^0[0-9]+",acres_LotSize), 
                                  yes = as.numeric(acres_LotSize), 
                                  no = NA)) %>%
    mutate(LotSize_SQFT = ifelse(test = !is.na(acres_LotSize),
           yes = acres_LotSize * 43560, 
           no = LotSize_SQFT )) %>%
    select(c(colnames(tax),LotSize_SQFT))

# This is the results:
head(tax[!grepl(SQFT_pattern, tax$LotSize) & is.na(tax$xy_dim), c("LotSize", "LotSize_SQFT")])

##########################################
# Finally we are going to convert inches to feet and then compute the lotsize 
# for the remaining rows by multiplying lotx and loty 

tax <- tax %>%
    mutate(lotx = case_when(
    !is.na(lotx_feet) & !is.na(lotx_inches) ~  lotx_feet + lotx_inches * 0.0833333,
    !is.na(lotx_feet) & is.na(lotx_inches) ~  lotx_feet)) %>%
    mutate(loty = case_when(
        !is.na(loty_feet) & !is.na(loty_inches) ~  loty_feet + loty_inches * 0.0833333,
        !is.na(loty_feet) & is.na(loty_inches) ~  loty_feet)) %>%
    mutate(LotSize_SQFT = ifelse(test = is.na(LotSize_SQFT) & !is.na(lotx) & !is.na(loty), 
                                 yes = lotx * loty,
                                 no = LotSize_SQFT))
#This is the results:
head(tax[grepl(x_times_y_pattern, tax$LotSize), c("LotSize", "LotSize_SQFT")])


##########################################################################################
################################# Checking the results: ##################################
##########################################################################################
# All the square feet values of lot size are stored in LotSize_SQFT 
head(tax[,c("PropertyID", "LotSize", "LotSize_SQFT")])


# If the lotsizes are correct then we expect that City tax increase with the lotsize
p <- ggplot() + 
    geom_point(aes(x = log(tax$CityTax), y = log(tax$LotSize_SQFT))) +
    xlab("log(City Tax (USD))") + ylab("log(LotSize (SQFT))")

ggsave(filename = "~/Downloads/LotsizeSQFT_vs_Citytax.png", plot = p)
save(tax, file = "~/Downloads/tax_LotSizeSQFT.rda")

#There are some homes that have a very small city tax, maybe the units 
# in city tax for small numbers is not in dollars:
tax %>%
    filter(log(CityTax)<2) %>%
    filter(!is.na(LotSize_SQFT)) %>%
    select(PropertyID, LotSize, LotSize_SQFT, CityTax)

# There are some homes that have a large city tax but small lot size
tax %>%
    filter(log(LotSize_SQFT)<2) %>%
    filter(!is.na(CityTax)) %>%
    select(PropertyID, LotSize, LotSize_SQFT, CityTax)

# There are couple of homes such as property ID == 1375 002, that we need 
# to go back and fix, probably two commas were not allowed in the patterns
# and some other small mistakes that I can fix later if I had more time for this test
# Thanks