#Loading libraries
library(tidyr)
library(dplyr)
library(stringr)

#####Checkpoint 1: Data Cleaning 1#######################
#Load the companies and rounds data into two data frames-companies and rounds2 respectively.

companies <- read.delim("companies.txt", stringsAsFactors = FALSE)
companies$permalink <- tolower(companies$permalink)

rounds2 <- read.csv("rounds2.csv",header = T, stringsAsFactors = F)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# Unique companies are present in rounds2
length(unique(rounds2$company_permalink))

#Unique companies present in companies
length(unique(companies$permalink))

# Merge the companies and rounds2 data frame as master_frame.
master_frame <- merge(rounds2,companies, by.x = "company_permalink", by.y = "permalink")

# Numer of observations in master_frame
nrow(master_frame)

#### Checkpoint 2: Data Cleaning 2 #####

# NA values are present in the column raised_amount_usd
 number_of_na <- sum(is.na(master_frame$raised_amount_usd))

#Replacing NA values of raised_amount_usd with values not present in dataframe '-1'
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- -1

########Checkpoint 3: Funding Type Analysis###############

# Calcualting the Average investment amount for each of the four funding types 
## For Venture Type
venture_group <- filter(master_frame, master_frame$funding_round_type== 'venture')
## Average funding amount of venture type
summarise(venture_group, mean(venture_group$raised_amount_usd))

## For Angel Type
angel_group <- filter(master_frame,master_frame$funding_round_type == 'angel')
##  Average funding amount of angel type
summarise(angel_group, mean(angel_group$raised_amount_usd))

## For Seed Type
seed_group <- filter(master_frame, master_frame$funding_round_type == 'seed')
## Average funding amount of seed type
summarise(seed_group, mean(seed_group$raised_amount_usd))

## For Private Equity Type
private_equity_group <- filter(master_frame, master_frame$funding_round_type == 'private_equity')
##Average funding amount of private equity type
summarise(private_equity_group, mean(private_equity_group$raised_amount_usd))

## Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
# From the four investment type, Venture Type satisfy the condition of 5 to 15 million USD and so Venture Type is suitable for Spark Funds

#############Checkpoint 4: Country Analysis##################

## Applying group by for venture type investment based on country code
group_by_countries <- group_by(venture_group,country_code)
sort_country_usd <- summarise(group_by_countries,total_invest=sum(raised_amount_usd, na.rm = T))
sort_country_usd <- sort_country_usd[order(sort_country_usd$total_invest,decreasing = T),]

#Top nine countries based on the total investment amount each country has received
top9 <- sort_country_usd[1:9,]

#####################Checkpoint 5: Sector Analysis 1####################

# Loading mapping.csv file into mapping_file data frame
mapping_file <- read.csv("mapping.csv")

# Converting the wide data format of mapping_file to Long Format
main_sector_mapping_file <- gather(mapping_file,main_sector,my_val,Automotive...Sports: Social..Finance..Analytics..Advertising)
main_sector_mapping_file <- main_sector_mapping_file[!(main_sector_mapping_file$my_val==0),]
colnames(main_sector_mapping_file)[1] <- "primary_sector"
main_sector_mapping_file<- main_sector_mapping_file[,-3]

# splitting the category_list of masterframe 
new_master_frame <- master_frame
new_master_frame$category_list <- str_split_fixed(new_master_frame$category_list,"\\|",n=3)
colnames(new_master_frame)[9] <- "primary_sector"

# Creating new data frame with main_sector and primary_sector columns
merged_master_frame <- merge(main_sector_mapping_file, new_master_frame, by="primary_sector")


##################Checkpoint 6: Sector Analysis 2#################
## Dataframe D1 containing the observations for country USA and of funding type Venture falling within the 5-15 million USD range

# C1 is the datatframe for USA country with 5 to 15 million USD and venture type
C1 <- filter(merged_master_frame, country_code == "USA" & raised_amount_usd > 5000000 & raised_amount_usd < 15000000 & funding_round_type == "venture")

# Grouping C1 based on mainsector and obtaining sum of investments sector wise.
C1_investment_group_by <- group_by(C1, main_sector)
C1_total_investment <- summarise(C1_investment_group_by,total_invest=sum(raised_amount_usd, na.rm = T))

# Merging the C1 dataframe with C1_total_invvestment to obtain the sum of investments column
D1 <- merge(C1,C1_total_investment, by="main_sector")

# Count of investments based on grouping of main sector
C1_investments_count <- count(group_by(C1, main_sector))

# Datatframe D1 
D1 <- merge(D1, C1_investments_count, by="main_sector")

## Dataframe D2 containing the observations for country GBR and of funding type Venture falling within the 5-15 million USD range
# C2 is the datatframe for GBR country with 5 to 15 million USD and venture type
C2 <- filter(merged_master_frame, country_code == "GBR" & raised_amount_usd > 5000000 & raised_amount_usd < 15000000 & funding_round_type == "venture")
# Grouping C2 based on mainsector and obtaining sum of investments sector wise.
C2_investment_group_by <- group_by(C2, main_sector)
C2_total_investment  <- summarise(C2_investment_group_by, total=sum(raised_amount_usd, na.rm = T))

# Merging the C2 dataframe with C2_total_invvestment to obtain the sum of investments column
D2 <- merge(C2,C2_total_investment, by="main_sector")

# Count of investments based on grouping of main sector
C2_investments_count <- count(group_by(C2, main_sector))

# Datatframe D2
D2 <- merge(D2,C2_investments_count, by="main_sector")

## Dataframe D3 containing the observations for country IND and of funding type Venture falling within the 5-15 million USD range
# C3 is the datatframe for IND country with 5 to 15 million USD and venture type
C3 <- filter(merged_master_frame, country_code == "IND" & raised_amount_usd > 5000000 & raised_amount_usd < 15000000 & funding_round_type == "venture")

# Grouping C3 based on mainsector and obtaining sum of investments sector wise.
C3_investment_group_by <- group_by(C3, main_sector)
C3_total_investment <- summarise(C3_investment_group_by, total=sum(raised_amount_usd, na.rm = T))

# Merging the C3 dataframe with C3_total_invvestment to obtain the sum of investments column
 D3 <- merge(C3,C3_total_investment, by="main_sector")

# Count of investments based on grouping of main sector
C3_investments_count <- count(group_by(C3, main_sector))

# Datatframe D3
D3 <- merge(D3, C3_investments_count, by="main_sector")

# Changing the numer of investment column name in dataframes as "no_of_investments"
colnames(D1)[18]<- "Number_of_investments"
colnames(D2)[18]<- "Number_of_investments"
colnames(D3)[18]<- "Number_of_investments"

# Total Number of Investments for C1, C2, C3 countries (USA,GBR,IND)
sum(D1$Number_of_investments)
sum(D2$Number_of_investments)
sum(D3$Number_of_investments)

# Total Sum of Investments for C1, C2, C3 countries (USA,GBR,IND)
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

# Top sector (based on count of investments) for C1 - USA
top_sector_c1 <- arrange((distinct(D1,main_sector,Number_of_investments)),desc(Number_of_investments))
top_sector_c1[1:1,1:2]

# Top sector (based on count of investments) for C2 - GBR
top_sector_c2 <- arrange((distinct(D2,main_sector,Number_of_investments)),desc(Number_of_investments))
top_sector_c2[1:1,1:2]

# Top sector (based on count of investments) for C3 - IND
top_sector_c3 <- arrange((distinct(D3,main_sector,Number_of_investments)),desc(Number_of_investments))
top_sector_c3[1:1,1:2]

#Second-best sector (based on count of investments) for C1
second_sector_c1 <-top_sector_c1[2,1]

#Second-best sector (based on count of investments) for C2
second_sector_c2 <-top_sector_c2[2,1]

#Second-best sector (based on count of investments) for C3
second_sector_c3 <- top_sector_c3[2,1]

#Third-best sector (based on count of investments) for C1
third_sector_c1 <-top_sector_c1[3,1]

#Third-best sector (based on count of investments) for C2
third_sector_c2 <-top_sector_c2[3,1]

#Third-best sector (based on count of investments) for C3
third_sector_c3 <- top_sector_c3[3,1]

#Number of investments in the top sector for C1,C2,C3
top_sector_c1[1,]
top_sector_c2[1,]
top_sector_c3[1,]

# Number of investments in the second-best sector for C1,C2,C3
top_sector_c1[2,]
top_sector_c2[2,]
top_sector_c3[2,]


#Number of investments in the third-best sector for C1,C2,C3
top_sector_c1[3,]
top_sector_c2[3,]
top_sector_c3[3,]

#For the top sector count-wise,which company received the highest investment for C1
head(arrange(filter(D1, main_sector == "Others" ),desc(raised_amount_usd)),n=1L)[,3]

#For the top sector count-wise,which company received the highest investment for C2
head(arrange(filter(D2, main_sector == "Others" ),desc(raised_amount_usd)),n=1L)[,3]

#For the top sector count-wise,which company received the highest investment for C3
head(arrange(filter(D3, main_sector == "Others" ),desc(raised_amount_usd)),n=1L)[,3]

#For the second-best sector count-wise, which company received the highest investment for C1
head(arrange(filter(D1, main_sector == "Cleantech...Semiconductors" ),desc(raised_amount_usd)),n=1L)[,3]

#For the second-best sector count-wise, which company received the highest investment for C2
head(arrange(filter(D2, main_sector == "Cleantech...Semiconductors" ),desc(raised_amount_usd)),n=1L)[,3]

#For the second-best sector count-wise, which company received the highest investment for C3
head(arrange(filter(D3, main_sector == "News..Search.and.Messaging" ),desc(raised_amount_usd)),n=1L)[,3]










