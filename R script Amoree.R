install.packages("tidyverse")
library("tidyverse")
library(magrittr)
library(readxl)
library(dplyr) 
library(tibble)  



                        ### 2021
                        ### Import 2021 data from excel file


Amoree2021 <- read_excel("Data analysys/Project Amoree Gifting/Amoree Gifting Sales.xlsx",sheet = "2021")
head(Amoree2021)


Amoree2021 <- data.frame(Amoree2021)

colnames(Amoree2021)



                          ### Rename columns

Amoree2021_v2 <- Amoree2021 %>% 
  rename(Sl_No=Sl.No.,Customer=Client.Name.,Price=Sale.Price...,Date=Delivery.Date.,
         Expense=Cost.Incurred..., Profit=Profit.Amount....) 

Amoree2021_v2 <- add_column(Amoree2021_v2, Category=NA)
head(Amoree2021_v2)

                          ### Categorize the Products based on text identified in Product column

for (i in 1:nrow(Amoree2021_v2)) 
{
  if(str_detect(Amoree2021_v2[i,3], ("Birthday")) == TRUE)
  {
    Amoree2021_v2[i,"Category"]="Birthday"
    
  }
  else
    
    if(str_detect(Amoree2021_v2[i,3],("Monogram")) == TRUE)
    {
      
      Amoree2021_v2[i,"Category"]="Monogram"
    }
  
  else
    
    if(str_detect(Amoree2021_v2[i,3],("Frame")) == TRUE)
    {
      
      Amoree2021_v2[i,"Category"]="Frames"
      
    }
  
  else
    
  {
    Amoree2021_v2[i,"Category"]="Others"
  }
  
}



for(i in 1:nrow(Amoree2021_v2))
if((str_detect(Amoree2021_v2[i,3],("Liquor"))) | (str_detect(Amoree2021_v2[i,3],("Men"))) | (str_detect(Amoree2021_v2[i,3],("Alcohol")))  == TRUE)
{
  
  Amoree2021_v2[i,"Category"]="Men's hampers"
  
}

                      ### Categorize the Products based on Quantity

for(i in 1:nrow(Amoree2021_v2))
{
  if(Amoree2021_v2[i,6]>5)
    Amoree2021_v2[i,"Category"]="Return gifts/Bulk)"
  
}



glimpse(Amoree2021_v2)


                    ### Convert price, expense to numeric data type from char
                    ### Convert Date in char to Date in Date type, split into month and year column



Amoree2021_v2 <- transform(Amoree2021_v2,Price = as.numeric(Price),Expense = as.numeric(Expense),Date=dmy(Date))

Amoree2021_v2 <- Amoree2021_v2 %>% mutate(Month=month(Date)) %>%
  mutate(Year=year(Date)) %>% arrange(Month)


head(Amoree2021_v2)





                    ### Profit analysis

Profit_analysis_2021 <- Amoree2021_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% mutate(Profit_per_item=Profit/Qty) %>% 
  group_by(Category) %>% 
  summarise(Avg_profit_peritem=mean(Profit_per_item, na.rm = TRUE))
View(Profit_analysis_2021)

Revenue_per_category_2021 <- Amoree2021_v2 %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
  summarise(Revenue_per_category=sum(Price,na.rm=TRUE),
            Total_qty_per_category=sum(Qty,na.rm=TRUE))

Profit_per_category_2021 <- Amoree2021_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
  summarise(Profit_per_category=sum(Profit,na.rm=TRUE))


                   ###### Final Profit analysis

Profit_percentage_per_category_2021 <- Profit_per_category_2021 %>%
  mutate(Revenue_per_category=Revenue_per_category_2021$Revenue_per_category,
         Qty=Revenue_per_category_2021$Total_qty_per_category) %>% 
  mutate(Avg_price_per_category=Revenue_per_category/Qty,
         Profit_percent_per_category=(Profit_per_category/Revenue_per_category)*100)

View(Profit_percentage_per_category_2021)

colnames(Profit_percentage_per_category_2021)



                          ### Trends plots



ggplot(Amoree2021_v2)+ geom_bar(mapping=aes(x=Category))+ 
  labs(title="Number of orders by Category 2021") 

ggplot(Rev_2021,aes(x=Month,y=Revenue)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=seq(0,12, by=1)) +
  labs(title = "Revenue by months 2021",y="Revenue")

ggplot(Amoree2021_v2,aes(x=Month,y=Profit,fill=Category)) + 
  geom_col() + scale_x_continuous(breaks=seq(0,12.5,by=1)) +
  labs(title = "Profit by months 2021")

ggplot(Amoree2021_v2)+ geom_col(mapping=aes(x=Category,y=Price),fill="darkblue")+
  labs(title="Revenue per Category 2021",y="Revenue") 

ggplot(Amoree2021_v2)+ geom_col(mapping=aes(x=Category,y=Profit),fill="blue") +
  labs(title="Profit per Category 2021")








                        ### 2022
                        ### Import 2022 data from excel file


Amoree2022 <- read_excel("Data analysys/Project Amoree Gifting/Amoree Gifting Sales.xlsx",sheet = "2022")
head(Amoree2022)


Amoree2022 <- data.frame(Amoree2022)
colnames(Amoree2022)


                        ### Rename the column name for convenience

Amoree2022_v2 <- Amoree2022 %>% 
  rename(Sl_No=Sl.No.,Customer=Client.Name.,Price=Sale.Split....,Date=Delivery.Date.,
         Expense=Cost.Incurred...., Profit=Profit.Amount.....,Payment_details=Payment.details) 


                       ### Add new empty column to categorize

Amoree2022_v2 <- add_column(Amoree2022_v2, Category=NA)
head(Amoree2022_v2)

                        ### Categorize the Products based on text identified in Product column

for (i in 1:nrow(Amoree2022_v2)) 
{
    if(str_detect(Amoree2022_v2[i,3], ("Birthday")) == TRUE)
    {
      Amoree2022_v2[i,"Category"]="Birthday"
      
    }
  else
    
    if(str_detect(Amoree2022_v2[i,3],("Monogram")) == TRUE)
    {
  
  Amoree2022_v2[i,"Category"]="Monogram"
}

else
  
  if(str_detect(Amoree2022_v2[i,3],("Frame")) == TRUE)
  {
    
    Amoree2022_v2[i,"Category"]="Frames"

  }
else
  
  Amoree2022_v2[i,"Category"]="Others"
  
}



for(i in 1:nrow(Amoree2022_v2))
  if((str_detect(Amoree2022_v2[i,3],("Liquor"))) | (str_detect(Amoree2022_v2[i,3],("Men"))) | (str_detect(Amoree2022_v2[i,3],("Alcohol")))  == TRUE)
  {
    
    Amoree2022_v2[i,"Category"]="Men's hampers"
    
  }

                        ### Categorize the Products based on Quantity

for(i in 1:nrow(Amoree2022_v2))
{
  if(Amoree2022_v2[i,6]>5)
    Amoree2022_v2[i,"Category"]="Return gifts/Bulk"
    
}
      



                        ### Convert price, expense to numeric data type from char
                        ### Convert Date in char to Date in Date type, split into month and year column
                        


Amoree2022_v2 <- transform(Amoree2022_v2,Price = as.numeric(Price),Expense = as.numeric(Expense),Date=dmy(Date))

Amoree2022_v2 <- Amoree2022_v2 %>% mutate(Month=month(Date)) %>%
                                        mutate(Year=year(Date)) %>% arrange(Month)


View(Amoree2022_v2)



                            ### Summarize revenue cost profit


Rev_2022 <- Amoree2022_v2 %>% group_by(Month) %>%  summarise(Revenue = sum(Price,na.rm=TRUE),
                                   Cost = sum(Expense,na.rm=TRUE),
                                   Profit = sum(Profit,na.rm=TRUE))




                          ### Profit analysis

Profit_analysis_2022 <- Amoree2022_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% mutate(Profit_per_item=Profit/Qty) %>% 
  group_by(Category) %>% 
  summarise(Avg_profit_peritem=mean(Profit_per_item, na.rm = TRUE))
View(Profit_analysis_2022)

Revenue_per_category_2022 <- Amoree2022_v2 %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
summarise(Revenue_per_category=sum(Price,na.rm=TRUE),
          Total_qty_per_category=sum(Qty,na.rm=TRUE))

Profit_per_category_2022 <- Amoree2022_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
  summarise(Profit_per_category=sum(Profit,na.rm=TRUE))


                   ###### Final Profit analysis

Profit_percentage_per_category_2022 <- Profit_per_category_2022 %>%
  mutate(Revenue_per_category=Revenue_per_category_2022$Revenue_per_category,
        Qty=Revenue_per_category_2022$Total_qty_per_category) %>% 
                mutate(Avg_price_per_category=Revenue_per_category/Qty,
                        Profit_percent_per_category=(Profit_per_category/Revenue_per_category)*100)

  View(Profit_percentage_per_category_2022)

colnames(Profit_percentage_per_category_2022)



                            ### Trends plots


ggplot(Amoree2022_v2)+ geom_bar(mapping=aes(x=Category))+ 
  labs(title="Number of orders by Category 2022")

    ##########label

ggplot(Amoree2022_v2, aes(x = "", y = Category, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") +
labs(title="Number of orders by Category 2022") +
  coord_polar("y", start = 0)+
  theme_void()

ggplot(Rev_2022,aes(x=Month,y=Revenue)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=seq(0,12, by=1)) +
     labs(title = "Revenue by months 2022",y="Revenue")

ggplot(Amoree2022_v2,aes(x=Month,y=Profit,fill=Category)) + 
  geom_col() + scale_x_continuous(breaks=seq(0,12.5,by=1)) +
  labs(title = "Profit by months 2022")

ggplot(Amoree2022_v2)+ geom_col(mapping=aes(x=Category,y=Price),fill="darkblue")+
  labs(title="Revenue per Category 2022",y="Revenue") 

ggplot(Amoree2022_v2)+ geom_col(mapping=aes(x=Category,y=Profit),fill="blue") +
  labs(title="Profit per Category 2022")






                  ### 2023
                  ### Import 2023 data



Amoree2023 <- read_excel("Data analysys/Project Amoree Gifting/Amoree Gifting Sales.xlsx",sheet = "2023")
head(Amoree2023)


Amoree2023 <- data.frame(Amoree2023)

colnames(Amoree2023)



### Rename columns

Amoree2023_v2 <- Amoree2023 %>% 
  rename(Sl_No="Sl.No.",Customer=Client.Name.,Price=Sale.Price....,Date=Delivery.Date.,
         Expense=Cost.Incurred...., Profit=Profit.Amount.....) 

Amoree2023_v2 <- add_column(Amoree2023_v2, Category=NA)
head(Amoree2023_v2)

### Categorize the Products based on text identified in Product column

for (i in 1:nrow(Amoree2023_v2)) 
{
  if(str_detect(Amoree2023_v2[i,3], ("Birthday")) == TRUE)
  {
    Amoree2023_v2[i,"Category"]="Birthday"
    
  }
  else
    
    if(str_detect(Amoree2023_v2[i,3],("Monogram")) == TRUE)
    {
      
      Amoree2023_v2[i,"Category"]="Monogram"
    }
  
  else
    
    if(str_detect(Amoree2023_v2[i,3],("Frame")) == TRUE)
    {
      
      Amoree2023_v2[i,"Category"]="Frames"
      
    }
  else
  {
    Amoree2023_v2[i,"Category"]="Others"
  }
  
}



for(i in 1:nrow(Amoree2023_v2))
  if((str_detect(Amoree2023_v2[i,3],("Liquor"))) | (str_detect(Amoree2023_v2[i,3],("Men"))) | (str_detect(Amoree2023_v2[i,3],("Alcohol")))  == TRUE)
  {
    
    Amoree2023_v2[i,"Category"]="Men's hampers"
    
  }




                      ### Categorize the Products based on Quantity

for(i in 1:nrow(Amoree2023_v2))
{
  if(Amoree2023_v2[i,6]>5)
    Amoree2023_v2[i,"Category"]="Return gifts/Bulk)"
  
}

View(Amoree2023_v2)



                    ### Convert price, expense to numeric data type from char
                    ### Convert Date in char to Date in Date type, split into month and year column



Amoree2023_v2 <- transform(Amoree2023_v2,Price = as.numeric(Price),Expense = as.numeric(Expense),Date=dmy(Date))

Amoree2023_v2 <- Amoree2023_v2 %>% mutate(Month=month(Date)) %>%
  mutate(Year=year(Date)) %>% arrange(Month)


head(Amoree2023_v2)



                    ### Summarize revenue cost profit


Rev_2023 <- Amoree2023_v2 %>% group_by(Month) %>%  summarise(Revenue = sum(Price,na.rm=TRUE),
                                                             Cost = sum(Expense,na.rm=TRUE),
                                                             Profit = sum(Profit,na.rm=TRUE))




                      ### Profit analysis

Profit_analysis_2023 <- Amoree2023_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% mutate(Profit_per_item=Profit/Qty) %>% 
  group_by(Category) %>% 
  summarise(Avg_profit_peritem=mean(Profit_per_item, na.rm = TRUE))
View(Profit_analysis_2023)

Revenue_per_category_2023 <- Amoree2023_v2 %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
  summarise(Revenue_per_category=sum(Price,na.rm=TRUE),
            Total_qty_per_category=sum(Qty,na.rm=TRUE))

Profit_per_category_2023 <- Amoree2023_v2  %>% 
  select(Product,Qty,Profit,Price,Category) %>% group_by(Category) %>% 
  summarise(Profit_per_category=sum(Profit,na.rm=TRUE))


                   ###### Final Profit analysis

Profit_percentage_per_category_2023 <- Profit_per_category_2023 %>% 
  mutate(Revenue_per_category=Revenue_per_category_2023$Revenue_per_category,
          Qty=Revenue_per_category_2023$Total_qty_per_category) %>% 
  mutate(Avg_price_per_category=Revenue_per_category/Qty,
         Profit_percent_per_category=(Profit_per_category/Revenue_per_category)*100)


View(Profit_percentage_per_category_2023)

colnames(Profit_percentage_per_category_2023)

                                          
                    ### Trends plots

ggplot(Amoree2023_v2)+ geom_bar(mapping=aes(x=Category))+ 
  labs(title="Number of orders by Category 2023")


ggplot(Amoree2023_v2, aes(x = "", y = Category, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") +
  labs(title="Number of orders by Category 2023") +
  coord_polar("y", start = 0)+
  theme_void()

ggplot(Rev_2023,aes(x=Month,y=Revenue)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=seq(0,12, by=1)) +
  labs(title = "Revenue by months 2023",y="Revenue")

ggplot(Amoree2023_v2,aes(x=Month,y=Profit,fill=Category)) + 
  geom_col() + scale_x_continuous(breaks=seq(0,12.5,by=1)) +
  labs(title = "Profit by months 2023")

ggplot(Amoree2023_v2)+ geom_col(mapping=aes(x=Category,y=Price),fill="darkblue")+
  labs(title="Revenue per Category 2023",y="Revenue") 

ggplot(Amoree2023_v2)+ geom_col(mapping=aes(x=Category,y=Profit),fill="blue") +
  labs(title="Profit per Category 2023")

### Export the cleaned data to query on SQL - SSMS 
  
write_csv(
  Amoree2021_v2,"Amoree Sales 2022 Cleaned.csv",
  append = FALSE,
  na="NA",
  path="C:\\Users\\91914\\Documents\\Data analysys\\Project Amoree Gifting\\Amoree Sales Cleaned.csv"
)

write_csv(
  Amoree2022_v2,"Amoree Sales 2022 Cleaned.csv",
  na="NA",
  append=TRUE,
  path="C:\\Users\\91914\\Documents\\Data analysys\\Project Amoree Gifting\\Amoree Sales Cleaned.csv"
)

write_csv(
  Amoree2023_v2,"Amoree Sales 2022 Cleaned.csv",
  append = TRUE,
  na="NA",
  path="C:\\Users\\91914\\Documents\\Data analysys\\Project Amoree Gifting\\Amoree Sales Cleaned.csv"
)



                                       ############# END ###############



### TEST AREA



### Summarize revenue cost profit



Rev_2021 <- Amoree2021_v2 %>% group_by(Month) %>%  summarise(Revenue = sum(Price,na.rm=TRUE),
                                                             Cost = sum(Expense,na.rm=TRUE),
                                                             Profit = sum(Profit,na.rm=TRUE))




ggplot(Amoree2021_v2, aes(x = "", y = Category, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") +
  labs(title="Number of orders by Category 2021") +
  coord_polar("y", start = 0)+
  theme_void()


dateis <- "22 January 2022"
print(typeof(dateis))

dateis <- dmy(dateis)
print(dateis)

ggplot(Amoree2022_v2, aes(x=Month)) + geom_col(aes(y=Price)) +
  scale_x_continuous(breaks=seq(0,12.5,by=1)) +
  labs(title = "Revenue by months 2022",y="Revenue")

###x=factor(Amoree2022_v2,level=c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
