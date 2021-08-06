library(tidyverse)
library(lubridate)
library(randomForest)

# DATA INPUTS ####
libraries <- read.csv("C:/Users/juani/Box/Autonomo/Billups/Data Challenge/libraries.csv")
books <- read.csv("C:/Users/juani/Box/Autonomo/Billups/Data Challenge/books.csv")
customers <- read.csv("C:/Users/juani/Box/Autonomo/Billups/Data Challenge/customers.csv")
checkouts <- read.csv("C:/Users/juani/Box/Autonomo/Billups/Data Challenge/checkouts.csv")

# OTHER INPUTS ####
checkout_period <- 28

# remove % from dates
checkouts$date_checkout<-gsub("[[:punct:]]","",checkouts$date_checkout)
checkouts$date_checkout<-ymd(checkouts$date_checkout)

checkouts$date_returned<-gsub("[[:punct:]]","",checkouts$date_returned)
checkouts$date_returned<-ymd(checkouts$date_returned)

# format to dates
checkouts$date_checkout <- as.Date(checkouts$date_checkout)
checkouts$date_returned <- as.Date(checkouts$date_returned)

# assumption date_checkouts: all year dates to 2018
checkouts <- checkouts %>%
  do({year(.$date_checkout)<-2018; .})

# assumption date_returned: all year dates to 2018 except 2019 ones (usually close to late 2018 so deemed true)
`%notin%` <- Negate(`%in%`)

checkouts <- checkouts %>%
  mutate(year_returned = ifelse(year(date_returned) %notin% c(2018, 2019), 2018, year(date_returned)),
         date_returned = as.Date(paste(year_returned, month(date_returned), day(date_returned),sep="-"), "%Y-%m-%d")
         ) %>% select(-c(year_returned)) 

# late book binary check
checkouts <- checkouts %>%
  mutate(late_book = ifelse(date_returned - date_checkout > checkout_period, 1, 0),
         month_checkout = month(date_checkout)
         )

sum(checkouts$late_book, na.rm = TRUE)

# CLEAN VARIABLES INTO GLM ####
# books price USD, |, $, *, 
books$price<-gsub("USD","",books$price)
books$price<-gsub("[[:punct:]]","",books$price)    # removes decimal points as well

books$price<-as.numeric(books$price)

# pattern is divide by 100 if ends in 99, by 10 if ends in 0 or 5
books <- books %>% mutate(price = ifelse(price%%5 == 0, price / 10, 
                                         ifelse(price%%2 == 0, price / 10, price / 100)))

# remove special characters from pages
books$pages<-gsub("[[:punct:]]","",books$pages)    
books$pages<-as.numeric(books$pages)

# remove special characters from zipcode - customers
customers$zipcode<-gsub("[[:punct:]]","",customers$zipcode)    
customers$zipcode<-as.numeric(customers$zipcode)

customers <- customers %>% mutate(zipcode = round(zipcode / 10, 0))

# treat birth of date to extract only meaningful year of birth
# 2019-05-21 is the latest returned date in the sample
max_date_returned <- max(checkouts$date_returned, na.rm = TRUE)

# 79.6 years is the life expectancy in Oregon State (oregon.gov)
oregon_life_exp <- 79.6 * 365

# every year of birth in the customers sample after (max_date_returned - 10 years) and before (max_date_returned - oregon_life_exp) will be deemed NA
customers$birth_date <- as.Date(customers$birth_date)

customers <- customers %>% mutate(year_birth = ifelse(year(birth_date) < year(max_date_returned - 3650) & 
                                                        year(birth_date) > year(max_date_returned - oregon_life_exp), year(birth_date), NA))

sum(is.na(customers$year_birth))

# deal with gender and others
# unique(customers$gender) -> "female"  " female" "male"    "MALE"    ""        "male "   "FEMALE"  " male"   "female "
# remove spaces and change to lower case
customers$gender<-gsub(" ","",customers$gender)
customers$gender<- tolower(customers$gender)

# remove spaces and change to lower case
customers$education<-gsub(" ","",customers$education)
customers$education<- tolower(customers$education)

# remove spaces and change to lower case
customers$occupation<-gsub(" ","",customers$occupation)
customers$occupation<- tolower(customers$occupation)

# deal with post code - libraries
libraries$postal_code<-gsub("[[:punct:]]","",libraries$postal_code)
libraries$postal_code<-gsub(" ","",libraries$postal_code)

# TRAINING GLM ####
# building modeling sample for model 1
checkouts_modelling <- left_join(checkouts,
                                 books %>% select(id, price, pages),
                                 by=c("id")
                                 )

checkouts_modelling <- left_join(checkouts_modelling,
                                 customers %>% select(patron_id=id, zip_customer=zipcode, year_birth, gender, education, occupation),
                                 by=c("patron_id")
                                 )

checkouts_modelling <- left_join(checkouts_modelling,
                                 libraries %>% select(library_id=id, zip_library=postal_code),
                                 by=c("library_id"))

zip_customer_summary <- checkouts_modelling %>% group_by(zip_customer) %>% 
  summarise(n=n(),
            late=sum(late_book, na.rm = TRUE)
            )

# checkouts_modelling <- na.omit(checkouts_modelling)
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$late_book) | checkouts_modelling$late_book==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$month_checkout) | checkouts_modelling$month_checkout==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$price) | checkouts_modelling$price==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$pages) | checkouts_modelling$pages==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$year_birth) | checkouts_modelling$year_birth==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$gender) | checkouts_modelling$gender==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$education) | checkouts_modelling$education==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$occupation) | checkouts_modelling$occupation==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$zip_library) | checkouts_modelling$zip_library==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$zip_customer) | checkouts_modelling$zip_customer==""), ]


# check and change data format going into GLM
str(checkouts_modelling)
checkouts_modelling$zip_customer <- as.factor(checkouts_modelling$zip_customer)
checkouts_modelling$gender <- as.factor(checkouts_modelling$gender)
checkouts_modelling$education <- as.factor(checkouts_modelling$education)
checkouts_modelling$occupation <- as.factor(checkouts_modelling$occupation)
checkouts_modelling$zip_library <- as.factor(checkouts_modelling$zip_library)
# we can leave month as numerical

checkouts_modelling <- checkouts_modelling %>% select(-c(id, patron_id, library_id, date_checkout, date_returned
                                                         ))

# split sample for training and test 
set.seed(333)
sample <- sample.int(n = nrow(checkouts_modelling), size = floor(.85*nrow(checkouts_modelling)), replace = F)

train_checkouts <- checkouts_modelling[sample, ]
test_checkouts  <- checkouts_modelling[-sample, ]

# train GLM (binomial logistic regression)
# model 1 with all variables that can make sense to add
model_version_1 <- glm(late_book ~ month_checkout + price + pages + zip_customer + year_birth + gender + education + occupation + zip_library,
                       family=binomial(link='logit'),data=train_checkouts)

summary(model_version_1)
anova(model_version_1, test="Chisq")

# model 1
fitted.results <- predict(model_version_1,newdata=subset(test_checkouts,select=c(2:10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_checkouts$late_book)
overall_accuracy <- 1-misClasificError

test_checkouts_m1 <- cbind(test_checkouts, fitted.results)

test_checkouts_m1 <- test_checkouts_m1 %>% mutate(positives = ifelse(late_book == 0, "", 
                                                                     ifelse(late_book == 1 & fitted.results ==1, 1, -1)))

sum(as.numeric(test_checkouts_m1$positives), na.rm = TRUE)


# model 2 with only significant variables in model 1
checkouts_modelling <- left_join(checkouts,
                                 books %>% select(id, pages),
                                 by=c("id")
)

checkouts_modelling <- left_join(checkouts_modelling,
                                 libraries %>% select(library_id=id, zip_library=postal_code),
                                 by=c("library_id"))

# checkouts_modelling <- na.omit(checkouts_modelling)
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$late_book) | checkouts_modelling$late_book==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$month_checkout) | checkouts_modelling$month_checkout==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$pages) | checkouts_modelling$pages==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$zip_library) | checkouts_modelling$zip_library==""), ]


# check and change data format going into GLM
str(checkouts_modelling)
checkouts_modelling$late_book <- as.factor(checkouts_modelling$late_book)
checkouts_modelling$month_checkout <- as.factor(checkouts_modelling$month_checkout)
checkouts_modelling$zip_library <- as.factor(checkouts_modelling$zip_library)

checkouts_modelling <- checkouts_modelling %>% select(-c(id, patron_id, library_id, date_checkout, date_returned))

# split sample for training and test 
set.seed(333)
sample <- sample.int(n = nrow(checkouts_modelling), size = floor(.85*nrow(checkouts_modelling)), replace = F)

train_checkouts <- checkouts_modelling[sample, ]
test_checkouts  <- checkouts_modelling[-sample, ]

model_version_2 <- glm(late_book ~ month_checkout + pages + zip_library,
                       family=binomial(link='logit'),data=train_checkouts)

summary(model_version_2)
anova(model_version_2, test="Chisq")

# model 2
fitted.results <- predict(model_version_2,newdata=subset(test_checkouts,select=c(2:4)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_checkouts$late_book)
overall_accuracy <- 1-misClasificError

test_checkouts_m2 <- cbind(test_checkouts, fitted.results)

test_checkouts_m2 <- test_checkouts_m2 %>% mutate(positives = ifelse(late_book == 0, "", 
                                                                     ifelse(late_book == 1 & fitted.results ==1, 1, -1)))

sum(as.numeric(test_checkouts_m2$positives), na.rm = TRUE)

# MODEL RANDOM FOREST ####
# building modeling sample
checkouts_modelling <- left_join(checkouts,
                                 books %>% select(id, price, pages),
                                 by=c("id")
)

checkouts_modelling <- left_join(checkouts_modelling,
                                 customers %>% select(patron_id=id, zip_customer=zipcode, year_birth, gender, education, occupation),
                                 by=c("patron_id")
)

checkouts_modelling <- left_join(checkouts_modelling,
                                 libraries %>% select(library_id=id, zip_library=postal_code),
                                 by=c("library_id"))

# check and change data format going into GLM
str(checkouts_modelling)
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$late_book) | checkouts_modelling$late_book==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$month_checkout) | checkouts_modelling$month_checkout==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$price) | checkouts_modelling$price==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$pages) | checkouts_modelling$pages==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$year_birth) | checkouts_modelling$year_birth==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$gender) | checkouts_modelling$gender==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$education) | checkouts_modelling$education==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$occupation) | checkouts_modelling$occupation==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$zip_library) | checkouts_modelling$zip_library==""), ]
checkouts_modelling <-  checkouts_modelling[!(is.na(checkouts_modelling$zip_customer) | checkouts_modelling$zip_customer==""), ]

# check and change data format going into GLM
str(checkouts_modelling)
checkouts_modelling$zip_customer <- as.factor(checkouts_modelling$zip_customer)
checkouts_modelling$gender <- as.factor(checkouts_modelling$gender)
checkouts_modelling$education <- as.factor(checkouts_modelling$education)
checkouts_modelling$occupation <- as.factor(checkouts_modelling$occupation)
checkouts_modelling$zip_library <- as.factor(checkouts_modelling$zip_library)
checkouts_modelling$month_checkout <- as.factor(checkouts_modelling$month_checkout)
checkouts_modelling$late_book <- as.factor(checkouts_modelling$late_book)


checkouts_modelling <- checkouts_modelling %>% select(-c(id, patron_id, library_id, date_checkout, date_returned
))



# split sample for training and test 
set.seed(333)
sample <- sample.int(n = nrow(checkouts_modelling), size = floor(.85*nrow(checkouts_modelling)), replace = F)

train_checkouts <- checkouts_modelling[sample, ]
test_checkouts  <- checkouts_modelling[-sample, ]

model_rf_1 <- randomForest(late_book ~ month_checkout + price + pages + year_birth + gender + education + occupation + zip_library
                           , data = train_checkouts, importance = TRUE)

model_rf_1

# Predicting on train set
predTrain <- predict(model_rf_1, test_checkouts, type = "class")
# Checking classification accuracy
table(predTrain, test_checkouts$late_book)  

test_checkouts_m4 <- cbind(test_checkouts, predTrain)

# group data by diff ids (book, customer, library) to have a hint of whats going on ####
checkouts_modelling <- left_join(checkouts,
                                 books %>% select(id, price, pages),
                                 by=c("id")
)

checkouts_modelling <- left_join(checkouts_modelling,
                                 customers %>% select(patron_id=id, zip_customer=zipcode, year_birth, gender, education, occupation),
                                 by=c("patron_id")
)

checkouts_modelling <- left_join(checkouts_modelling,
                                 libraries %>% select(library_id=id, zip_library=postal_code),
                                 by=c("library_id"))

hist(checkouts_modelling$year_birth)
hist(checkouts_modelling$month_checkout)

gender_summary <- checkouts_modelling %>% group_by(gender) %>% 
  summarise(n=n(),
            late=sum(late_book, na.rm = TRUE)
            ) %>% 
  mutate(freq=round(late/n, 2))

education_summary <- checkouts_modelling %>% group_by(education, month_checkout) %>% 
  summarise(n=n(),
            late=sum(late_book, na.rm = TRUE)
  ) %>% 
  mutate(freq=round(late/n, 2))

occupation_summary <- checkouts_modelling %>% group_by(occupation) %>% 
  summarise(n=n(),
            late=sum(late_book, na.rm = TRUE)
  ) %>% 
  mutate(freq=round(late/n, 2))








