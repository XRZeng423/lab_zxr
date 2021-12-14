library(readr)
library(tidyverse)
library(stargazer)
library(fixest)
library(kableExtra)
library(caret)
library(cluster)
library(Metrics)
library(lares)


set.seed(1234)
setwd("E:/data/1213")

# load data
df<-read.csv("TrainingData.csv")

stargazer(df,
          type = 'text', 
          digits=2, align=T,
          median=T,
          title = "Summary Statistics ")



# preprocess
data <- df %>% 
  mutate_if(is.character, list(~na_if(.,"")))  %>% #fill blank with NA
  filter(total_net_acres != 0)%>% # delete missing value in size
  filter(location_zip_code != 0)%>% # delete missing value in location
  filter(location_zip_code != 44077)%>% #delete location in another state 
  mutate(sale_price = ifelse(predict==1,'predict', log(sale_price/total_net_acres)))%>% #convert price into price/size
  mutate(transaction_year = as.numeric(str_sub(transaction_date,-4,-1))) %>% # extract year
  mutate(transaction_month = as.numeric(match(str_to_title(str_sub(transaction_date,-7,-5)),month.abb))) %>% # extract month
  mutate(remodeled_year = ifelse(is.na(remodeled_year), built_year, remodeled_year))%>%
  mutate(remodeled_year = ifelse(remodeled_year==0, built_year, remodeled_year))%>%
  mutate (built_age = transaction_year-built_year)%>%  # change year into time interval
  mutate (location_zip_code = as.character(location_zip_code))
  
# remove redundant variables
data<-data[, !colnames(data) %in% c("transaction_date","built_year","remodeled_year",
                                    "section","township","range","quarter",
                                    "address_number","street_name",
                                    "street_type_code","unit_no","city_name","land_economic_area_code",
                                    "tax_district_no","neighborhood_code","neighborhood_extension","pre_direction_code",
                                    "grantor","grantee","built_as_code",
                                    "built_as")]

# missing values
#missing_values<-sapply(data, function(y) sum(is.na(y)))
data<-data[, which(colMeans(!is.na(data)) > 0.8)] # omit columns with more than 80% of missing value 
data<-na.omit(data)

# outliers
discrete<-colnames(data[ ,sapply(data, is.numeric)])[1:15]

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.1, .9), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


data[discrete]<-as.data.frame(sapply(data[discrete], function(y) remove_outliers(y)))
data<-na.omit(data)

# zero- and near zero-variance predictors
nzv<-nearZeroVar(data[, !colnames(data) %in% c("predict")],saveMetrics = TRUE)
nzv_features<-rownames(nzv)[which(nzv$nzv==TRUE)]
nzv_features
data<-data[ , -which(colnames(data) %in% nzv_features)]



# encoding: convert categorical variables to ordinal numerical variables
categorical<-colnames(data[ ,sapply(data, is.character)])

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

data$deed_type<-encode_ordinal(data$deed_type)
data$walkout_basement_flag<-encode_ordinal(data$walkout_basement_flag)

# for quality_code and condition, set the order mannually
data$quality_code<-encode_ordinal(data$quality_code,order = c("Low", "Fair", "Average", "Good","Very Good","Excellent"))
data$condition<-encode_ordinal(data$condition,order = c("Badly Worn", "Worn Out", "Average", "Good","Very Good","Excellent"))


train<-subset(data,predict==0)
train$sale_price<-as.numeric(train$sale_price)

# for hvac, exterior_construction_type, roof_material_code there are too many categories, use clustering algorithm to reduce the dimension
hvac<-aggregate(train$sale_price,by=list(train$hvac),mean)
d <- dist(hvac[,2], method = "euclidean") # Calculate the euclidean distance
fit2 <- hclust(d, method="ward.D") #Ward hierarchical clustering
plot(fit2) 
groups <- cutree(fit2, k=3) 
hvac$cluster<-groups
hvac$cluster<-ifelse(hvac$cluster=='3', 0, hvac$cluster)
colnames(hvac)<-c("hvac","price","hvac_group")

data<-merge(data,hvac[,c(1,3)],by=c("hvac"))

exterior_construction_type<-aggregate(train$sale_price,by=list(train$exterior_construction_type),mean)
d <- dist(exterior_construction_type[,2], method = "euclidean") # Calculate the euclidean distance
fit2 <- hclust(d, method="ward.D") #Ward hierarchical clustering
plot(fit2) 
groups <- cutree(fit2, k=3) 
exterior_construction_type$cluster<-groups
colnames(exterior_construction_type)<-c("exterior_construction_type","price","exterior_construction_type_group")
data<-merge(data,exterior_construction_type[,c(1,3)],by=c("exterior_construction_type"))


roof_material_code<-aggregate(train$sale_price,by=list(train$roof_material_code),mean)
d <- dist(roof_material_code[,2], method = "euclidean") # Calculate the euclidean distance
fit2 <- hclust(d, method="ward.D") #Ward hierarchical clustering
plot(fit2) 
groups <- cutree(fit2, k=4) 
roof_material_code$cluster<-groups
roof_material_code$cluster<-ifelse(roof_material_code$cluster=='4', 0, roof_material_code$cluster)
colnames(roof_material_code)<-c("roof_material_code","price","roof_material_code_group")
data<-merge(data,roof_material_code[,c(1,3)],by=c("roof_material_code"))

location_zip_code<-aggregate(train$sale_price,by=list(train$location_zip_code),mean)
d <- dist(location_zip_code[,2], method = "euclidean") # Calculate the euclidean distance
fit2 <- hclust(d, method="ward.D") #Ward hierarchical clustering
plot(fit2) 
groups <- cutree(fit2, k=5) 
location_zip_code$cluster<-groups
colnames(location_zip_code)<-c("location_zip_code","price","location_zip_code_group")
data<-merge(data,location_zip_code[,c(1,3)],by=c("location_zip_code"))


data<-data[, !colnames(data) %in% c("hvac","exterior_construction_type","roof_material_code","location_zip_code")]



# Correlated predictors
train<-subset(data,predict==0)
train<-train[, !colnames(train) %in% c("predict","property_id")]
train$sale_price<-as.numeric(train$sale_price)
descrCor <-  as.matrix(round(cor(train),2))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(round(cor(train),2), method="color", col=col(200),
         order="hclust",
         addCoef.col = "black",number.cex = 0.5,
         tl.col="black", tl.srt=45,tl.cex=0.8 
)

cor<-findCorrelation(descrCor,cutoff=0.8,names = TRUE)
cor
data<-data[, !colnames(data) %in% cor]

# reg
train<-train[, !colnames(train) %in% cor]

lmMod1 <- lm(sale_price ~ . , data = train)  # fit lm() model
summary(lmMod1)
 

coeff<-abs(coef(summary(lmMod1))[, "t value"])
coeff<-abs(coef(summary(lmMod1))[, "Estimate"])
coeff[order(coeff,decreasing = T)]

train<-train[, !colnames(train) %in% c("roof_material_code_group")]
lmMod2 <- lm(sale_price ~ . , data = train)  # fit lm() model
summary(lmMod2)

lmMod3 <- lm(sale_price ~ total_net_acres+
               transaction_year+
               location_zip_code_group+
               built_age+
               hvac_group+
               no_of_story
             , data = train)  # fit lm() model
summary(lmMod3)
stargazer(lmMod1,lmMod2,lmMod3,
          type = 'text',
          add.lines=list(c("AIC", round(AIC(lmMod1),1), round(AIC(lmMod2),1),round(AIC(lmMod3),1))))


# in sample predict
smp_size <- floor(0.7 * nrow(train))


set.seed(1234)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_train <- train[train_ind, ]
train_test <- train[-train_ind, ]


modFit <- train(sale_price  ~ . , method = "lm", data = train_train)
finMod <- modFit$finalModel
print(modFit)

print(finMod)

#Plotting Regression Diagnostic Plots
par(mfrow = c(2,2))
plot(finMod)

par(mfrow = c(1,2))
plot(finMod, 4, pch = 19, cex = 0.5)
plot(finMod, 5, pch = 19, cex = 0.5)


Prediction <- as.data.frame(predict(modFit, train_test))
Prediction$property_id<-data[rownames(Prediction),'property_id']
colnames(Prediction)[1]<-'predict'
train_test$property_id<-data[rownames(train_test),'property_id']
evaluation<-merge(Prediction,df[1:53],by=c('property_id'))

outliers<-evaluation[c(24346,40455),]


rmse(evaluation$sale_price, evaluation$predict)
mape(evaluation$sale_price, evaluation$predict)


mplot_lineal(tag = evaluation$sale_price,
              score = evaluation$predict,
              subtitle = "House Price Regression Model")


# outsample


test<-subset(data,predict==1)
valid<-read.csv("TestData.csv")

Prediction <- as.data.frame(predict(modFit, test))
Prediction$property_id<-data[rownames(Prediction),'property_id']
colnames(Prediction)[1]<-'predict'
evaluation<-merge(Prediction,valid[1:54],by=c('property_id'))
evaluation$sale_price<-log(evaluation$sale_price/evaluation$total_net_acres)

evaluation$se<-se(evaluation$sale_price, evaluation$predict)
evaluation$ape<-ape(evaluation$sale_price, evaluation$predict)

head(evaluation)
write.csv(evaluation,"prediction.csv")


rmse(evaluation$sale_price, evaluation$predict)
mape(evaluation$sale_price, evaluation$predict)

mplot_lineal(tag = evaluation$sale_price,
             score = evaluation$predict,
             subtitle = "House Price Regression Model")



