#I have chosen to display my analysis in chronological order, showing the order of steps
#I took as I moved through the analysis. Sections with 'required' are necessary to build
#the model, whereas the other sections are additional analyses conducted to gain an
#understanding of the data and support model development. Within each section is an
#explanation of the section, the explanations are compiled in the 'script section
#breakdown' document which is attached with my submission.

#libraries_______________________________________________________required####
#The libraries I used to conduct the analysis
library(data.table) #General syntax and functions
library(ggplot2) #Visualizations
library(rpart) #Decision tree
library(rpart.plot) #Decision tree visualization
library(quanteda) #NLP

#data____________________________________________________________required####
#Read in the listings.csv data
data = fread("C:/Users/jordan.torti/Documents/misc/ndr/listings.csv")


#data understanding####
#Gain a general understanding of the data before considering trimming outliers,
#feature engineering and model selection. Followed a question, answer format
#where I write a comment with the question, then the code in the next line and a
#commented answer on the following line, where applicable.

#Q: What does the data look like
head(data)

#Q: how many listings are there?
nrow(data)
#A: 48,864 listings


#Q: are all listings in nyc?
unique(data$neighbourhood_group)
#A: yes, listings in all 5 boroughs

#Q: what is the count by borough?
table(data$neighbourhood_group)
#A:
#manhattan = 21,456
#brooklyn = 20,114
#queens = 5,811
#bronx = 1,105
#staten island = 378

#Q: What is the room type breakdown
table(data$room_type)
#A:
#entire home/apt = 25,296
#private room = 22,397
#shared room = 1,171

#Q: how many neighborhoods have listings?
length(unique(data$neighbourhood))
#A: 222 different neighborhoods


#Q: what are the summaries of the numeric data?
summary(data$minimum_nights)
summary(data$calculated_host_listings_count)
summary(data$availability_365)
summary(data$number_of_reviews)
summary(data$reviews_per_month)
summary(data$price)


#Q: how many hosts are there?
length(unique(data$host_id))
#37,384 unique hosts amonog the 48,864 listings

#Q: how many unique host names are there?
length(unique(data$host_name))
#A: 11,408 host names among the 48,864 listings

#Q: is the sum of the calculated host listings equal to the number of listings in the dataset?
sum(data$calculated_host_listings_count)
#A: sum is 363,464

#Q: shouldn't the sum of calculated_host_listings_count be equal to nrow(data)?
#A: (from ndr team) listings.csv represents a subset of the total AirBnB listing dataset,
#   hosts may have listings outside of this dataset

#Q: How many listings exist per host_id?
listings_by_host = data[, .N, by = host_id]

#Q: How many listings exist per night minimum?
min_nights = data[, .N, by = minimum_nights]

#Q: How many listings have a night minimum of more than 10?
nrow(data[minimum_nights > 10])
#6,690 have a night minimum of more than 10

#Q: How many listings exist per availability?
availability = data[, .N, by = availability_365]

#Q: How many listings with 0 availability have reviews?
review_by_availability = data[, .N, by = list(availability_365,number_of_reviews)]


#Q: Spot check to see if calculated_host listings_count matches existing data
nrow(data[host_id == "219517861"])
nrow(data[host_id == "107434423"])
#A: all have the same nrow in the dataset as value of calculated_host_listings_count

#Q: how many listings have no reviews?
reviews_analysis = data[, .N, by = number_of_reviews]
#A: 10,131 have no reviews

#data preparation________________________________________________required####
#Trim outliers and error from data. To produce better visualizations in future sections,
#create data_trim to visualize listing price distribution. Justifications for each removal below.

#Remove: all listings that have had no availability in the past 365 days
#Why: decided this because we can't make any price predictions that take vacancy into account
#     using listings that have not been active for a year
#Remove: listings with minimum_nights over 120
#Why: decided that this was the threshhold for outlier values
#Remove: listings where minimum_nights > availability_365
#Why: a user on the app couldn't book this location since the listing would have fewer
#     nights available for booking than the number of nights required to make a booking
#Remove: listings with no reviews
#Why: challenge to accurately infer vacancy  
data = data[!(minimum_nights > 120 | availability_365 < minimum_nights |
                availability_365 == '0' | number_of_reviews == '0') ]

#trim price points above 350 to produce better
#visualizations. chose 350 bc 2x value at 75th percentile
#and also did not trim a large number of data points

data_trim = data[price < 350]
#cut from 25,164 to 23,778


##
#feature engineering_____________________________________________required####
#Create a feature that represents nights booked in the past year.
#The feature, 'percent_booked' takes on a few assumptions:
#First, for listings on the platform for over 1 year, I assumed
#that in the past year, each listing sustained its average reviews per month rather than assuming
#any change in the rate of reviews.
#Second, I assumed that 50% of guests who booked a listing left a review.
#Third, I assumed that every booking was for the minimum number of nights.
#All assumptions were made by researching on various AirBnB host forums, also kept in mind
#the desire to not overestimate the feature.


#intermediate functions to show process 
#sample[,'months_on_platform':= sample[,13] / sample[,14]]
#sample[,'reviews_365' := ifelse(months_on_platform < 12, (number_of_reviews), (reviews_per_month*12))]
#sample[,'bookings_365' := ifelse(months_on_platform < 12, 2*(number_of_reviews), 2*(reviews_per_month*12))]

#below is the value for nights booked before setting the threshold
data[,'intermediate_nights_booked' := ifelse((data[,13] / data[,14]) > 12,
                                  (minimum_nights*2*(reviews_per_month*12)),
                                  (minimum_nights*2*(number_of_reviews)))]

#set a threshold so nights booked is maxed at the listings availabilty
#Why: done because bookings that had intermediate values higher than their
#     availability are assumed to be in high demand
data[, 'nights_booked' := ifelse(intermediate_nights_booked > availability_365, floor(availability_365),
                                   floor(intermediate_nights_booked))]

data = data[,-'intermediate_nights_booked']

data[, 'percent_booked' := floor((nights_booked / availability_365)*100) ]


#pearson coefficient####
#Calculated the pearson coefficient of price versus percent_booked to understand the
#strength of correlation

#see what they lok like in a plot
ggplot(data, aes(x=percent_booked, y=price)) + geom_point()
ggplot(data_trim, aes(x=percent_booked, y=price)) + geom_point()

cor.test(data$percent_booked, data$price, method = "pearson")
#-.07


#double check price v vacancy relationship####
#With a Pearson coefficient near 0, I decided to recreate the percent_booked feature,
#this time removing any assumptions made in the previous version of the feature. The
#first assumption was accounted for because any listings that had been on the platform
#for more than 12 months were removed. It was also assumed that number of reviews equaled
#number of bookings, which accounted for assumption two. Assumption three remained, since
#it was already the most conservative estimate possible.

ind_test_data = data
ind_test_data[,'months_on_platform' := (ind_test_data[,13] / ind_test_data[,14])]
                                       
ind_test_data = ind_test_data[!(months_on_platform > 12)]
#limiting dataset to listings that are less than one year old eliminated the avg_reviews_per_month assumption

ind_test_data[,'intermediate_nights_booked' := number_of_reviews]

ind_test_data[, 'nights_booked' := ifelse(intermediate_nights_booked > availability_365, floor(availability_365),
                                 floor(intermediate_nights_booked))]

ind_test_data = ind_test_data[,-'intermediate_nights_booked']

ind_test_data[, 'percent_booked' := floor((nights_booked / availability_365)*100) ]

summary(ind_test_data$price)
ind_test_data_trim = ind_test_data[price < 370]
summary(ind_test_data_trim$price)

ggplot(ind_test_data_trim, aes(x=percent_booked, y=price)) + geom_point()

cor.test(ind_test_data_trim$percent_booked, ind_test_data_trim$price, method = "pearson")
#-.06 so still basically no correlation

#visualize price data####
#Price has been verified to be independent of vacancy, so visualize
#listing price information against possible features

ggplot(data_trim, aes(x=number_of_reviews, y=price)) + geom_point()

boxplot_room.type = ggplot(data_trim, aes(x = room_type, y = price)) +
  geom_boxplot()

boxplot_borough = ggplot(data_trim, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

#visualize trimmed price low vacancy data####
#Another vacancy versus price gut check, visualizing price patterns
#for all listings versus listings with low vacancy
low_vacancy = data_trim[percent_booked > 74]

summary(low_vacancy$price)
summary(data_trim$price)

lv_boxplot_borough = ggplot(low_vacancy, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

ggplot(data_trim, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

#review v price correlation####
#Calculate Pearson coefficient for number_of_reviews v price

ggplot(data_trim, aes(x=number_of_reviews, y=price)) + geom_point()

cor.test(data_trim$number_of_reviews, data_trim$price, method = "pearson")



#classify data and data_trim_____________________________________required####
#Classify the data into 4 listing price groups, decided based on
#price IQR range

summary(data$price)
#1st.qu=69, median=105, 3rd.qu=175
data$price_class <- ifelse(data$price > 174.99, 1,
                       ifelse(data$price > 104.99, 2,
                              ifelse(data$price > 68.99, 3, 4)))

summary(data_trim$price)
#1st.qu=65, median=100, 3rd.qu=160
data_trim$price_class <- ifelse(data_trim$price > 159.99, 1,
                           ifelse(data_trim$price > 99.99, 2,
                                  ifelse(data_trim$price > 64.99, 3, 4)))










#split data and data_trim into training, validation and test_____required####
#70% training, 15% validation, 15% test

#split data
split_data = sample(1:3, size = nrow(data), prob = c(.7,.15,.15), replace = TRUE)
data_train = data[split_data == 1]
data_val = data[split_data == 2]
data_test = data[split_data == 3]
rm(split_data)

#split data_trim
split_data_trim = sample(1:3, size = nrow(data_trim), prob = c(.7,.15,.15), replace = TRUE)
data_trim_train = data_trim[split_data_trim == 1]
data_trim_val = data_trim[split_data_trim == 2]
data_trim_test = data_trim[split_data_trim == 3]
rm(split_data_trim)
#run and evaluate model on training data_________________________required####
#Train decision tree model and measure accuracy and precision

test = rpart(price_class~ neighbourhood_group + room_type, method = "class", data = data_train)
rpart.plot(test, tweak = 1)

pred_data_train = predict(test, data_train, type = "class")
train_conf_mat = table(data_train$price_class, pred_data_train)
sum(diag(train_conf_mat)/sum(train_conf_mat)) #accuracy
#52.75% accurate
(train_conf_mat[1,1])/(sum(train_conf_mat[,1])) #1 precision 
#63% precise
(train_conf_mat[2,2])/(sum(train_conf_mat[,2])) #2 precision
#41% precise
(train_conf_mat[3,3])/(sum(train_conf_mat[,3])) #3 precision
#43% precise
(train_conf_mat[4,4])/(sum(train_conf_mat[,4])) #4 precison
#59% precise

#tf idf analysis and model creation####
#To improve model accuracy and precision, a term frequency-inverse 
#document frequency (tf-idf) analysis was run on the names of all the listings.
#A tf-idf analysis examines the importance of a term in a text document. My idea
#is that certain words that may be present in a name, like 'cozy', or 'getaway'
#could make a listing more appealing to a user on the platform and allow the host
#to increase the listing's price. As you can see in the code below, this hypothesis
#was proved to be untrue as models with tf-idf terms output the same results as the
#models without them.

text_data = corpus(data_train$name)
text_data = dfm(text_data, remove = c(stopwords("english"),
                remove_punct = TRUE, remove_numbers = TRUE))

tf_idf = dfm_tfidf(text_data, scheme_tf = "count", scheme_df = "inverse", base = 10,force = FALSE)
tf_idf = as.data.table(tf_idf)

tf_idf_dataset = cbind(data_train,tf_idf)

tf_idf_dataset = tf_idf_dataset[,c(18,5,9,19:5885)]

#tf_idf_model1 = rpart(Relevant~.,method = "class", data = training_data_tf_idf_model1)

nlp_test = rpart(price_class~ neighbourhood_group + room_type + hideaway + perfect + super +
hip + modern + village + comfy + cute + suite + comfortable + amazing + enormous +
welcomes + charming + luminous + glorious + modernist + luxurious, method = "class", data = tf_idf_dataset)



rpart.plot(nlp_test, tweak = 1)

#realized that no words were able to decipher further than what the room type / borough analysis does

#run and evaluate model on validation data_______________________required####
#Validate decision tree model and measure accuracy and precision


pred_data_val = predict(test, data_val, type = "class")
val_conf_mat = table(data_val$price_class, pred_data_val)
sum(diag(val_conf_mat)/sum(val_conf_mat)) #accuracy
#51.46% accurate
(val_conf_mat[1,1])/(sum(val_conf_mat[,1])) #1 precision 
#59% precise
(val_conf_mat[2,2])/(sum(val_conf_mat[,2])) #2 precision
#39% precise
(val_conf_mat[3,3])/(sum(val_conf_mat[,3])) #3 precision
#42% precise
(val_conf_mat[4,4])/(sum(val_conf_mat[,4])) #4 precison
#60.50% precise

#run and evaluate model on test data_____________________________required####
#Test decision tree model and measure accuracy and precision

pred_data_test = predict(test, data_test, type = "class")
test_conf_mat = table(data_test$price_class, pred_data_test)
sum(diag(test_conf_mat)/sum(test_conf_mat)) #accuracy
#52.48% accurate
(test_conf_mat[1,1])/(sum(test_conf_mat[,1])) #1 precision 
#61.47% precise
(test_conf_mat[2,2])/(sum(test_conf_mat[,2])) #2 precision
#39.52% precise
(test_conf_mat[3,3])/(sum(test_conf_mat[,3])) #3 precision
#43.47% precise
(test_conf_mat[4,4])/(sum(test_conf_mat[,4])) #4 precison
#61.22% precise
