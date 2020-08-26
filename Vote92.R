# Case Study Solutions : Vote92.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('vote92.csv')
# View the data loaded
data
# Dropping the 1st column which is a serial number
data=data[2:10]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 909 rows and 9 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to  : 
# Model on voter's probability of voting for a candidate (Perot, Bush or Clinton) :
# based on the different predictor variables (viz. dem, rep, female, clintondis, bushdis, perotdis, persfinance, natlecon)

# The response variable here is : vote ( 3 level variable ) 

# The data collected in the given excel file is grouped under which data class : "Cross Sectional Data"


#-------------------------------------------------------------------------------------------------

# Soln. to Question 3:

#Summarising the dataset : 
summary(data)

# Observations :
# 416 observations (max) seen for Clinton under vote followed by 310 for Bush and 183 for Perot
# The average clintondes is 3.5 followed by bushdis : 3.38 and perotdis : 2.17

# Check the datatypes
str(data)
sapply(data, class)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

data$vote
# Vote : the response variable is a factor vatiable with 3 levels (Bush, Clinton & Perot)
# Rest all the predictor variables or co-variates as seen are integer or numeric

#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

# Creating a frequency table

freq_table <- table(data$vote)
freq_table

# Highest choice of support is for Clinton with 416 votes and least is for Perot with 183 votes
#-------------------------------------------------------------------------------------------------

# Soln. to Question 6:

# Creating a 2 way frequency table

freq_table <- table(data$vote,data$female)
freq_table

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Vote : Gender Wise Distribution",xlab="Female",legend = rownames(freq_table), stacked=TRUE)

# Most of the females : 223 out of the total females have given vote choice for Clinton 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

# Fit a logit model for Perot

data$votePerot <- as.numeric(data$vote == "Perot")

logit_perot <- glm(votePerot ~ rep + female + perotdis,
                   data = data,
                   family = binomial(link = "logit"))
summary(logit_perot)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

# Calculating the "predicted probabilities" using predict() function :
pp_obs_perot <- predict(logit_perot, type = "response")
quantile(pp_obs_perot)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Objective of any logit model is to "maximize likelihood estimation"

# How do we achieve this objective ?

# A probability distribution for the target variable (class label) must be assumed and 
# then a likelihood function defined that calculates the probability of observing the outcome given the input data and the model. 
# This function can then be optimized to find the set of parameters that results in the largest sum likelihood over the training dataset.


#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

# Fit a logit model for Clinton

data$voteClinton <- as.numeric(data$vote == "Clinton")
logit_clinton <- glm(voteClinton ~ rep + female + clintondis,
                   data = data,
                   family = binomial(link = "logit"))
summary(logit_clinton)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# Calculating the "predicted probabilities" using predict() function :
pp_obs_clinton <- predict(logit_clinton, type = "response")
quantile(pp_obs_clinton)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

# Fit a logit model for Bush

data$voteBush <- as.numeric(data$vote == "Bush")
logit_bush <- glm(voteBush ~ rep + female + bushdis,
                     data = data,
                     family = binomial(link = "logit"))
summary(logit_bush)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# Calculating the "predicted probabilities" using predict() function :
pp_obs_bush <- predict(logit_bush, type = "response")
quantile(pp_obs_bush)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# Comparison of above created 3 logit models :

summary(logit_bush)
# AIC = 709.68 and Residual deviance of 701.68 on 905 degrees of freedom

summary(logit_perot)
# AIC = 904.5 and Residual deviance of 896.50 on 905 degrees of freedom

summary(logit_clinton)
# AIC = 788.56 and Residual deviance of 780.56 on 905 degrees of freedom

# A good model is the one that has minimum AIC among all the other models
# A lower AIC indicates a better fit.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

data$Bush <- ifelse(data$vote=="Bush", 1, 0)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

t.test(persfinance~Bush, data=data)

# The answer is yes, those who viewed their personal finances as improving were more likely to vote for Bush.
# The pvalue indicates that the difference in means between the two groups was highly unlikely to have occured by chance. 
# It is not impossible, but it is highly unlikely so we can declare there is a significant difference.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

library(dplyr)
data=data.frame(data)

# Creating a second dataframe only having Bush and Clinton in vote and removing Perot voters
vote92b = subset(data,vote != "Perot", )
attach(vote92b)
vote92b$vote[vote=="Perot"] = NA
detach(vote92b)
summary(vote92b)

#-------------------------------------------------------------------------------------------------

# Soln. to Question 19:

model1 = as.formula (vote ~ female + persfinance + natlecon + clintondis + bushdis)
glm.v1 = glm(vote ~ dem + rep + female + clintondis + bushdis + persfinance + natlecon, data=vote92b, famil=binomial(link="logit"))
summary(glm.v1)

#-------------------------------------------------------------------------------------------------
# Answer to Question : 20

# Alternative Model including gender interactions

model2 = update ( model1 ,. ~ . + female : clintondis + female : bushdis)
glm.v2 = glm (model2, family = binomial ( "logit" ), data = vote92b)
summary(glm.v2)
#-------------------------------------------------------------------------------------------------
