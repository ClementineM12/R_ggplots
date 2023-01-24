# Import necessary libraries
library(readxl)
library(dplyr)
library(patchwork)
library(scales)

# Open the Helper_functions script
source("C:/Users/George/Desktop/Biostatistics/Helper_functions_Biostatistics.R") # Insert directory path.

# View the dataset
data <- BIOSTATISTICS_ATTICA_10YS_STUDY
dim(data) # View the shape of the data
data <- na.omit(data) # Drop the rows with missing values
dim(data)

# Out of the 431 individuals in the reviews, 53 developed CVD within 10 year period.
table(data$CVD_10_yr)

# Define specific variables

# Body Mass Index (~ 18.5 to 24.9)
bmi <- data$bmi
# Systolic blood pressure (~ < 120 mmHg)
sbp <- data$sbp
# Diastolic blood pressure (~ < 80 mmHg)
dbp <- data$dbp
# Triglyceride (~ < 150 mg/dL)
tgl <- data$tgl
# HDL (good) cholesterol (~ > 45 mg/dL)
hdl <- data$hdl
# LDL (bad) cholesterol (~ < 100 mg/dL)
ldl <- data$ldl
# Mediterranean diet score
MedDiet <- data$MedDietScore

## A. Statistic measurements and visualization.
## A.1)
# Create a list of the variables and a vector of the variables' names
data_ <- list(bmi, sbp, dbp, tgl, hdl, ldl, MedDiet)
data_names <- c('BMI', 'SBP', 'DBP', 'TGL', 'HDL', 'LDL', 'MedDiet')
# Table with the statistical measures of the given variables
statistics_df(data_, data_names)

# Frequency histograms-polygons
frequency_plots(bmi, "Body Mass Index", 0.5, 3)
frequency_plots(sbp, "Systolic blood pressure", 5, 9)
frequency_plots(dbp, "Diastolic blood pressure", 3, 6)
frequency_plots(tgl, "Triglycedire", 5, 10)
frequency_plots(hdl, "HDL (good) cholesterol", 0.8, 3)
frequency_plots(ldl, "LDL (bad) cholesterol", 3, 9)
frequency_plots(MedDiet, "Mediterranean diet score", 0.8, 6)

## A.2) Correlation with BMI.
smoke <- c("Non-Smokers", "Smokers")

box_density_plots(cursmok, bmi, smoke)
box_density_plots(cursmok, sbp, smoke)
box_density_plots(cursmok, dbp, smoke)
box_density_plots(cursmok, tgl, smoke)
box_density_plots(cursmok, hdl, smoke)
box_density_plots(cursmok, ldl, smoke)
box_density_plots(cursmok, MedDiet, smoke)

## A.3) Correlation with Physical Activity.
exercise <- c("No-Exercise", "With-Exersice")

box_density_plots(PhysActivity, bmi, exercise)
box_density_plots(PhysActivity, sbp, exercise)
box_density_plots(PhysActivity, dbp, exercise)
box_density_plots(PhysActivity, tgl, exercise)
box_density_plots(PhysActivity, hdl, exercise)
box_density_plots(PhysActivity, ldl, exercise)
box_density_plots(PhysActivity, MedDiet, exercise)

## A.4) Correlation with the appearance of CVD within 10 years range.
CVD_10 <- c("No-CVD", "CVD-appearance")

box_density_plots(CVD_10_yr, bmi, CVD_10)
box_density_plots(CVD_10_yr, sbp, CVD_10)
box_density_plots(CVD_10_yr, dbp, CVD_10)
box_density_plots(CVD_10_yr, tgl, CVD_10)
box_density_plots(CVD_10_yr, hdl, CVD_10)
box_density_plots(CVD_10_yr, ldl, CVD_10)
box_density_plots(CVD_10_yr, MedDiet, CVD_10)

## A.5)
family_cat <- c("Unmarried", "Married", "Separated", "Widowed")
# Column financial has actually four categories instead of three as mentioned in the document.
table(data$financia)
financial_cat <- c("Low", "Average_low", "Average_High", "High")

box_density_plots(family, bmi, family_cat)
box_density_plots(family, sbp, family_cat)
box_density_plots(family, dbp, family_cat)
box_density_plots(family, tgl, family_cat)
box_density_plots(family, hdl, family_cat)
box_density_plots(family, ldl, family_cat)
box_density_plots(family, MedDiet, family_cat)

box_density_plots(financia, bmi, financial_cat)
box_density_plots(financia, sbp, financial_cat)
box_density_plots(financia, dbp, financial_cat)
box_density_plots(financia, tgl, financial_cat)
box_density_plots(financia, hdl, financial_cat)
box_density_plots(financia, ldl, financial_cat)
box_density_plots(financia, MedDiet, financial_cat)

## Β.1)
# 1st variable is row variable(x), 2nd variable is column variable(y)
names_var2 <- c("NO", "YES")
plot_title <- c("CVD Occurence within 10 years")
manual_name <- c("CVD")

data_table_and_percentage_barplot(sex, CVD_10_yr, c("F", "M"), 
                                  names_var2, plot_title, manual_name, c("Gender"))
data_table_and_percentage_barplot(cursmok, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Smoking"))
data_table_and_percentage_barplot(PhysActivity, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Physical activity"))
data_table_and_percentage_barplot(HTN, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Hypertension"))
data_table_and_percentage_barplot(DM, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Diabetes"))
data_table_and_percentage_barplot(HCHOL, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Hypercholesterolaemia"))
data_table_and_percentage_barplot(MS, CVD_10_yr, c("NO", "YES"), 
                                  names_var2, plot_title, manual_name, c("Metabolic syndrome"))
# Based on the BMI group
data_table_and_percentage_barplot(bmi_grou, CVD_10_yr, c("0", "1", "2", "3"), 
                                  names_var2, plot_title, manual_name, c("BMI"))

## B.2)
## B.3)


## C. Analyse. Searching for associations between variables.
# Hypothesis testing : H0: There's no difference between the means.
#                      H1: (what we hope to support)
# We presume that the null hypothesis is true, unless the data provide sufficient evidence that it is not.

## C.1) Numeric Variables ~ Smoking
barplot_(cursmok, c("No", "Yes"))

t.test(bmi ~ cursmok, data = data) # p-value = 0.5735  
t.test(sbp ~ cursmok, data = data) # p-value = 0.2915
t.test(dbp ~ cursmok, data = data) # p-value = 0.06325
t.test(tgl ~ cursmok, data = data) # p-value = 0.3725
t.test(hdl ~ cursmok, data = data) # p-value = 0.5815
t.test(ldl ~ cursmok, data = data) # p-value = 0.8083
t.test(MedDiet ~ cursmok, data = data) # p-value = 0.9599
# Observation: Fail to reject the null hypothesis and conclude that not enough evidence 
#              is available to suggest the null is false at the 95% confidence level.
#              The results were not statistically significant.

## C.2) Numeric Variables ~ Exercising
barplot_(PhysActivity, c("No", "Yes"))

t.test(bmi ~ PhysActivity, data = data) # p-value = 0.00004323 
t.test(sbp ~ PhysActivity, data = data) # p-value = 0.2042
t.test(dbp ~ PhysActivity, data = data) # p-value = 0.2052
t.test(tgl ~ PhysActivity, data = data) # p-value = 0.1678
t.test(hdl ~ PhysActivity, data = data) # p-value = 0.001065
t.test(ldl ~ PhysActivity, data = data) # p-value = 0.5647
t.test(MedDiet ~ PhysActivity, data = data) # p-value = 0.6376
# Observation: For variables BMI, HDL we reject the null hypothesis with a high degree of confidence, 
#              as a result there is actually a difference between the means in the BMI for groups of 
#              smokers and non-smokers. (same for the HDL)
#              The observed difference is statistically significant.

## C.3) Numeric Variables ~ Cardiovascular occurrence within 10 years
barplot_(CVD_10_yr, c("No", "Yes"))

t.test(bmi ~ CVD_10_yr, data = data) # p-value = 0.00002108
t.test(sbp ~ CVD_10_yr, data = data) # p-value = 0.00002276
t.test(dbp ~ CVD_10_yr, data = data) # p-value = 0.0000007036
t.test(tgl ~ CVD_10_yr, data = data) # p-value = 0.00007588
t.test(hdl ~ CVD_10_yr, data = data) # p-value = 0.2431
t.test(ldl ~ CVD_10_yr, data = data) # p-value = 0.0008687
t.test(MedDiet ~ CVD_10_yr, data = data) # p-value = 0.00001672
# Observation: 
#              
#  

## C.4) Two categorical variables
data_table_and_percentage_barplot(HTN, CVD_10_yr, c("No", "Yes"), 
                                  names_var2, c("CVD appearance ~ High blood pressure"), 
                                  manual_name, c("HBP"))

CVD_with_HBP <- data$CVD_10_yr[data$HTN == 1]
CVD_with_no_HBP <- data$CVD_10_yr[data$HTN == 0]

var.test(CVD_with_no_HBP, CVD_with_HBP) # p-value = 0.00000000714
t.test(CVD_with_no_HBP, CVD_with_HBP, mu=0, var.equal=TRUE) # p-value = 0.0000608
# Observation: 
#              
#  

## C.5) Two categorical variables
Cardiovascular_disease <- data$CVD_10_yr
High_blood_pressure <- data$HTN

chisq.test(Cardiovascular_disease, High_blood_pressure) # p-value = 0.0001318
# Observation: 
#              
#  

## C.6) Numerical variable ~ Categorical
# Because the categorical variable has more than two categories we have to do a ANOVA
shapiro.test(data$family) # p-value < 2.2e-16
ks.test(data$family, data$bmi)
shapiro.test(data$financia) # p-value < 2.2e-16
# Hypothesis testing : H0: The population is normally distributed.
#                      H1: Not normally distributed.
# Observation: Because p-value < 0.05 the null hypothesis is rejected.
#              
#  
# Bartlett test of homogenecity of variances
bartlett.test(data$bmi ~ data$family) # p-value = 0.5676
bartlett.test(data$bmi ~ data$financia) # p-value = 0.03187


## C.7)











