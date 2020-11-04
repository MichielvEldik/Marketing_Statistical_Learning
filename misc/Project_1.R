library(car)
library(corrplot)
library(nFactors)
library(GPArotation)
library(gplots)
library(RColorBrewer)
library(dplyr)
library(tidyr)

#Set up Bartlett's test for shared variance
Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, 
                        parameter=df, 
                        p.value=p.value,
                        method=method, 
                        data.name=data.name), 
                   class="htest"))
}

#Set up KMO test for sampling adequacy
kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) 
  r <- cor(x) 
  r2 <- r^2 
  i <- solve(r) 
  d <- diag(i) 
  p2 <- (-i/sqrt(outer(d, d)))^2 
  diag(r2) <- diag(p2) <- 0 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}


# Which working directory are we in?
getwd()

#Create dataset with standardized variables
sc_data <- read.csv('misc/SLIMSustainableFashionData.csv')

# There must be at least some correlation between variables. Else, there is nothing to compress
corrplot(cor(sc_data[, 11:23]), order="hclust")
corrplot(cor(sc_data[, 24:34]), order="hclust")

# This tests if your correlation matrix is sufficiently different from an identity matrix
Bartlett.sphericity.test(sc_data[,11:23])
Bartlett.sphericity.test(sc_data[,24:34])

#Run KMO test
kmo(sc_data[,11:23])
kmo(sc_data[,24:34])


# -----------
# Question 1 
# -----------
# What are the factors that can be derived from the many lifestyle and sustainable clothing perception variables?

#Standardize Q11 to Q34 because we need equal distance measure
sc_data[, 11:34] <- data.frame(scale(sc_data[, 11:34]))
#Create variable with all questions that are renamed
sc_data_Q11_to_q34 <- sc_data[, 11:34]
#Name sc_data Q11 to Q23
colnames(sc_data_Q11_to_q34) <- c("Taste", 
                                  "Expensive accessories", 
                                  "Famous", 
                                  "Ideas",
                                  "Function", 
                                  "Good eye", 
                                  "Expensive", 
                                  "Comfortable", 
                                  "Need", 
                                  "Different stores", 
                                  "First to try", 
                                  "Read about fashion",
                                  "Overall coordination",
                                  "Know where to buy",
                                  "Know about",
                                  "Don't feel knowledgeable ",
                                  "Expert (compared to friends)",
                                  "Don't know (compared to others)",
                                  "Don't know a lot",
                                  "Difficult to buy",
                                  "Easy to buy",
                                  "External influences that prevent buying",
                                  "Believe ofexpensiveness",
                                  "Control of ability in buying")
#Check dataset
summary(sc_data)
summary(sc_data_Q11_to_q34)

# Bartlett's test again
Bartlett.sphericity.test(sc_data[,11:23])
Bartlett.sphericity.test(sc_data[,24:34])

# Run KMO test again
kmo(sc_data[,11:23])
kmo(sc_data[,24:34])

# Determine eigen values and store in vector. Anything > 1 is good
sc_data_ev_q11toq23 <- eigen(cor(sc_data[, 11:23]))
sc_data_ev_q11toq23_vector <- sc_data_ev_q11toq23$values
sc_data_ev_q11toq23_vector
plot(sc_data_ev_q11toq23_vector)
# Determine eigen values and store in vector. Anything > 1 is good
sc_data_ev_q24toq34 <- eigen(cor(sc_data[, 24:34]))
sc_data_ev_q24toq34_vector <- sc_data_ev_q24toq34$values
sc_data_ev_q24toq34_vector
plot(sc_data_ev_q24toq34_vector)

# use nScree plot
nScree(sc_data_ev_q11toq23_vector)
nScree(sc_data_ev_q24toq34_vector)

# Factor analyses without rotation and store in vector
# Please do mind; Varimax is default!
sc_data_fa_q11toq23_no_rotation <- factanal(sc_data[, 11:23], factors = 4, rotation = "none")
sc_data_fa_q24toq34_no_rotation <- factanal(sc_data[, 24:34], factors = 4, rotation = "none")

# Factor analyses with default rotation and store in vector
sc_data_fa_q11toq23 <- c(factanal(sc_data[, 11:23], factors = 4))
sc_data_fa_q24toq34 <- c(factanal(sc_data[, 24:34], factors = 3))

#Factor analyses and store in vector with renamed labels
sc_data_fa_q11toq23 <- c(factanal(sc_data_Q11_to_q34[, 1:13], factors = 4))
sc_data_fa_q24toq34 <- c(factanal(sc_data_Q11_to_q34[, 14:24], factors = 3))

#factor analyses with orthogonal rotation and store in vector
sc_data_fa_orthogonal_q11toq23 <- c(factanal(sc_data[, 11:23], factors = 4, rotation= "varimax"))
sc_data_fa_orthogonal_q24toq34 <- c(factanal(sc_data[, 24:34], factors = 3, rotation= "varimax"))

#Print loadings of factor analyses
print(sc_data_fa_q11toq23$loadings)
print(sc_data_fa_q24toq34$loadings)

#Print loadings of factor analyses with no rotation
print(sc_data_fa_q11toq23_no_rotation$loadings)
print(sc_data_fa_q11toq23_no_rotation$loadings)



# Print loadings of factor analyses with orthogonal rotation
print(sc_data_fa_orthogonal_q11toq23$loadings)
print(sc_data_fa_orthogonal_q24toq34$loadings)


# make graph Q11 to Q23 with rotation
heatmap.2(sc_data_fa_q11toq23$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2, cexRow = 1.2, margins = c(19, 19),
          main="\n\n\n\n\nFactor loadings fashion")
# make graph Q11 to Q23 without rotation
heatmap.2(sc_data_fa_q11toq23_no_rotation$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for fashion without rotation")


# make graph Q24 to Q34 with rotation
heatmap.2(sc_data_fa_q24toq34$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2, cexRow = 1.2, margins = c(19, 19),
          main="\n\n\n\n\nFactor loadings sustainability")
# make graph Q24 to Q34 without rotation
heatmap.2(sc_data_fa_q24toq34_no_rotation$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for sustainable clothing without rotation")



# How do different age categories score on different factors on Q11 to Q23
# No rotation applied as it doesn't really do anything
sc_data_fa_q11toq23 <- c(factanal(sc_data[, 11:23], factors = 4, scores = "Bartlett")) # But you can always try to do so! :)
# Make it a data frame
Q11toQ23_scores <- data.frame(sc_data_fa_q11toq23$scores)
# Add age category
Q11toQ23_scores$AgeCategory <- sc_data$AgeCategory
# Have a look what at what it all looks like
head(Q11toQ23_scores)
# Aggregate everything on age category level
Q11_toQ23_fa_mean <- aggregate(. ~ AgeCategory, data=Q11toQ23_scores, mean)
# Not sure what's being done here...
rownames(Q11_toQ23_fa_mean) <- Q11_toQ23_fa_mean[, 1]
Q11_toQ23_fa_mean <- Q11_toQ23_fa_mean[, -1]
# Give names to columns
names(Q11_toQ23_fa_mean) <- c("Exploring", "Conscious", "Image oriented", "Functional")
# Have a look
Q11_toQ23_fa_mean
# What can we see? How are they gradient and stuff?
heatmap.2(as.matrix(Q11_toQ23_fa_mean),
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none", Rowv =FALSE,
          cexCol=1.2, cexRow = 1.2, margins = c(10, 10), main="\n\n\n\n\nFashion score in age
categories")



#How do different age categories score on different factors on Q24 to Q34
sc_data_fa_q24toq34 <- c(factanal(sc_data[, 24:34], factors = 3, scores = "Bartlett"))
Q24toQ34_scores <- data.frame(sc_data_fa_q24toq34$scores)
Q24toQ34_scores$AgeCategory <- sc_data$AgeCategory
head(Q24toQ34_scores)
Q24_toQ34_fa_mean <- aggregate(. ~ AgeCategory, data=Q24toQ34_scores, mean)
rownames(Q24_toQ34_fa_mean) <- Q24_toQ34_fa_mean[, 1]
Q24_toQ34_fa_mean <- Q24_toQ34_fa_mean[, -1]
names(Q24_toQ34_fa_mean) <- c("Aware", "Unaware", "Ease of buying")
Q24_toQ34_fa_mean
heatmap.2(as.matrix(Q24_toQ34_fa_mean),
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none", Rowv =FALSE,
          cexCol=1.2, cexRow = 1.2, margins = c(10, 10), main="\n\n\n\n\nSustainability score in age
categories")


#  ------------
# | Question 2 |
#  ------------
# Are respondents purchase intentions and willingness to recommend sustainable clothing
# the same across education level categories?


# Test for homogeneity of variances WTR
Bartlett_homogeneity_WTR <- bartlett.test(WTR ~ Education, data = sc_data)
Bartlett_homogeneity_WTR
# Test for homogeneity of variances PI1
Bartlett_homogeneity_PI1 <- bartlett.test(PI1 ~ Education, data = sc_data)
Bartlett_homogeneity_PI1
# Test for homogeneity of variances WTR
Fligner_homogeneity_WTR <- fligner.test(WTR ~ Education, data = sc_data)
Fligner_homogeneity_WTR
# Test for homogeneity of variances PI1
Fligner_homogeneity_PI1 <- fligner.test(PI1 ~ Education, data = sc_data)
Fligner_homogeneity_PI1
# Do focal variables have correlations? (Doesn't really make any sense to do this)
corrplot.mixed(cor(sc_data[ , c("Education", "PI1")]), upper="ellipse")
corrplot.mixed(cor(sc_data[ , c("Education", "WTR")]), upper="ellipse")
# Create dummy variables for age category (not required)
sc_data <- sc_data %>%
  mutate(Age21_30 = ifelse(sc_data$AgeCategory == '2', 1, 0)) %>%
  mutate(Age31_40 = ifelse(sc_data$AgeCategory == '3', 1, 0)) %>%
  mutate(Age41_50 = ifelse(sc_data$AgeCategory == '4', 1, 0)) %>%
  mutate(Age51_60 = ifelse(sc_data$AgeCategory == '5', 1, 0)) %>%
  mutate(Age60_99 = ifelse(sc_data$AgeCategory == '6', 1, 0))
  
#  Create dummy variables for education level
sc_data$Education_HS <- ifelse(sc_data$Education == '1', 1, 0)
sc_data$Education_MBO <- ifelse(sc_data$Education == '2', 1, 0)
sc_data$Education_HBO <- ifelse(sc_data$Education == '3', 1, 0)
sc_data$Education_UNI <- ifelse(sc_data$Education == '4', 1, 0)

# Linear model of purchase intention on education level with dummy variables
lm_m1 <- lm(PI1~Education_MBO + Education_HBO + Education_UNI, data=sc_data)
summary(lm_m1)
# lm_m1 reduced model
lm_m1_reduced <- lm(PI1 ~ 1, data=sc_data)
summary(lm_m1_reduced)
# Compare full model and reduced model (ANOVA) - P=0.1016, therefore not significant (marginally
# significant?) meaning there are no differences between groups of education level on purchase
# intention
anova(lm_m1_reduced, lm_m1)


# Linear model of WTR on education level with dummy variables
lm_m2 <- lm(WTR~Education_MBO + Education_HBO + Education_UNI, data=sc_data)
summary(lm_m2)
#lm_m2 reduced model
lm_m2_reduced <- lm(WTR~1, data=sc_data)
summary(lm_m2_reduced)
# Compare full model and reduced model (ANOVA) - P=0.9388, therefore not significant meaning
# there are no differences between groups of education of education level on willingness to
# recommend
anova(lm_m2_reduced,lm_m2)
#Purchase frequency is a continuous variable, therefore we can use it as a control variable
##Linear model with purchase frequency covariate of WTR on education level with dummy variables
lm_m3 <- lm(WTR~PurchaseFreq + Education_MBO + Education_HBO + Education_UNI,
            data=sc_data)
summary(lm_m3)
#lm_m3 reduced model
lm_m3_reduced <- lm(WTR~PurchaseFreq, data=sc_data)
summary(lm_m3_reduced)
# Compare full model and reduced model (ANCOVA) = P=.9778. therefore insignificant, when
# counting for purchase frequency, there are no differences between groups of education category
# on willingness to recommend
anova(lm_m3_reduced,lm_m3)
#Linear model with purchase frequency covariate of purchase intention on education level with
# dummy variables
lm_m4 <- lm(PI1~PurchaseFreq + Education_MBO + Education_HBO + Education_UNI, data=sc_data)
summary(lm_m4)
#lm_m4 reduced model
lm_m4_reduced <- lm(PI1~PurchaseFreq, data=sc_data)
summary(lm_m4_reduced)
# Compare full model and reduced model (ANCOVA) = P=0.03438, therefore significant, when
# accounting for purchase frequency, there are differences between groups of education levels on
# purchase intention
anova(lm_m4_reduced,lm_m4)


# Checking for Homogeneity in ANCOVA
# lm_m4 becomes reduced model, lm_m5 has interaction effects and is full model
lm_m5_reduced <- lm_m4
lm_m5 <- lm(PI1~PurchaseFreq + Education_MBO + Education_HBO + Education_UNI +
              PurchaseFreq*Education_MBO + PurchaseFreq*Education_HBO + PurchaseFreq*Education_UNI,
            data=sc_data)
summary(lm_m5)
# Checking for homogeneity in ANCOVA of PI1 with covariate purchase frequency - P=0.4698.
# Therefore not significant. This means Homogeneity is respected and the ANCOVA of purchase
# Intention with the covariate Purchase Frequency is better
anova(lm_m5_reduced, lm_m5)
#Use AIC and BIC
AIC(lm_m1)
BIC(lm_m1)
AIC(lm_m2)
BIC(lm_m2)


# -----------
#Question 3 |
# -----------
# What are the most important drivers of purchase intentions and willingness to
# recommend sustainable clothing?

# Create dataset that includes the factor scores with correct names
data_without_factors <- read.csv('misc/SLIMSustainableFashionData.csv')
# Getting the scores 
bar_scores_q11_q23 <- factanal(data_without_factors[,11:23], factors = 4, scores = "Bartlett")
bar_scores_q24_q34 <- factanal(data_without_factors[,24:34], factors = 3, scores = "Bartlett")

# data framing and naming the scores
factors_q11_q23 <- data.frame(bar_scores_q11_q23$scores)
colnames(factors_q11_q23) <- c("Exploring", "Conscious", "Image_oriented", "Functional")

# data framing and naming the scores
factors_q24_q34 <- data.frame(bar_scores_q24_q34$scores)
colnames(factors_q24_q34) <- c("Aware", "Unaware", "Ease_of_buying")

# Formulate new data set
data_with_factors <- cbind(data_without_factors, factors_q11_q23, factors_q24_q34)


#Create dummy variables for gender (Use else as reference? or dismiss else?)
data_with_factors$Gender_Female <- ifelse(data_with_factors$Gender == '1', 1, 0)
data_with_factors$Gender_Male <- ifelse(data_with_factors$Gender == '2', 1, 0)
data_with_factors$Gender_Else <- ifelse(data_with_factors$Gender == '3', 1, 0)

# Remove Gender else 
data_with_factors <- data_with_factors[data_with_factors$Gender != "3", ]

# Create multiple regression PI1
multipleregression_1 <- lm(PI1 ~ 
                             Exploring + 
                             Conscious + 
                             Image_oriented + 
                             Functional + 
                             Aware +
                             Unaware + 
                             Ease_of_buying + 
                             Gender_Female, 
                           data = data_with_factors)
summary(multipleregression_1)

# Create multiple regression WTR
multipleregression_2 <- lm(WTR ~ Exploring +
                             Conscious + 
                             Image_oriented + 
                             Functional + 
                             Aware +
                             Unaware + 
                             Ease_of_buying + 
                             Gender_Female, 
                           data = data_with_factors)
summary(multipleregression_2)

#Load car package
library(car)

# Linear hypothesis: is gender more important than image orientation? Not significant for PI1
# meaning they are equal to 0, maybe gender and image orientation are correlated?
linearHypothesis(multipleregression_1, "Image_oriented = Gender_Female")
# Potentially different one, however not significant
linearHypothesis(std_multipleregression_2, "Image_oriented = Aware")

# Correlation tests -> r(154) = .17, p = 0.036
cor.test(data_with_factors$Image_oriented, data_with_factors$Gender_Female)

# testing for multicollinearity model 1
# create vector of VIF values and vector of tolerance values
vif_values <- vif(multipleregression_1)
tolerance_values <- 1 / vif_values
vif_values
tolerance_values

# create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values multiple regression PI1", horiz = TRUE, xlim = c(0,12), col =
          "red")
#add vertical line at 4 and 10
abline(v = 4, lwd= 3, lty = 2 )
abline(v = 10, lwd= 3, lty = 2 )
#testing for multicollinearity model 2
#create vector of VIF values and vector of tolerance values
vif_values <- vif(multipleregression_2)
tolerance_values <- 1 / vif_values
vif_values
tolerance_values
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values multiple regression WTR", horiz = TRUE, xlim = c(0,12), col =
          "red")
#add vertical line at 4 and 10
abline(v = 4, lwd= 3, lty = 2 )
abline(v = 10, lwd= 3, lty = 2 )
#Question 4 -> moderation effect van IV: Image orientation met moderator spenditure on clothing op
# DV PI1
#Mean-center -> rescale x and z (moderator and IV) -> now means one more than the mean of the
# old variable
mm1_helpa = data_with_factors[,c(39, 40, 41, 42, 43, 44, 45, 46, 8)]
PI1 = data_with_factors[,c(35)]
center_scale <- function(x){scale(x,scale = FALSE)}
mm1_helpb <- center_scale(mm1_helpa)
data_with_factors_mc1.df <- data.frame(mm1_helpb, PI1)
#Moderation model
Moderation_model1 <- lm(PI1~Exploring + Conscious + Image_oriented + Functional + Aware +
                          Unaware + Ease_of_buying + Gender_Female + MoneySpent + Image_oriented*MoneySpent , data
                        =data_with_factors_mc1.df)
summary(Moderation_model1)

#Create dummy variables for education level in data_with_factors
data_with_factors$Education_HS <- ifelse(data_with_factors$Education == '1', 1, 0)
data_with_factors$Education_MBO <- ifelse(data_with_factors$Education == '2', 1, 0)
data_with_factors$Education_HBO <- ifelse(data_with_factors$Education == '3', 1, 0)
data_with_factors$Education_UNI <- ifelse(data_with_factors$Education == '4', 1, 0)

# Mean-center -> rescale x and z (moderator and IV) -> now means one more than the mean of the
# old variable
mm2_helpa = data_with_factors[,c(39, 40, 41, 42, 43, 44, 45, 46, 49, 50, 51)]
WTR = data_with_factors[,c(37)]
center_scale <- function(x){scale(x,scale = FALSE)}
mm2_helpb <- center_scale(mm2_helpa)
data_with_factors_mc2.df <- data.frame(mm2_helpb, WTR)

# Moderation model
Moderation_model2 <- lm(WTR~Exploring + Conscious + Image_oriented + Functional + Aware +
                          Unaware + Ease_of_buying + Gender_Female + Education_HS + Education_MBO + Education_HBO +
                          Image_oriented*Education_HS + Image_oriented*Education_MBO +
                          Image_oriented*Education_HBO, data =data_with_factors_mc2.df)
summary(Moderation_model2)
