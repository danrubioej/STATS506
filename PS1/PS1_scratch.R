### Daniel Rubio, PS1
# As an aid to my problem solving, I used UM GPT to probe for explanations
# when I was confused, and for suggestions of possible built in functions to use
# I did not copy and paste the prompts into the AI, but rather used it to 
# answer the questions I had as they arose.
## Problem 1: Wine Data
# (a) 
# Here I read in the square data from wine.data into a var called df:
df <- read.csv("wine.data")

# wine.names does not seem like a file that is ease to parse with code,
# therefore I manually created a vector for the names:
names <- c("class", "alcohol", "malic acid", "ash", "ash_alcalinity",
           "magnesium", "phenols", "flavanoids", "nf_phenols", "proanthocyanins",
           "color", "hue","OD", "proline")   
# Add the names to df
colnames(df) <- names
# (b)
# Instances according to wine.names:  1: 59, 2: 71, 3: 48
# 
class_count <- table(df$class)
print(class_count)
print(nrow(df))

# There are 177 objects, which 177 = 58+71+48, so wine.names was wrong

# (c.1)
abv_color_cor <-cor(df$alcohol,df$color)

# (c.2)
df1 <- df[df$class == 1,]
class1_cor <-cor(df1$alcohol,df1$color)

df2 <- df[df$class == 2,]
class2_cor <-cor(df2$alcohol,df2$color)

df3 <- df[df$class == 3,]
class3_cor <-cor(df3$alcohol,df3$color)

# Class 1 has the highest correlation at 0.41,
# followed by 3 with 0.35, then 2 with 0.27

# (c.3)
brightest_abv <- df$alcohol[which.max(df$color)]

# (c.4)
n_pro_over_ash <- nrow(df[df$proanthocyanins > df$ash,])
pro_over_ash_percentage <- 100*n_pro_over_ash/nrow(df)

# (d)

mean_vals <- colMeans(df)
mean_vals[1] <- 0
mean_by_class <- aggregate(.~class,data=df,FUN=mean)

mean_vals <- rbind(mean_vals, mean_by_class)

# (e)
a_b <-t.test(df1$phenols,df2$phenols)
b_c <-t.test(df2$phenols,df3$phenols)
a_c <-t.test(df1$phenols,df3$phenols)

#> print(a_b)
# 
# Welch Two Sample t-test
# 
# data:  df1$phenols and df2$phenols
# t = 7.3888, df = 119.57, p-value = 2.196e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4260304 0.7379472
# sample estimates:
#   mean of x mean of y 
# 2.840862  2.258873 
# 
# > print(b_c)
# 
# Welch Two Sample t-test
# 
# data:  df2$phenols and df3$phenols
# t = 7.0125, df = 116.91, p-value = 1.622e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4162855 0.7439610
# sample estimates:
#   mean of x mean of y 
# 2.258873  1.678750 
# 
# > print(a_c)
# 
# Welch Two Sample t-test
# 
# data:  df1$phenols and df3$phenols
# t = 17.006, df = 98.593, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.026510 1.297715
# sample estimates:
#   mean of x mean of y 
# 2.840862  1.678750 
# Since the P values are less than 0.5 (almost zero in fact) we can say that
# The means of the phenols do vary between the three classes of wines

##
rm(list=ls())
## Problem 2: Ask A Manager
# (a)
df <- read.csv("AskAManager.csv")
# (b)
names <- c("entry","datetime","age","industry","title",
           "job_context", "salary","bonus","currency","currency_other",
           "income_context","country","us_state","city","experience",
           "field_experience","education","gender","race_s")
colnames(df) <- names

# (c)
nrow(df) # Output:[1] 28062
df <- df[df$currency == "USD",]
nrow(df) # Output:[1] 23374

# (d)
ages <- table(df$age)
print(ages) # There are 13 people listed as "under 18"

df <- df[df$age != "under 18",]
nrow(df) # [1] 23361

exps <- table(df$experience)
f_exps <- table(df$field_experience)
# The rationale for this ranking is to take the high of the age range
# and subtract 18, to see what is the max possible amount of 
# experience that can be true for that range, such that:
# 18-24 -> 6 years, 25-34 -> 16 years, 35-44 -> 26 years,
# 45-54 -> 36 years, and a 55-64 -> 46 
# When comparing as factors this should eliminate only impossibilities
time_order <-c("1 year or less","2 - 4 years","5-7 years","18-24",
               "8 - 10 years","11 - 20 years","25-34",
               "21 - 30 years","35-44",
               "31 - 40 years","45-54",
               "41 years or more","55-64", "65 or over")

df$age_f <- factor(df$age,levels=time_order,ordered=TRUE)
df$experience_f <- factor(df$experience,levels=time_order,ordered=TRUE)
df$field_experience_f <- factor(df$field_experience,levels=time_order,ordered=TRUE)

nrow(df) # [1] 23361
df <- df[(df$age_f > df$experience_f)&(df$age_f > df$field_experience_f),]
nrow(df) # [1] 23313
df <- df[df$experience_f >= df$field_experience_f,]
nrow(df) # [1] 23116

# (e)
boxplot(df$salary)
print(df[which.max(df$salary),])
# this guy is an outlier that makes the rest of the data
# unintelligible in graphs, probably reported in peso due to notes
df <-df[-which.max(df$salary),]
nrow(df) # [1] 2311
boxplot(df$salary)
median(df$salary)
millionaires <- df[df$salary>1e6,]
print(millionaires)
# These people seem legit, so I'll leave these data
# Looking at low
nrow(df[df$salary <= 100,]) # 72 people
no_money <-df[df$salary <= 100,]
# Looking at this data, it seems that some of them truly have no income,
# some of them put their income as bonuses (or do so for tax reasons)
# or thought they were reporting in thousands of dollars

nrow(df[df$salary <= 10000,]) # 96 people
less_money <-df[df$salary <= 10000,]

# Similar to no money, except maybe here it was some people who just dropped a 
# 0 when typing their salary, for this case, I would probably just remove
# all of these people as it is a very small fraction of the data set (< 100)
# as I would be guessing trying to fix the data and that introduces bias

df <- df[df$salary > 10000,]
nrow(df)# [1] 23019

##
rm(list=ls())
## Problem 3: Palindromic Numbers
# (a)
isPalindromic <- function(x){
  # Take in a positive integer, and returns a list
  # of two elements, a boolean showing if the int is
  # palindromic, and the reversed int
  if (!is.numeric(x)) {
    warning("x must be numeric, attempting to convert")
    suppressWarnings(x <- as.numeric(x))
    if (all(is.na(x))) {
      stop("x must be numeric, or convertible to numeric")
    }
  }
  if (x%%1 != 0) {
    stop("x must be an integer")
  }
  if (x < 0) {
    stop("x must be positive")
  }
  bool <- TRUE
  x_str <- strsplit(as.character(x),split="")[[1]]
  x_r <- rev(x_str)
  if (length(x_str) == 1){
    return(list(isPalindromic=bool, reversed=x))
  }

  for (i in 1:floor(length((x_str))/2)){
    if( x_str[i]!= x_r[i]){
      bool <-FALSE
    }
  }
  x_r <- paste(x_r,collapse = "")
  x_r <- as.numeric(x_r)
  return(list(isPalindromic=bool, reversed=x_r))
  }
# **note, some of the error checking is taken directly from lecture scripts

isPalindromic(728827)
isPalindromic(39951)

# (b)
nextPalindrome <- function(x){
  # Take in a positive integer, and returns a vector containing
  # the next palindromic number greater than x
  if (!is.numeric(x)) {
    warning("x must be numeric, attempting to convert")
    suppressWarnings(x <- as.numeric(x))
    if (all(is.na(x))) {
      stop("x must be numeric, or convertible to numeric")
    }
  }
  if (x%%1 != 0) {
    stop("x must be an integer")
  }
  if (x < 0) {
    stop("x must be positive")
  }
  bool <- TRUE
  while (bool){
    x <- x+1
    if (isPalindromic(x)[[1]] == TRUE)
      bool <- FALSE
  }

  return(x)
}
# **note, some of the error checking is taken directly from lecture scripts
nextPalindrome(7152)
nextPalindrome(765431537)

# (c)
nextPalindrome(391)
nextPalindrome(9928)
nextPalindrome(19272719)
nextPalindrome(109)
nextPalindrome(2)


