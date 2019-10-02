temps <- as.numeric(nhtemp)
(as.numeric(nhtemp) - 32) * (5 / 9) # ANS: the single line of R code
words <- c("wrath", "avarice", "sloth", "pride", "lust", "envy", "gluttony") # ANS
words[(length(words)-1):length(words)] # ANS: print last two elements

a <- c("Harvard", "MIT", "Stanford")
b <- c(TRUE, TRUE, FALSE)
b + 3

d <- matrix(c(a, b), nrow=3)
d
# d + 3
method1 <- matrix(49:20, nrow=6, ncol=5, byrow = TRUE) # method 1
method1
method1[3, ] # display values in the 3rd row
method1[,seq(from=2,to=4,by=2)] # display values in 2nd and 4th column
method1[,c(2, 4)] # display values in 2nd and 4th column

method2 <- rbind(49:45, 44:40, 39:35, 34:30, 29:25, 24:20)
method2

method3 <- cbind(c(49, 44, 39, 34, 29, 24),
                 c(48, 43, 38, 33, 28, 23),
                 c(47, 42, 37, 32, 27, 22),
                 c(46, 41, 36, 31, 26, 21),
                 c(45, 40, 35, 30, 25, 20))
method3

data(BOD)
class(BOD) # returns data.frame
mode(BOD) # returns list
str(BOD) # how many obs and variables BOD has

data(USArrests)
# extracting data for California
USArrests[5,]
# extracting all values for UrbanPop
USArrests[,3]
# all variables for the state with the lowest murder rate
USArrests[which.min(USArrests$Murder),]
# Only observations with a minimum UrbanPop of 85 or higher
USArrests[USArrests$UrbanPop >= 85, ]
# Fahrenheit to Celsius Function
fToC <- function(temp) {
  finalValue <- (temp - 32) * (5 / 9)
  finalValue
}