# 1. More Readable Version of the SD function
mysd1 <- function(x, na.rm = FALSE) {
  if (is.vector(x) || is.factor(x))
    x <- x
  else
    x <- as.double(x)
  variance <- var(x, na.rm = na.rm)
  standard_deviation <- sqrt(variance)
  standard_deviation
}
# 2. Modify an R function to accommodate something different
mysd2 <- function(x, na.rm = FALSE) {
  if (is.vector(x) || is.factor(x))
    x <- x
  else if (is.data.frame(x))
    return(sapply(x, sd))
  else
    x <- as.double(x)
  variance <- var(x, na.rm = na.rm)
  standard_deviation <- sqrt(variance)
  standard_deviation
}
# 3. Pythagorean Theorem Function
pythag <- function(a, b) {
  if (is.character(sidea) || is.character(sideb))
    stop('I need numeric values to make this work')
  else if (sidea < 0 || sideb < 0)
    stop('Values need to be positive')
  result <- sqrt((sidea ^ 2) + (sideb ^ 2))
  return(list("hypotenuse" = result, "sidea" = a, "sideb" = b))
}
# 4. Loops vs. lapply
set.seed(1)
a <- data.frame(matrix(sample(1:1000,1000000, replace=TRUE), nrow=100000, ncol=10))
names(a) <- c("alpha", "bravo", "charlie", "delta", "echo", "foxtrot",
              "golf", "hotel", "india", "juliet")
# Timing for loop 1
system.time({out <- data.frame(matrix(NA, nrow(a), ncol(a)))
names(out) <- names(a)
for(i in seq_along(a)){
  out[[i]] <- sqrt(a[,i])
}  
})
# Timing for loop 2
system.time({out2 <- data.frame(matrix(NA, nrow(a), ncol(a)))
names(out2) <- names(a)
nm <- names(a)
for(nm in names(a)){
  out2[[nm]] <-sqrt(a[[nm]])
}  
})
# Timing for loop 3
xs <- as.vector(unlist(a[1:1000,]))
system.time({
  res <- c()
  for (x in xs) {
    res <-c(res, sqrt(x))
  }
  res <-data.frame(matrix(res, nrow=1000, ncol=10))
})
# timing for lapply style 1
system.time({a2 <- lapply(a, function(x) sqrt(x))
names(a2) <- names(a)
})
# timing for lapply style 2
system.time({a2 <- lapply(seq_along(a), function(i) sqrt(a[[i]]))
names(a2) <- names(a)
})
# timing for lapply style 3
system.time({nm <- names(a)
a2 <- lapply(names(a), function(nm) sqrt(a[[nm]]))
names(a2) <- names(a)
})
# Answers for Part 4:

# An Alternate Method
baseball2 <- plyr::baseball %>% select(c(1, 2, 6:12)) %>% group_by(year) %>% 
  mutate(g = g / max(g), ab = ab / max(ab), r = r / max(r),
         h = h / max(h), X2b = X2b / max(X2b), X3b = X3b / max(X3b), hr = hr / max(hr))

kable(head(baseball2))
str(baseball2)

# 4.1
baseball <- plyr::baseball
# Method 1: 
col_names <- lapply(names(baseball), class)
names(col_names) <- names(baseball) 
# Method 2:
col_names_alt <- lapply(baseball, class)
# 4.2
baseball2 <- baseball[, c(1, 2, 6:12)]
# tapply(baseball2$g, baseball2$year, max, na.rm = TRUE) %>% View()

for (i in 1871:2007) {
  for (j in 3:9) {
    maximum <- max(baseball2[baseball2$year == i, ][j])
    baseball2[baseball2$year == i, ][j] <- (baseball2[baseball2$year == i, ][j]/maximum)
  } 
}
baseball2 # modified dataset

baseball3 <- plyr::baseball %>% select(c(1, 2, 6:12)) %>% group_by(year) %>% 
  mutate(g = g / max(g), ab = ab / max(ab), r = r / max(r),
         h = h / max(h), X2b = X2b / max(X2b), X3b = X3b / max(X3b), hr = hr / max(hr))

kable(head(baseball3))
# 5. Interacting with files outside of R
# Part A:
random_url <- "http://www.stat.ucla.edu/~vlew/datasets/spssSTUFF.zip"
download.file(random_url, "spssSTUFF.zip")
unzip("spssSTUFF.zip", exdir = "spssSTUFFdata")
list.files("spssSTUFFdata")
# Part B: 
library(readr)
merged <- do.call(rbind, files <- lapply(list.files(pattern = "JAN[0-9][0-9]*.csv"), function(x) read.csv(x, stringsAsFactors = FALSE)))

my_table <- lapply(list.files(pattern = "JAN[0-9][0-9]*.csv"), read_csv) %>% bind_rows()
kable(head(my_table))