# Load dataset
data(mtcars)

# View first rows
head(mtcars)

# Boxplot for mpg
boxplot(mtcars$mpg,
        main="Boxplot of MPG",
        ylab="Miles per Gallon",
        col="purple")