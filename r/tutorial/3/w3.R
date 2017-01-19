library(datasets)
data(iris)
data(mtcars)

mean(subset(iris, iris$Species == "virginica")$Sepal.Length)
# [1] 6.588

with(mtcars, tapply(mpg, cyl, mean))
#        4        6        8
# 26.66364 19.74286 15.10000

hp_by_cyl = with(mtcars, tapply(hp, cyl, mean))
hp_by_cyl[["8"]] - hp_by_cyl[["4"]]
# [1] 126.5779

