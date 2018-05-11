#===#===#===#===#===#===#===#===#===#===#===#===#===#
#===#===#===#===#===#===#===#===#===#===#===#===#===#
#===#                                           #===#
#===#         Standard error functions          #===#
#===#                                           #===#
#===#===#===#===#===#===#===#===#===#===#===#===#===#
#===#===#===#===#===#===#===#===#===#===#===#===#===#


## Regular SE

se <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / sqrt(length(na.exclude(x)))


## SE for difference

se_diff <- function(x, y) {
    
    xy <- data.frame(x, y)
    xy.c <- xy[complete.cases(xy), ]
    
    sqrt( (var(xy.c$x) / length(xy.c$x)) + var(xy.c$y) / length(xy.c$y) )
}
