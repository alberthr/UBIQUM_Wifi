df <- read.csv("trainingData.csv")
vd <- read.csv("validationData.csv")


df$origen <- "df"
vd$origen <- "vd"

tdf <- rbind(df, vd)
tdf2 <- sample_n(tdf,size = 100)

library(plotly)
plot_ly(tdf2, x=~LONGITUDE, y=~LATITUDE, Z=~FLOOR, color=~origen,
        type="scatter3d", mode="markers")






library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Weight'),
                        yaxis = list(title = 'Gross horsepower'),
                        zaxis = list(title = '1/4 mile time')))
p
