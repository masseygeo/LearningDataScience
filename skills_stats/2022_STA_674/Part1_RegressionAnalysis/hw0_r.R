####click tool -> install packages -> type ggplot2 -> and install 


library(ggplot2)
x <- seq(5, 15, length=1000)
y <- dnorm(x, mean=10, sd=3)
d = data.frame(x, y)
ggplot(data = d, aes(x = x, y = y)) + geom_line() + 
  ggtitle("Type your first name here" , subtitle = "Type your last name here")

######After you generate the plot, you need to save the plot.
###Click Export (right above the plot) -> save as either pdf of jpg 
###-> save at the folder where you can find it. -> upload to canvas and you are done