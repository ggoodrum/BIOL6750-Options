# Author: Greg Goodrum
# Course: BIOL 6750 - Programming for Biologists
# Section: Session 13 - A Brief Tour of Plotting in R
# ------------------------------------------------------------

# 1.  Replicate 'Homework 1' in Advanced Base R using ggplot2.
#     Build slowly one piece at a time.

# Organize data inside a data frame.
data <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100), response = a*b+c+rnorm(1000))
data <- data.frame(a=a, b=b, c=c, response=response)

# Create better color ramp
better.cols <- colorRampPalette(brewer.pal(9, name='RdBu'))(1000)

# Create scaled version of c variable so it ranges from 0 to 2.
scale.c <- c + abs(min(data$c))
scale.c <- (scale.c/(max(scale.c))) * 2

# Create plots, consider doing one with ggplot and one with qplot()
plt1 <- ggplot(data=data, aes(x=b,y=a)) + geom_point(aes(colour=response, size=scale.c)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), plot.title = element_text(face = 'bold', hjust = 0.5), axis.text.y= element_text(angle=90, face='bold')) +
  scale_size(range=c(0,2), guide = FALSE) +
  scale_colour_gradient2(low='red', high='blue', mid='grey40', guide = FALSE) +
  xlab('b') + ylab('a') + ggtitle("OK colors")

plt2 <- ggplot(data=data, aes(x=b,y=a)) + geom_point(aes(colour=response, size=scale.c)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), plot.title = element_text(face = 'bold', hjust = 0.5), axis.text.y = element_text(angle=90, face='bold')) +
  scale_colour_gradientn(colours = better.cols, guide=FALSE) +
  scale_size(range=c(0,2), guide = FALSE) +
  xlab('b') + ylab('a') + ggtitle("Better colors")

# Use grid.arrange() to orient plots next to one another
grid.arrange(plt1, plt2, nrow=1)


# 2. (a) Add a legend where indicated in "Advanced Base R"
#        Make sure to do in Base R.


# 2. (b) Make that legend appear in a separate plotting frame
#        in the bottom row of your plot.  You should have the
#        main plot as the entire top row, and the bottom row
#        should contaain two separate plotting frames.


# 3. (a) Go through ggplot2 options and make notes to yourself
#        on at least 5 options you want to use.




