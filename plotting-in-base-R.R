# Plotting in base R
# QAEco coding club Thursday 7 March

# we're going to work with a function to make an elephant
# (see Mayer et al. 2010 [Am. J. Phys. 78:648-649] for details)
source("elephant-function.R")

# let's make an elephant
elephant_parameters <- c(50 - 30i, 18 + 8i, 12 - 10i, -14 - 60i, 40 + 20i)
my_elephant <- elephant(seq(0, 2 * pi, length = 1000), elephant_parameters)

# start with a basic plot
plot(my_elephant$y ~ my_elephant$x)

# we can do better than this
plot(my_elephant$y ~ my_elephant$x,
     xaxt = "n", yaxt = "n",    # get rid of the axes
     bty = "n",                 # remove the bounding box
     xlab = "", ylab = "")      # remove axis labels

# add an eye for the elephant
points(my_elephant$eye_y ~ my_elephant$eye_x,
       col = "black", cex = 1.2)

# maybe the eye would be better as a solid point?
points(my_elephant$eye_y ~ my_elephant$eye_x,
       col = "black", cex = 1.2, pch = 16)

# what if we'd rather plot the elephant with lines rather than points?
plot(my_elephant$y ~ my_elephant$x,
     xaxt = "n", yaxt = "n",
     bty = "n",
     xlab = "", ylab = "",
     type = "l")             # tell R to plot a line "l" not points "p"

# is the line thick enough?
plot(my_elephant$y ~ my_elephant$x,
     xaxt = "n", yaxt = "n",
     bty = "n",
     xlab = "", ylab = "",
     type = "l",
     lwd = 3)                # we can set the line width (lwd)

# sometimes we might prefer dashed lines
plot(my_elephant$y ~ my_elephant$x,
     xaxt = "n", yaxt = "n",
     bty = "n",
     xlab = "", ylab = "",
     type = "l",
     lwd = 3,
     lty = 2)                # change the line type from solid (lty = 1) to dashed (lty = 2)

# don't forget its eye!
points(my_elephant$eye_y ~ my_elephant$eye_x,
       col = "black", cex = 1.2, pch = 16)

# what if we'd rather add a filled shape?
plot(my_elephant$y ~ my_elephant$x,
     xaxt = "n", yaxt = "n",
     bty = "n",
     xlab = "", ylab = "",
     type = "n")
polygon(my_elephant$y ~ my_elephant$x,
        border = NA, col = "gray50")
points(my_elephant$eye_y ~ my_elephant$eye_x,
       pch = 16, col = "black", cex = 1.2)

# it can be useful to have repeated steps in a function
elephant_plot <- function(elephant, col = "gray50", eye_colour = "black", ...) {
  plot(elephant$y ~ elephant$x,
       xaxt = "n", yaxt = "n",
       bty = "n",
       xlab = "", ylab = "",
       type = "n",
       ...)
  polygon(elephant$y ~ elephant$x,
          border = NA, col = col)
  points(elephant$eye_y ~ elephant$eye_x,
         pch = 16, col = eye_colour, cex = 1.2)
}

# now we just need one line to make this plot
elephant_plot(my_elephant)

# and we can easily change the colour to whatever we want
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")

# what if we want to add some info to the plot?
mtext("This is an elephant", side = 1, adj = 0.5, line = 2, cex = 1.2)
mtext("This is its trunk", side = 4, adj = 0.5, line = 0, cex = 1.2)

# maybe we think axes are important?
axis(1)
axis(2)

# those are pretty bad, let's make them a bit cleaner
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")
axis(1, at = c(-60, -15, 50), labels = c("Back", "Middle", "Front"),
     lwd.ticks = 0)

# or what about this?
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")
axis(1, at = c(-60, -15, 50), labels = c("Back", "Middle", "Front"),
     tick = FALSE)

# but we could also do this with mtext
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")
mtext(c("Back", "Middle", "Front"), side = 1, adj = c(0.1, 0.5, 0.9))

# what if we want to shrink the margins around our plot?
par(mar = c(2, 1, 1, 1))
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")

# if we try to plot text really far out, it won't show anymore
mtext(c("Back", "Middle", "Front"), side = 1, adj = c(0.1, 0.5, 0.9),
      line = 4)

# but we can still add it within the margins
mtext(c("Back", "Middle", "Front"), side = 1, adj = c(0.1, 0.5, 0.9),
      line = 0)

# what is par()?
?par

# I changed the margin width earlier, it's a good idea to set it back to a sensible value
par(mar = c(5.1, 4.1, 4.1, 2.1))

# sometimes we might want multiple panels in one plot
par(mfrow = c(2, 2))
for (i in 1:4) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# this does almost the same thing, with one key difference
par(mfcol = c(2, 2))
for (i in 1:4) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# can we create this an alternative way?
layout_matrix <- rbind(c(1, 2), c(3, 4))
layout(mat = layout_matrix)
layout.show(n = 4)
for (i in 1:4) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# why would we use this?
# sometimes we don't want all rows/columns to be the same
layout_matrix <- rbind(c(1, 2), c(3, 4))
layout(mat = layout_matrix,
       heights = c(0.5, 1), widths = c(1, 1))
layout.show(n = 4)
for (i in 1:4) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# and sometimes we don't want all plots to fill the same space
layout_matrix <- rbind(c(1, 1), c(2, 3))
layout(mat = layout_matrix)
layout.show(n = 3)
for (i in 1:3) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# or some combination of these
layout_matrix <- rbind(c(1, 1), c(2, 3))
layout(mat = layout_matrix, 
       widths = c(0.5, 1), heights = c(1, 0.5))
layout.show(n = 3)
for (i in 1:3) {
  hist(rnorm(100), main = paste0("Histogram ", i))
}

# where does R put my plot?
dev.cur()      # where is R currently plotting?
dev.list()     # where could it plot?
dev.new()      # let's make a new plotting device
dev.off()      # I'm bored with that one, let's turn it off

# we can use these ideas to plot directly to a pdf if we want
pdf(file = "my_elephant_plot.pdf", height = 7, width = 7)
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")

# where is this plotting?
dev.cur()

# we need to turn this off or we'll just keep plotting there
dev.off()

# we don't have to make our plot a PDF, can use bitmap formats too
# tiff()
# jpeg()
## NOTE: the differences in heights and widths (why is this?)
png(file = "my_elephant_plot.png", width = 480, height = 480)
elephant_plot(my_elephant,
              col = scales::alpha("red", 0.5),
              eye_colour = "red")

# don't forget to turn off the png device
dev.off()

# Task: make me four elephants with different colours, each in its own panel
#       (or make a plot of your choosing with your own data)

# Major task: sign up to lead future sessions!
