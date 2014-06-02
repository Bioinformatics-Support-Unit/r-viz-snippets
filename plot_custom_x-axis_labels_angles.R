##'========================================================================#
##'Description: Function to add custom x-axis labels at custom Angle       #
##'                                                                        #
##'Takes a vector of strings and adds custom X axis labels at 90 degrees   #
##'srt - Angle of Text                                                     #
##'Will work with objects derrived from parent plot() function             #
##'Original plot call must include xaxt="n" to disable drawing of          #
##'default x-axis                                                          #
##'========================================================================#
x_lab_write <- function(x_labs)
{
  axis(1, at=seq(1, length(x_labs), by=1), labels = FALSE)
  text(seq(1+.25, length(x_labs)+.75, by=1), par("usr")[3]-.4,
       labels = x_labs, srt = -90, pos = 1, xpd = TRUE, cex=0.4)
}
