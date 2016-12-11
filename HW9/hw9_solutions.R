monthNames = c("JANUARY", "FEBRUARY", "MARCH", "APRIL",
               "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER",
               "OCTOBER", "NOVEMBER", "DECEMBER")
daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                31, 30, 31, 30, 31)
cumDays = cumsum(c(1, daysInMonth))

makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
                          margins, cityName, xtop = TRUE){
  par(bg = bgcolor, adj=0, mar=margins, lwd = 1.5)
  plot(NULL, type = "n", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n",
       xaxs = "i", axes = FALSE)
  if(xtop){
    for(i in 1:12){
      mtext(side = 3, at = (cumDays[i+1] + cumDays[i])/2, text = monthNames[i], cex = 0.8, font = 2)
      axis(side = 2, las=2, lwd = 3, cex.axis=0.8, col = "#99928c", col.axis = "#635d57", at=ylabels, labels = parse(text = paste(ylabels, "*degree", sep = "")), col.ticks=bgcolor, pos=xlim[1]-0.4)
      axis(side = 4, las=2, lwd = 3, cex.axis=0.8, col = "#99928c", col.axis = "#635d57", at=ylabels, labels = parse(text = paste(ylabels, "*degree", sep = "")), col.ticks=bgcolor, pos=xlim[2]+0.4)
    }
  }else{
    for(i in 1:12){
      mtext(side = 1, at = (cumDays[i+1] + cumDays[i])/2, text = monthNames[i], cex = 0.8, font = 2)
      axis(side = 2, las=2, lwd = 3, cex.axis=0.8, col = "#99928c", col.axis = "#635d57", at=ylabels, col.ticks=bgcolor, pos=xlim[1]-0.4)
      axis(side = 4, las=2, lwd = 3, cex.axis=0.8, col = "#99928c", col.axis = "#635d57", at=ylabels, col.ticks=bgcolor, pos=xlim[2]+0.4)
    }
  }
}

  # This function is to produce a blank plot that has 
  # the proper axes labels, background color, etc.
  # It is to be used for both the top and bottom plot.

  # The parameters are
  # xlim is a two element numeric vector used for the two
  #   end points of the x axis
  # ylim is the same as xlim, but for the y axis
  # ylabels is a numeric vector of labels for "tick marks"
  #   on the y axis
  # We don't need to x labels because they are Month names
  # margins specifies the size of the plot margins (see mar parameter in par)
  # cityName is a character string to use in the title
  # xtop indicates whether the month names are to appear
  # at the top of the plot or the bottom of the plot
  # 
  # See the assignment for a pdf image of the plot that is
  # produced as a result of calling this function.

drawTempRegion = function(day, high, low, col){
  
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record temps, normal temps, and 
  # observed temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high temperatures
  # low - a numeric vector of 365 low temperatures
  # col - color to fill the rectangles
  
  rect(day-0.5, low, day+0.5, high, col = col, border = NA)
}

  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both temp and precip plots

addGrid = function(location, col, ltype, vertical = TRUE) {
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  
  if (vertical) {
    abline(v = location, col = col, lty = ltype)
  } else {
    abline(h = location, col = col, lty = ltype)
  }
}

  # This function adds one month's precipitation to the 
  #   precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative precipitation curve,
  # fills the area below with color, add the total
  # precipitation for the month, and adds a reference
  # line and text for the normal value for the month

monthPrecip = function(day, dailyprecip, normal){
  is.na(dailyprecip) = 0
  polygon(x = c(day, max(day), day[1]), y = c(cumsum(dailyprecip), 0, 0), col = "#e8d8be", border = NA)
  lines(day, cumsum(dailyprecip), col = "#0d5968", lwd = 4)
  lines(day, rep(normal,length(day)), col = "#89bfc6", lwd = 3.5)
  text(day[1]+2, normal+.3, labels =  normal, cex = 0.8, font = 3)
  text(max(day)-0.1, sum(dailyprecip)+0.25, labels = sum(dailyprecip), cex = 0.8, pos = 2)
}
  

  # day a numeric vector of dates for the month
  # dailyprecip a numeric vector of precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total precip
  #  for the month

finalPlot = function(temp, precip){
  
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip
  
  
  # Here are some vectors that you might find handy
  
  normPrecip = as.numeric(as.character(precip$normal))
  
  ### Fill in the various stages with your code
  
  
  ### Add any additional variables that you will need here
  
  temp$Precip[is.na(temp$Precip)] = 0
  recordPrecip = temp$RecordPrecip
  recordHigh = temp$RecordHigh
  recordLow = temp$RecordLow
  normalHigh = temp$NormalHigh
  normalLow = temp$NormalLow
  tiedHigh = temp$High[which(recordHigh == temp$High)]
  tiedLow = temp$Low[which(recordLow == temp$Low)]
  tiedHighday = which(recordHigh == temp$High)
  tiedLowday = which(recordLow == temp$Low)
  
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  
  pdf("HW9_Plot", width = 17, height = 12)
  layout(matrix(c(1,2), nrow = 2, ncol = 1, byrow = TRUE), height = c(2,1))
  
  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  
  makePlotRegion(xlim = c(1,365), ylim = c(20, 110), bgcolor = "#ededea", 
                 ylabels = seq(20, 110, 10), margins = c(4,3.5,5,3.5), cityName = "", xtop = TRUE)
  
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  
  drawTempRegion(c(1:365), recordHigh, recordLow, col = "#e8d8be")
  drawTempRegion(c(1:365), normalHigh, normalLow, col = "#a59789")
  drawTempRegion(c(1:365), temp$High, temp$Low, col = "#681746")
  
  ### Call addGrid to add the grid lines to the plot
  
  addGrid(cumDays[2:12]-1, col = "#635d57", lty = 3, TRUE)
  addGrid(seq(20, 110, 10), col = "#ededea", lty = 1, FALSE)
  
  ### Add the markers for the record breaking days
  
  text(tiedHighday[which(tiedHigh == max(tiedHigh))] + 1, max(tiedHigh) + 10, 
       labels = paste("RECORD HIGH: ", max(tiedHigh)), cex = 0.75)
  segments(tiedHighday[which(tiedHigh == max(tiedHigh))], max(tiedHigh), y1 = max(tiedHigh) + 10)
  
  text(tiedLowday[which(tiedLow == min(tiedLow))] + 1, min(tiedLow) - 10, 
       labels = paste("TIED RECORD LOW:", min(tiedLow)), cex = 0.75)
  segments(tiedLowday[which(tiedLow == min(tiedLow))], min(tiedLow), y1 = min(tiedLow) - 10)
  
  ### Add the titles 
  
  title(main = list("San Francisco's Weather in 2011", cex = 2.1), adj=0)
  text(5, 108, labels = "Temperature", cex = 1.5, font = 2)
  text(5, 102.5, labels = "Bars represent range between the", cex = 1.2)
  text(5, 99, labels = "daily high and low.", cex = 1.2)
  segments(190, 23, y1 = 38, col = "#e8d8be", lwd = 7)
  text(187, 23, labels = "RECORD LOW", adj = 1, cex = 0.75)
  text(187, 38, labels = "RECORD HIGH", adj = 1, cex = 0.75)
  segments(190, 28, y1 = 33, col = "#a59789", lwd = 7)
  segments(c(188, 186.5, 186.5), c(33,33,28), x1 = c(186.5,186.5,188), y1 = c(33,28,28))
  text(185.5, 30, labels="NORMAL RANGE", adj = 1, cex = 0.75)
  segments(190, 30, y1 = 35, col = "#681746", lwd = 4)
  segments(191, 30, x1 = 192.5)
  text(193.5, 30, labels = "ACTUAL LOW", cex = 0.75)
  segments(191, 35, x1 = 192.5)
  text(193.5, 35, labels = "ACTUAL HIGH", cex = 0.75)
  
  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
  
  makePlotRegion(xlim = c(1, 365), ylim = c(0, 6), bgcolor = "#ededea",
                 ylabels = seq(0, 6, 1), margins = c(4,3.5,5,3.5), cityName = "", xtop = FALSE)
  
  ### Call monthPrecip 12 times to create each months 
  ### cumulative precipitation plot. To do this use 
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             }) 
  ### the anonymous function calls monthPrecip with the 
  ### appropriate arguments
  
  sapply(1:12, function(m){
    monthPrecip(day = cumDays[m]:(cumDays[m+1]-1),
                dailyprecip = temp$Precip[cumDays[m]:(cumDays[m+1]-1)],
                normal = normPrecip[m])
  })
  
  ### Call addGrid to add the grid lines to the plot
  
  addGrid(cumDays[2:12]-1, col = "#635d57", lty = 3, TRUE)
  addGrid(seq(0, 5, 1), col = "#ededea", lty = 1, FALSE)
  
  ### Add the titles
  
  title(main = list("Precipitation", cex=1.5, font=2))
  mtext(side = 3, line = 2, text = paste("Cumulative monthly precipitation in inches compared with normal monthly precipitation. Total precipitation in 2011 was", sum(temp$Precip), "inches."))
  text(x=3, y=5., labels = "NORMAL", cex = 0.8, font = 3)
  text(x=9, y=1.24, labels = "ACTUAL", cex = 0.8)
  
  ### Close the pdf device dev.off()
  dev.off()
}

### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)