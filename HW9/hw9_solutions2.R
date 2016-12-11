load("~/Downloads/weather2011.rda")

makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
               margins, cityName, xtop = TRUE) {
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
  
  par(bg = bgcolor, mar = margins)
  plot(0,0,xlim = xlim, ylim = ylim, ylab = "", xlab = "", 
       frame.plot = F, axes = F, col.lab = "darkgray")
  a = ifelse(ylim[2]>10, 10, 1)
  at = seq(ylim[1], ylim[2], a)
  
  axis(side = 2, at = at, tck = 0, lty = 1, las = 2, col = "bisque4",cex.axis = 0.5,
      labels = ylabels, lwd = 2, pos = 0, col.axis = "dimgray", font.axis = 2)
  t = 0
  at2 = {}
  daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                  31, 30, 31, 30, 31)
  monthNames = c("January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December")
  
  for(d in daysInMonth){
    t = t+d/2
    at2 = c(at2, t)
    t = t+d/2
  }
  side = ifelse(xtop,3,1)
  axis(side = side, at= at2,labels = monthNames, lwd = 0, cex.axis = .75, 
       font.axis = 2, col.axis = "dimgray", line = -1)
  axis(side = 4, at= at,tck = 0, lty = 1, las = 2, col = "bisque4", cex.axis = 0.5,
       labels = ylabels, lwd = 2, pos = 365, col.axis = "gray24", font.axis = 2)
  
}


drawTempRegion = function(day, high, low, col){
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record temps, normal temps, and 
  # observed temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high temperatures
  # low - a numeric vector of 365 low temperatures
  # col - color to fill the rectangles
  
  par(new= T)
  lines(rep(day,2), c(low,high),
       type = "l" , col = col, lwd = 2)
}

addGrid = function(location, col, ltype, vertical = TRUE) {
  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both temp and precip plots
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  par(new = T)
  
  if(vertical){
    abline(h = NULL, v = location, col = col, lty = ltype, lwd = 0.01)
  }
  else{
    abline(h = location, v =NULL, col = col, lty = ltype, lwd = 0.01)
  }
  
}


monthPrecip = function(day, dailyprecip, normal){
  # This function adds one month's precipitation to the 
  #   precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative precipitation curve,
  # fills the area below with color, add the total
  # precipitation for the month, and adds a reference
  # line and text for the normal value for the month
  
  # day a numeric vector of dates for the month
  # dailyprecip a numeric vector of precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total precip
  #  for the month
  
  p = cumsum(c(dailyprecip))
  normal = as.numeric(as.character(normal))
  x = c(day, rev(day))
  y = c(p, rep(0, length(p)))
  par(new = T)
  polygon(x,y,col = "bisque3", border = NA )
  lines(day, p, lwd = 4, col = "darkcyan")
  lines(c(min(day), max(day)), c(normal, normal), col = "cyan", lwd = 2)
  text(x= day[1], y = normal+.2, as.character(normal), col = "dimgray", cex = 0.5, pos = 4)
  text(x= day[length(day)], y = p[length(p)]+.2, as.character(p[length(p)]), col = "dimgray", cex = 0.5, pos = 2)
}

finalPlot = function(temp, precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or temp
  # precip is the data frame sfoMonthlyPrecip or precip

  
  # Here are some vectors that you might find handy
  
  daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                  31, 30, 31, 30, 31)
  
  cumDays = cumsum(c(0, daysInMonth))
  cumDays = cumDays[c(-1,-13)]
  cityName = "Los Angeles"
  
  ### Fill in the various stages with your code
 
  
  ### Add any additional variables that you will need here
  
  min_all = (min(c(min(temp$RecordLow), min(temp$NormalLow  ), min(temp$RecordLow)))%/%10)*10- 10
  max_all = (max(c(max(temp$RecordHigh), min(temp$NormalHigh  ), min(temp$RecordHigh)))%/%10)*10 +10
  
  y = seq(min_all, max_all, 10)
  text = paste(y, expression(~degree), sep = "")
  labels = parse(text = text)
  
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  pdf("weather_report.pdf", width = 12 , height = 6.5)
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights = c(2.5,1))
  
  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  
  makePlotRegion(xlim = c(0,366), ylim = c(min_all, max_all), bgcolor = "whitesmoke", 
                 ylabels = labels,margins = c(0,1,3,1), cityName = cityName, xtop = TRUE)
  
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  for(i in 1:length(temp$Day))
  {
    drawTempRegion(i, temp$RecordLow[i], temp$RecordHigh[i], "bisque3")
    drawTempRegion(i, temp$NormalLow[i], temp$NormalHigh[i], "bisque4")
    drawTempRegion(i, temp$Low[i], temp$High[i], "brown4")
  }
    
  ### Call addGrid to add the grid lines to the plot
  addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
  addGrid(location = y, col = "whitesmoke", ltype = 1, vertical = FALSE )
  
  ### Add the markers for the record breaking days
  low = which.min(temp$RecordLow)
  low_msg = paste(temp$RecordLow[low],expression(~degree), sep = "")
  low_msg = parse(text = low_msg)
  legend(x = low+0.5,y=temp$RecordLow[low]-28,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
         legend =c("                      ",
                   "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
  text(x = low, y = temp$RecordLow[low]-0.5, 
       "Record Low", pos = 4, cex = 0.5, col = "gray24" ,font = 2)
  text(x = low, y = temp$RecordLow[low]-2.5, 
        low_msg, pos = 4, cex = 0.5, col = "gray24" ,font = 2)
  
  high = which.max(temp$RecordHigh)
  high_msg = paste(temp$RecordHigh[high],expression(~degree), sep = "")
  high_msg = parse(text = high_msg, keep.source = F)
  legend(x = high+0.5,y=temp$RecordHigh[high]-1,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
         legend =c("                      ",
                   "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
   text(x = high, y = temp$RecordHigh[high]+2.5, "Record High",
         cex = .5, col = "gray24",
       bg = "whitesmoke", font = 2, pos = 4)
  text(x = high, y = temp$RecordHigh[high]+.5, high_msg,
       cex = .5, col = "gray24",
       bg = "whitesmoke", font = 2, pos = 4)

avg =    mean(mean(temp$High[!is.na(temp$High)]), mean(temp$Low[!is.na(temp$Low)]))
avg = round(avg, 2)

# legend(x = x,y=5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
#        legend =c("                                    ",
#                  "                                    ",
#                  "                                     "), adj = 0, xjust = 0, pch = NA, yjust = 0)

col_line = c("bisque3", "bisque4", "brown4")
x = 365/2
j = 35
n = 1
for(i in seq(10, 20, 5)){
par(new= T)
lines(rep(x,2), c(i,j),
      type = "l" , col = col_line[n], lwd = 5)
j = j - 5
n= n+1
}


text(x = x+1,y=10,"Record Low",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=35,"Record High",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=20,"Actual Low",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2)
text(x = x+1,y=25,"Actual High",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3,y=22.5,"Normal Range",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3.2,y=22.5,"[",
     bg = "whitesmoke", pos = 4, cex = 1.5, col = "brown4")

text(x = x+1, y=22.5,"[",
     bg = "whitesmoke", pos = 2, cex = 3, col = "bisque4")




  
  ### Add the titles    
title(main = paste(cityName,"' Weather in 2011", sep = ""), 
      font.main = 2, col.main = "gray24", cex.main = 1.2, adj = 0, line = 2)

legend(x = 0.5,y=108.5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                                    ",
                 "                                    "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = 1,y=118,"Temperature",
     bg = "whitesmoke", pos = 4, cex = .75, col = "gray24", font = 2) 
text(x = 1,y=113,"Bars represent the range between the daily high and low.",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 
text(x = 1,y=110,parse(text = paste("Average~temperature~this~year~was~",
                       avg,"~degree", sep = "")),
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 
  
  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
labels2 = seq(0,5,1)
  
  makePlotRegion(xlim = c(0,366), ylim = c(0,5), bgcolor = "whitesmoke", 
                 ylabels = labels2,margins = c(2,1,1,1), cityName = "Los Angeles", xtop = FALSE)
  
  ### Call monthPrecip 12 times to create each months 
  ### cumulative precipitation plot. To do this use 
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             }) 
  ### the anonymous function calls monthPrecip with the 
  ### appropriate arguments
  
 temp$Precip[is.na(temp$Precip)] = 0
  
  for(i in 1:12)
  {
    days = which(temp$Month == i)
    normprecip = temp$Precip[days]
    monthPrecip(days, normprecip, precip$normal[i])
    
  }
  
  ### Call addGrid to add the grid lines to the plot
  
  addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
  addGrid(location = labels2, col = "whitesmoke", ltype = 1, vertical = FALSE )
  
  ### Add the titles
  text(x = 0,y=5.5,"Precipitation",
        pos = 4, cex = .75, col = "gray24", font = 2, xpd = T) 
  text(x = 32,y=5.5,paste("Cumulative monthly precipitaion in inches compared with normal monthly precipitation. Total precipitaion in 2011 was ", sum(temp$Precip)," inches." ,sep = ""),
       pos = 4, cex = .5, col = "dimgrey", xpd = T) 
  
  
  ### Close the pdf device dev.off()
  dev.off()
  
}

### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)

finalPlot(temp = laxWeather, precip = laxMonthlyPrecip)



################### Extra Credit  ##############################


daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                31, 30, 31, 30, 31)

cumDays = cumsum(c(0, daysInMonth))
cumDays = cumDays[c(-1,-13)]
cityName = "San Fransisco"

### Fill in the various stages with your code

temp = sfoWeather
precip = sfoMonthlyPrecip
### Add any additional variables that you will need here

min_all = (min(c(min(temp$RecordLow), min(temp$NormalLow  ), min(temp$RecordLow)))%/%10)*10- 10
max_all = (max(c(max(temp$RecordHigh), min(temp$NormalHigh  ), min(temp$RecordHigh)))%/%10)*10 +10

y = seq(min_all, max_all, 10)
text = paste(y, expression(~degree), sep = "")
labels = parse(text = text)

### Set up the graphics device to plot to pdf and layout
### the two plots on one canvas
### pdf("", width = , height = )
### layout(  )
pdf("weather_report_combined.pdf", width = 24 , height = 13)
layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE), heights = c(2.5,1,2.5,1))

### Call makePlotRegion to create the plotting region
### for the temperature plot

makePlotRegion(xlim = c(0,366), ylim = c(min_all, max_all), bgcolor = "whitesmoke", 
               ylabels = labels,margins = c(0,1,3,1), cityName = cityName, xtop = TRUE)

### Call drawTempRegion 3 times to add the rectangles for
### the record, normal, and observed temps
for(i in 1:length(temp$Day))
{
  drawTempRegion(i, temp$RecordLow[i], temp$RecordHigh[i], "bisque3")
  drawTempRegion(i, temp$NormalLow[i], temp$NormalHigh[i], "bisque4")
  drawTempRegion(i, temp$Low[i], temp$High[i], "brown4")
}

### Call addGrid to add the grid lines to the plot
addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
addGrid(location = y, col = "whitesmoke", ltype = 1, vertical = FALSE )

### Add the markers for the record breaking days
low = which.min(temp$RecordLow)
low_msg = paste(temp$RecordLow[low],expression(~degree), sep = "")
low_msg = parse(text = low_msg)
legend(x = low+0.5,y=temp$RecordLow[low]-28,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                      ",
                 "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = low, y = temp$RecordLow[low]-0.5, 
     "Record Low", pos = 4, cex = 0.5, col = "gray24" ,font = 2)
text(x = low, y = temp$RecordLow[low]-2.5, 
     low_msg, pos = 4, cex = 0.5, col = "gray24" ,font = 2)

high = which.max(temp$RecordHigh)
high_msg = paste(temp$RecordHigh[high],expression(~degree), sep = "")
high_msg = parse(text = high_msg, keep.source = F)
legend(x = high+0.5,y=temp$RecordHigh[high]-1,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                      ",
                 "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = high, y = temp$RecordHigh[high]+2.5, "Record High",
     cex = .5, col = "gray24",
     bg = "whitesmoke", font = 2, pos = 4)
text(x = high, y = temp$RecordHigh[high]+.5, high_msg,
     cex = .5, col = "gray24",
     bg = "whitesmoke", font = 2, pos = 4)

avg =    mean(mean(temp$High[!is.na(temp$High)]), mean(temp$Low[!is.na(temp$Low)]))
avg = round(avg, 2)

# legend(x = x,y=5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
#        legend =c("                                    ",
#                  "                                    ",
#                  "                                     "), adj = 0, xjust = 0, pch = NA, yjust = 0)

col_line = c("bisque3", "bisque4", "brown4")
x = 365/2
j = 35
n = 1
for(i in seq(10, 20, 5)){
  par(new= T)
  lines(rep(x,2), c(i,j),
        type = "l" , col = col_line[n], lwd = 5)
  j = j - 5
  n= n+1
}


text(x = x+1,y=10,"Record Low",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=35,"Record High",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=20,"Actual Low",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2)
text(x = x+1,y=25,"Actual High",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3,y=22.5,"Normal Range",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3.2,y=22.5,"[",
     bg = "whitesmoke", pos = 4, cex = 1.5, col = "brown4")

text(x = x+1, y=22.5,"[",
     bg = "whitesmoke", pos = 2, cex = 3, col = "bisque4")





### Add the titles    
title(main = paste(cityName,"' Weather in 2011", sep = ""), 
      font.main = 2, col.main = "gray24", cex.main = 1.2, adj = 0, line = 2)

legend(x = 0.5,y=108.5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                                    ",
                 "                                    "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = 1,y=118,"Temperature",
     bg = "whitesmoke", pos = 4, cex = .75, col = "gray24", font = 2) 
text(x = 1,y=113,"Bars represent the range between the daily high and low.",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 
text(x = 1,y=110,parse(text = paste("Average~temperature~this~year~was~",
                                    avg,"~degree", sep = "")),
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 

### Call makePlotRegion to create the plotting region
### for the precipitation plot
labels2 = seq(0,5,1)




makePlotRegion(xlim = c(0,366), ylim = c(0,5), bgcolor = "whitesmoke", 
               ylabels = labels2,margins = c(2,1,1,1), cityName = "Los Angeles", xtop = FALSE)

### Call monthPrecip 12 times to create each months 
### cumulative precipitation plot. To do this use 
### sapply(1:12, function(m) {
###             code
###             monthPrecip(XXXX)
###             }) 
### the anonymous function calls monthPrecip with the 
### appropriate arguments

temp$Precip[is.na(temp$Precip)] = 0

for(i in 1:12)
{
  days = which(temp$Month == i)
  normprecip = temp$Precip[days]
  monthPrecip(days, normprecip, precip$normal[i])
  
}

### Call addGrid to add the grid lines to the plot

addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
addGrid(location = labels2, col = "whitesmoke", ltype = 1, vertical = FALSE )

### Add the titles
text(x = 0,y=5.5,"Precipitation",
     pos = 4, cex = .75, col = "gray24", font = 2, xpd = T) 
text(x = 32,y=5.5,paste("Cumulative monthly precipitaion in inches compared with normal monthly precipitation. Total precipitaion in 2011 was ", sum(temp$Precip)," inches." ,sep = ""),
     pos = 4, cex = .5, col = "dimgrey", xpd = T) 

temp = laxWeather
precip = laxMonthlyPrecip
cityName = "San Fransisco"

makePlotRegion(xlim = c(0,366), ylim = c(min_all, max_all), bgcolor = "whitesmoke", 
               ylabels = labels,margins = c(0,1,3,1), cityName = cityName, xtop = TRUE)

### Call drawTempRegion 3 times to add the rectangles for
### the record, normal, and observed temps
for(i in 1:length(temp$Day))
{
  drawTempRegion(i, temp$RecordLow[i], temp$RecordHigh[i], "bisque3")
  drawTempRegion(i, temp$NormalLow[i], temp$NormalHigh[i], "bisque4")
  drawTempRegion(i, temp$Low[i], temp$High[i], "brown4")
}

### Call addGrid to add the grid lines to the plot
addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
addGrid(location = y, col = "whitesmoke", ltype = 1, vertical = FALSE )

### Add the markers for the record breaking days
low = which.min(temp$RecordLow)
low_msg = paste(temp$RecordLow[low],expression(~degree), sep = "")
low_msg = parse(text = low_msg)
legend(x = low+0.5,y=temp$RecordLow[low]-28,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                      ",
                 "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = low, y = temp$RecordLow[low]-0.5, 
     "Record Low", pos = 4, cex = 0.5, col = "gray24" ,font = 2)
text(x = low, y = temp$RecordLow[low]-2.5, 
     low_msg, pos = 4, cex = 0.5, col = "gray24" ,font = 2)

high = which.max(temp$RecordHigh)
high_msg = paste(temp$RecordHigh[high],expression(~degree), sep = "")
high_msg = parse(text = high_msg, keep.source = F)
legend(x = high+0.5,y=temp$RecordHigh[high]-1,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                      ",
                 "                      "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = high, y = temp$RecordHigh[high]+2.5, "Record High",
     cex = .5, col = "gray24",
     bg = "whitesmoke", font = 2, pos = 4)
text(x = high, y = temp$RecordHigh[high]+.5, high_msg,
     cex = .5, col = "gray24",
     bg = "whitesmoke", font = 2, pos = 4)

avg =    mean(mean(temp$High[!is.na(temp$High)]), mean(temp$Low[!is.na(temp$Low)]))
avg = round(avg, 2)

# legend(x = x,y=5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
#        legend =c("                                    ",
#                  "                                    ",
#                  "                                     "), adj = 0, xjust = 0, pch = NA, yjust = 0)

col_line = c("bisque3", "bisque4", "brown4")
x = 365/2
j = 35
n = 1
for(i in seq(10, 20, 5)){
  par(new= T)
  lines(rep(x,2), c(i,j),
        type = "l" , col = col_line[n], lwd = 5)
  j = j - 5
  n= n+1
}


text(x = x+1,y=10,"Record Low",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=35,"Record High",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x+1,y=20,"Actual Low",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2)
text(x = x+1,y=25,"Actual High",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3,y=22.5,"Normal Range",
     bg = "whitesmoke", pos = 2, cex = .5, col = "dimgrey", font = 2) 
text(x = x-3.2,y=22.5,"[",
     bg = "whitesmoke", pos = 4, cex = 1.5, col = "brown4")

text(x = x+1, y=22.5,"[",
     bg = "whitesmoke", pos = 2, cex = 3, col = "bisque4")





### Add the titles    
title(main = paste(cityName,"' Weather in 2011", sep = ""), 
      font.main = 2, col.main = "gray24", cex.main = 1.2, adj = 0, line = 2)

legend(x = 0.5,y=108.5,box.lwd = 0, box.col = "whitesmoke",bg = "whitesmoke",seg.len = 0, lwd = NA,
       legend =c("                                    ",
                 "                                    "), adj = 0, xjust = 0, pch = NA, yjust = 0)
text(x = 1,y=118,"Temperature",
     bg = "whitesmoke", pos = 4, cex = .75, col = "gray24", font = 2) 
text(x = 1,y=113,"Bars represent the range between the daily high and low.",
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 
text(x = 1,y=110,parse(text = paste("Average~temperature~this~year~was~",
                                    avg,"~degree", sep = "")),
     bg = "whitesmoke", pos = 4, cex = .5, col = "dimgrey" ) 

### Call makePlotRegion to create the plotting region
### for the precipitation plot
labels2 = seq(0,5,1)

makePlotRegion(xlim = c(0,366), ylim = c(0,5), bgcolor = "whitesmoke", 
               ylabels = labels2,margins = c(2,1,1,1), cityName = "Los Angeles", xtop = FALSE)

### Call monthPrecip 12 times to create each months 
### cumulative precipitation plot. To do this use 
### sapply(1:12, function(m) {
###             code
###             monthPrecip(XXXX)
###             }) 
### the anonymous function calls monthPrecip with the 
### appropriate arguments

temp$Precip[is.na(temp$Precip)] = 0

for(i in 1:12)
{
  days = which(temp$Month == i)
  normprecip = temp$Precip[days]
  monthPrecip(days, normprecip, precip$normal[i])
  
}

### Call addGrid to add the grid lines to the plot

addGrid(location = cumDays, col = "dimgrey", ltype = 3, vertical = TRUE )
addGrid(location = labels2, col = "whitesmoke", ltype = 1, vertical = FALSE )

### Add the titles
text(x = 0,y=5.5,"Precipitation",
     pos = 4, cex = .75, col = "gray24", font = 2, xpd = T) 
text(x = 32,y=5.5,paste("Cumulative monthly precipitaion in inches compared with normal monthly precipitation. Total precipitaion in 2011 was ", sum(temp$Precip)," inches." ,sep = ""),
     pos = 4, cex = .5, col = "dimgrey", xpd = T) 



### Close the pdf device dev.off()
dev.off()



