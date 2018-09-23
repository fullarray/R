
autos_data<-read.table("autos.dat",header=T, sep="\t")


max_y<-max(autos_data)

plot_colors<-c("blue","red","green")

png(filename = "./images/figure.png", height = 295, width = 300, bg="white")

plot(autos_data$cars, type="o", col=plot_colors[1],
ylim=c(0,max_y), axes=FALSE, ann=FALSE)

axis(1, at=1:5, lab=c("Mon", "Tue", "Wed", "Thu", "Fri"))

axis(2, las=1, at=4*0:max_y)

box()


lines(autos_data$trucks, type="o", pch=22, lty=2,
col=plot_colors[2])
lines(autos_data$suvs, type="o", pch=23, lty=3,
col=plot_colors[3])


title(main="Autos", col.main="red", font.main=4)

title(xlab= "Days", col.lab=rgb(0,0.5,0))

title(ylab= "Total", col.lab=rgb(0,0.5,0))

legend(1, max_y, names(autos_data), cex=0.8, col=plot_colors,
pch=21:23, lty=1:3)

dev.off()
