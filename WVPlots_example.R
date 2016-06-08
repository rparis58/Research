library(ggplot2)
library(ggExtra)
frm = read.csv("tips.csv")

plot_center = ggplot(frm, aes(x=total_bill,y=tip)) + 
  geom_point() +
  geom_smooth(method="lm")

# default: type="density"
ggMarginal(plot_center, type="histogram")

library(WVPlots)
frm = read.csv("tips.csv")

ScatterHist(frm, "total_bill", "tip",
            smoothmethod="lm",
            annot_size=3,
            title="Tips vs. Total Bill")
