# Gabe's country aid graphs


################################################################################
# SETUP AND DATA MANAGEMENT
################################################################################

library(ggplot2)
library(reshape2)
library(dplyr)
GabeData <- read.csv("K:/Development/ForeignAssistance/Aid Graphs/GabeData.csv")
GabeData$FY <- as.factor(GabeData$FY)

# Calculate CAGR
CAGR <- vector(mode="numeric")
for(i in 1:6){
      Reg <- as.character(GabeData$Region[i])
      IVall <- GabeData[GabeData$Region == Reg & GabeData$FY == "2008", "All"]
      FVall <- GabeData[GabeData$Region == Reg & GabeData$FY == "2017", "All"]
      Nall <- length(unique(GabeData$FY)) - 1
      CAGR[i] <- ((FVall / IVall)^(1/Nall)-1) * 100
}
GabeData$CAGR = CAGR

# Calculate CAGR (deflated)
CAGRdef <- vector(mode="numeric")
for(i in 1:6){
      Reg <- as.character(GabeData$Region[i])
      IVall <- GabeData[GabeData$Region == Reg & GabeData$FY == "2008", "All"]
      IVall <- IVall / .9981
      FVall <- GabeData[GabeData$Region == Reg & GabeData$FY == "2017", "All"]
      FVall <- FVall / 1.11029
      Nall <- length(unique(GabeData$FY)) - 1
      CAGRdef[i] <- ((FVall / IVall)^(1/Nall)-1) * 100
}
GabeData$CAGRdef <- CAGRdef

# Calculate CAGR (deflated) for economic only
eCAGRdef <- vector(mode="numeric")
for(i in 1:6){
      Reg <- as.character(GabeData$Region[i])
      IVecon <- GabeData[GabeData$Region == Reg & GabeData$FY == "2008", "Economic"]
      IVecon <- IVecon / .9981
      FVecon <- GabeData[GabeData$Region == Reg & GabeData$FY == "2017", "Economic"]
      FVecon <- FVecon / 1.11029
      Necon <- length(unique(GabeData$FY)) - 1
      eCAGRdef[i] <- ((FVecon / IVecon)^(1/Necon)-1) * 100
}
GabeData$eCAGRdef <- eCAGRdef

# Calculate CAGR (deflated) for security only
sCAGRdef <- vector(mode="numeric")
for(i in 1:6){
      Reg <- as.character(GabeData$Region[i])
      IVsec <- GabeData[GabeData$Region == Reg & GabeData$FY == "2008", "Security"]
      IVsec <- IVsec / .9981
      FVsec <- GabeData[GabeData$Region == Reg & GabeData$FY == "2017", "Security"]
      FVsec <- FVsec / 1.11029
      Nsec <- length(unique(GabeData$FY)) - 1
      sCAGRdef[i] <- ((FVsec / IVsec)^(1/Nsec)-1) * 100
}
GabeData$sCAGRdef <- sCAGRdef

# Create categorical variables for CAGR display
GabeData$CAGRdefcat <- cut(GabeData$CAGRdef, c(-1000, -2.5, -0.5, 0.5, 2.5, 1000))
GabeData$sCAGRdefcat <- cut(GabeData$sCAGRdef, c(-1000, -2.5, -0.5, 0.5, 2.5, 1000))
GabeData$eCAGRdefcat <- cut(GabeData$eCAGRdef, c(-1000, -2.5, -0.5, 0.5, 2.5, 1000))


# Now just deflate everything to 2009 dollars - I should have done this before
# deflation values from http://www.multpl.com/gdp-deflator/table
years <- as.character(unique(GabeData$FY))
deflator <- c(1.1029, 1.1029, 1.1029, 1.0907, 1.0759, 1.0593, 1.0392,
              1.0195, 1.0017, .9981)
for(i in seq_along(years)){
      GabeData[GabeData$FY == years[i], "All"] <-
            GabeData[GabeData$FY == years[i], "All"] / deflator[i]
      GabeData[GabeData$FY == years[i], "Economic"] <-
            GabeData[GabeData$FY == years[i], "Economic"] / deflator[i]
      GabeData[GabeData$FY == years[i], "Security"] <-
            GabeData[GabeData$FY == years[i], "Security"] / deflator[i]
}


################################################################################
# GRAPHS CREATION AND SAVING
################################################################################

# STACKED BARS FOR ALL
ggplot(GabeData, aes(x = FY, y = All, fill = Region)) +
      geom_bar(stat = "identity") +
      ylab("Aid (Billions)") +
      scale_y_continuous(breaks = c(0, 5000000, 10000000, 15000000, 20000000),
                         labels = c("0","5","10","15","20")) +
      theme(axis.title.x=element_blank()) +
      guides(fill=guide_legend(reverse=TRUE))

ggsave("K:/Development/ForeignAssistance/Aid Graphs/AllRegionStacked.png",
       width = 7, height = 7, unit="in", dpi = 300)



# USE "ALL" VARIABLE FACETED BY REGION
ggplot(GabeData, aes(x = FY, y= All, fill = Region)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      guides(fill = FALSE) + 
      ylab("Aid (Billions)") +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank())

ggsave("K:/Development/ForeignAssistance/Aid Graphs/AllRegionFacet.png",
       width = 10, height = 5, unit="in", dpi = 300)


# WITH DEFLATED CAGR

ggplot(GabeData, aes(x = FY, y= All, fill = CAGRdefcat)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      scale_fill_manual(labels = c("Less than\n-2.5% CAGR",
                                   "CAGR between\n-2.5% and -0.5%",
                                   "CAGR between\n-0.5% and 0.5%",
                                   "CAGR between\n0.5% and 2.5%",
                                   "Greater than\n2.5% CAGR"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "2008-2017\nGrowth Trends") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      ylab("Aid (Billions)") +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Aid Spending by Region\n(constant 2009 dollars)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/AllRegionFacetCAGR.png",
       width = 10, height = 5, unit="in", dpi = 300)



# FACET BY SECURITY/ECON, WTIH CAGR COLORS

datamelt <- melt(GabeData, id.vars = c("Region","FY","eCAGRdefcat", "sCAGRdefcat"), 
                 measure.vars = c("Security","Economic"), value.name = "Expense")
names(datamelt)[5] <- "Type"
datamelt$CAGRdefcat <- datamelt$eCAGRdefcat
for(i in seq_along(datamelt$CAGRdefcat)){
      if(datamelt$Type[i] == "Security"){
            datamelt$CAGRdefcat[i] <- datamelt$sCAGRdefcat[i]}
}

ggplot(datamelt, aes(x = FY, y= Expense, fill = CAGRdefcat)) +
      geom_bar(stat = "identity") +
      facet_grid(Type ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Aid (Billions)") +
      scale_fill_manual(labels = c("Less than\n-2.5% CAGR",
                                   "CAGR between\n-2.5% and -0.5%",
                                   "CAGR between\n-0.5% and 0.5%",
                                   "CAGR between\n0.5% and 2.5%",
                                   "Greater than\n2.5% CAGR"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "2008-2017\nGrowth Trends") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Aid Spending by Region\n(constant 2009 dollars)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/SecEconFacetCAGR.png",
       width = 9, height = 7, unit="in", dpi = 300)


# FACET BY SECURITY/ECON, WITH COLORS BY SHARE
# Calculated as 2017 share - 2008 share
GabeData$eShare = c(-3.08, 0.07, -1.59, 4.75, 0.68, -0.83)
GabeData$sShare = c(2.86, 0.90, 0.69, -0.28, -3.53, -0.64)
datamelt <- melt(GabeData, id.vars = c("Region","FY","eShare", "sShare"), 
                 measure.vars = c("Security","Economic"), value.name = "Expense")
names(datamelt)[5] <- "Type"
datamelt$Share <- datamelt$eShare
for(i in seq_along(datamelt$Share)){
      if(datamelt$Type[i] == "Security"){
            datamelt$Share[i] <- datamelt$sShare[i]}
}
datamelt$Sharecat <- cut(datamelt$Share, breaks = c(-20, -2.5, -0.5, 0.5, 2.5, 20))


ggplot(datamelt, aes(x = FY, y= Expense, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(Type ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Aid (Billions)") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Aid spending:\n(2008-2017 Trend)") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Aid Spending by Region\n(constant 2009 dollars)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/SecEconFacetShare.png",
       width = 9, height = 7, unit="in", dpi = 300)


# Separate econ and security
econmelt <- filter(datamelt, Type == "Economic")
secmelt <- filter(datamelt, Type == "Security")

ggplot(econmelt, aes(x = FY, y= Expense, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Economic Aid (Billions)") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Economic Aid\nSpending, 2008 to 2017") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Economic Aid Spending 2008-2017")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/EconShare.png",
       width = 10, height = 5, unit="in", dpi = 300)

ggplot(secmelt, aes(x = FY, y= Expense, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Security Aid (Billions)") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Security Aid\nSpending, 2008 to 2017") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 2000000, 4000000, 6000000, 8000000),
                         labels = c("0","2","4","6","8")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Security Aid Spending 2008-2017")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/SecurityShare.png",
       width = 10, height = 5, unit="in", dpi = 300)


# SHARE WITH SHARES ON THE Y AXIS

GabeData$eShare = c(-3.08, 0.07, -1.59, 4.75, 0.68, -0.83)
GabeData$sShare = c(2.86, 0.90, 0.69, -0.28, -3.53, -0.64)
datamelt <- melt(GabeData, id.vars = c("Region","FY","eShare", "sShare"), 
                 measure.vars = c("SecShare","EconShare"), value.name = "Sharepct")
names(datamelt)[5] <- "Type"
levels(datamelt$Type) <- c("Security","Economic")
datamelt$Share <- datamelt$eShare
for(i in seq_along(datamelt$Share)){
      if(datamelt$Type[i] == "Security"){
            datamelt$Share[i] <- datamelt$sShare[i]}
}
datamelt$Sharecat <- cut(datamelt$Share, breaks = c(-20, -2.5, -0.5, 0.5, 2.5, 20))

ggplot(datamelt, aes(x = FY, y= Sharepct, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(Type ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Share of spending") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Aid spending:\n(2008-2017 Trend)") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                         labels = c("0","10%","20%","30%","40%","50%",
                                    "60%","70%")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Aid Spending by Region\n(as share of category)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/SecEconFacetShare_Yshare.png",
       width = 9, height = 7, unit="in", dpi = 300)

# Separate econ and security
econmelt <- filter(datamelt, Type == "Economic")
secmelt <- filter(datamelt, Type == "Security")

ggplot(econmelt, aes(x = FY, y= Sharepct, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Share of spending") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Economic Aid\nSpending, 2008 to 2017") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                         labels = c("0","10%","20%","30%","40%","50%",
                                    "60%","70%")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Economic Aid by Region\n(as share of category)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/EconShare_Yshare.png",
       width = 10, height = 5, unit="in", dpi = 300)

ggplot(secmelt, aes(x = FY, y= Sharepct, fill = Sharecat)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Region) +
      # scale_alpha_discrete(range = c(0.5,1), guide = FALSE) +
      ylab("Security Aid (Billions)") +
      scale_fill_manual(labels = c("Declined by\n2.5 percentage\npoints or more",
                                   "Declined by\n0.5 to 2.5\npercentage points",
                                   "Between \n-0.5 and 0.5\npercentage points",
                                   "Increased by\n0.5 to 2.5\npercentage points",
                                   "Increased by\n2.5 percentage\npoints or more"),
                        values = c("red","pink","yellow","lightgreen",
                                   "darkgreen"), drop = FALSE) +
      labs(fill = "Share of Security Aid\nSpending, 2008 to 2017") +
      theme(legend.position = "bottom", legend.title=element_text(face="bold")) +
      scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                         labels = c("0","10%","20%","30%","40%","50%",
                                    "60%","70%")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.7)) +
      theme(axis.title.x=element_blank()) +
      ggtitle("Security Aid by Region\n(as share of category)")

ggsave("K:/Development/ForeignAssistance/Aid Graphs/SecurityShare_Yshare.png",
       width = 10, height = 5, unit="in", dpi = 300)
















# PROPORTIONAL STACKED BAR
library(plyr)

newdata <- ddply(GabeData, "FY", transform, 
                 percent = Economic / sum(Economic) * 100)

ggplot(newdata, aes(x = FY, y = percent, fill = Region)) +
      geom_bar(stat = "identity")

# LINE GRAPH
GabeData <- read.csv("GabeData.csv")
library(ggplot2)
ggplot(GabeData, aes(x = FY, y = Economic, colour = Region)) + 
      geom_line(size = 2)