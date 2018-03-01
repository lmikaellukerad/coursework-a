data <- read.csv(file="webvisit2.csv", header=TRUE, sep=",")
#data <- subset(webvisits, select = -user)

hist(webvisits$pages)

library(reshape2)
library(gplots)

melt_v <- melt(data, id.vars = c("user", "version"), measure.vars = "pages")
analysis_v <- dcast(melt_v, user + version ~ variable, value.var='value')

analysis_v_0 <- analysis_v[analysis_v$version==0, ]
analysis_v_1 <- analysis_v[analysis_v$version==1, ]

melt_p <- melt(data, id.vars = c("user", "portal"), measure.vars = "pages")
analysis_p <- dcast(melt_p, user + portal ~ variable, value.var='value')

analysis_p_0 <- analysis_p[analysis_p$portal==0, ]
analysis_p_1 <- analysis_p[analysis_p$portal==1, ]

analysis_vp <- data
analysis_vp_00 <- analysis_vp[(analysis_vp$version==0 & analysis_vp$portal==0), ]
analysis_vp_01 <- analysis_vp[(analysis_vp$version==0 & analysis_vp$portal==1), ]
analysis_vp_10 <- analysis_vp[(analysis_vp$version==1 & analysis_vp$portal==0), ]
analysis_vp_11 <- analysis_vp[(analysis_vp$version==1 & analysis_vp$portal==1), ]

hist(analysis_v_0$pages)
hist(analysis_v_1$pages)
hist(analysis_p_0$pages)
hist(analysis_p_1$pages)
hist(analysis_vp_00$pages)
hist(analysis_vp_01$pages)
hist(analysis_vp_10$pages)
hist(analysis_vp_11$pages)

