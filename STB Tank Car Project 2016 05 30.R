# Some statistics on ethanol and other tank car transport
# 2016 05 28
#  modified 05 21 to make tablename a variable so can do different years simply by changing it.
#  modified 05 27 to try 2012, 2013, 2014.
# 2013 does not work, while the other two do.
# problem is in q_sums; query will not run under phpmyadmin either

#use list with keys
system_key <- c("TP-W700","BRUCE-THINK")
system_wd <- c(rep("C:/Users/Bruce Hartman/OneDrive/Documents/My Latex/METRANS Proposal/Chicago Restructuring/FAF data/STB-Rail Car loadings",2))
#system_list <- setNames(as.list(system_wd), system_key)
#setwd(system_list[[Sys.info()["nodename"]]]); getwd()

setwd(setNames(as.list(system_wd), system_key)[[Sys.info()["nodename"]]]); getwd()

######################################
# find out if packages are installed
package_list <- c("RODBC","ggmap","stringr","car","gvlma","qcc","DMwR","cluster","mclust")
for (package in package_list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package); library(package, character.only=T) }
}
##################################

# set up connection to STB waybill database
ODBC_NAME <- "Just Host" ;
stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE)
odbcGetInfo(stecher)
####################################

# set up year of data table for proper year
year <- "2014"
(tablename <- paste("waybillinput",year,sep=""))
#a variety of queries in sql
# u <- "USE drbruce2_STBWaybill" # dont need this is database is set in the DSN
# sqlQuery(stecher,u)
(query <- paste("SELECT * FROM ",tablename," limit 10",sep=""))
sqlQuery(stecher,query)
#if we put update_timestamp in as <NA> in file it gets converted to NULL in db table, and then query works.

describe_commodity_groups <- "DESCRIBE STCC_commodity_groups" ; 
sqlQuery(stecher,describe_commodity_groups)
(geodist_cols <- sqlQuery(stecher,"SHOW COLUMNS FROM geodist")) 
sqlColumns(stecher,"area_codes",errors=TRUE)
(tables<-sqlQuery(stecher,"SHOW TABLES"))
q_carty <- "SELECT STB_Sch710_LineNo, STB_car_type_desc, STB_car_type_code FROM `STB_Form710_car_types`"
(cartype <- sqlQuery(stecher, q_carty))
q_commod <- "SELECT STCC_group, left(STCC_desc,100) 'STCC_desc' FROM `STCC_commodity_groups`"
(commodities <- sqlQuery(stecher,q_commod))

sqlQuery(stecher, paste("select count(*) from ",tablename," group by `account_period`",sep=""))


# # specific commodities in 28 13 20 29
# #dont do this every time #########################################################
# only ned it if you are investigating other commodity classes
# spec_commod <- read.table("specific commodity codes 13 20 29.txt", sep="\t") #28
# names(spec_commod)<-c("spec_commod_code", "spec_commod_desc")
# head(spec_commod)
#dont do this every time #########################################################
# # can i load this into mysql db
# load <- "load data local infile 'specific commodity codes 13 20 29.txt' into table spcom"
# stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE)
# sqlQuery(stecher,load)
#worked##################################################################
spe_com<-sqlQuery(stecher, "SELECT comcode, left(comdesc,70) 'comdescr' FROM spcom")
#names(spe_com)
spe_com
#####################################################################


# some calculations
(comcode_list <- paste(13,20,28,29,sep=",") ) #13,20,28,29

q_sums <- paste("SELECT w.`commodity_code` 'commodity',
left(c.`comdesc`,22) 'description',
concat('$', format(sum(w.`freight_rev`),0)) 'freight', 
concat('$', format(sum(w.`transit_charges`),0)) 'transit', 
concat('$', format(sum(w.`misc_charges`),0)) 'misc',
concat('$', format(sum(w.`expand_frt_rev`),0)) 'xrev',
sum(w.`no_carloads`) 'tot_cars', sum(w.`expand_carloads`) 'tot_xcars',
format((sum(w.`expand_carloads`)/sum(w.`no_carloads`)),3) 'ratio',
concat('$',format((sum(w.`freight_rev`)/sum(w.`expand_carloads`)),2)) 'costpcar'
FROM ",tablename," w, spcom c WHERE w.`ICC_car_type` between 50 and 51 and 
left(w.`commodity_code`,2) in (", comcode_list,  
") and w.`commodity_code` = c.`comcode`
 GROUP by w.`commodity_code` ORDER BY sum(w.`expand_frt_rev`) DESC",
sep="")
gsub("\n","",q_sums)
sqlQuery(stecher,"SET SQL_BIG_SELECTS=1")
tank_calcs <- sqlQuery(stecher,q_sums)
tank_calcs

# statistics with tank_calcs

plot(tank_calcs$tot_xcars,tank_calcs$costpcar)
plot(tank_calcs$ratio,tank_calcs$costpcar)
head(tank_calcs)
summary(tank_calcs)

tank_calcs[tank_calcs$commodity %in% c("28184","13111"),]
tank_calcs[order(-as.numeric(substr(tank_calcs$costpcar,2,length(tank_calcs$costpcar)-1))),]

# from here on we work with market shares of revenue and carloads
(gt_xrev<-sum(as.numeric(gsub("\\$","",gsub(",","",tank_calcs$xrev)))))
(gt_xcars<-sum(tank_calcs$tot_xcars))
100*tank_calcs$tot_xcars/gt_xcars
100*as.numeric(gsub("\\$","",gsub(",","",tank_calcs$xrev)))/gt_xrev
report1<-data.frame(tank_calcs$commodity, tank_calcs$description, 
                    round(100*tank_calcs$tot_xcars/gt_xcars,3),
                    round(100*as.numeric(gsub("\\$","",gsub(",","",tank_calcs$xrev)))/gt_xrev,3),
                    stringsAsFactors=FALSE)
names(report1)=c("commodity","description","mktshrcarloads","mktshrxrev")
report1

(sortcar<-report1[order(-report1$mktshrcarloads),])
# report1
(sortxrev<-report1[order(-report1$mktshrxrev),])
# plot(report1$commodity, report1$mktshrcarloads)

#pareto chart almost right!
#library(qcc)
cumpcn <- function ( x, col,cp ) { 
  for ( i in 1:nrow(x)) {
    if ( sum(x[1:i,col]) > cp + .02 ) { return(i)}
  }
}

n<-cumpcn(sortxrev,"mktshrxrev",80)
msxrev<-as.vector(sortxrev[,"mktshrxrev"])
names(msxrev) <- substr( 
  paste(as.character(sortxrev[,"commodity"]),sortxrev[,"description"],sep=" "),
  1, 20)

pareto.chart(msxrev[1:n], ylab = "Market Shares % of XRevenue", 
             ylab2 = paste("Cum % of first ", n, " commodities",sep=""),
             cumperc = seq(0,100, by = 20),
             main=paste("Pareto Chart: Market Share of Revenue","\n\r",
                       "First ",n," commodities contribute 80%",sep=""),
             plot=TRUE)

nc<-cumpcn(sortxrev,"mktshrcarloads",80)
msncar<-as.vector(sortxrev[,"mktshrcarloads"])
names(msncar) <- substr( 
  paste(as.character(sortxrev[,"commodity"]),sortxrev[,"description"],sep=" "),
  1, 20)
msncar[1:nc]
pareto.chart(msncar[1:nc], ylab = "Market Shares of Carloads", 
             main=paste("Pareto Chart: Market Share of Carloads","\n\r",
                        "First ",nc," commodities  contribute 80%",sep=""),
             plot=TRUE)

##################################################################################################


# tank_calcs
report2<-data.frame(tank_calcs$commodity, tank_calcs$description, 
                    round(tank_calcs$ratio,3),
                    round(as.numeric(gsub("\\$","",gsub(",","",tank_calcs$costpcar))),3),
                    stringsAsFactors=FALSE)
names(report2)=c("commodity","description","ratio","costpcar")
report2


(sortratio<-report2[order(report2$ratio),])
(sortcostpcar<-report2[order(-report2$costpcar),])
Rtop=30; Ctop=200
sr <- sortratio[ which(sortratio$ratio<Rtop),]
sc <- sortcostpcar[ which(sortcostpcar$costpcar>Ctop),]

#plot ratios
op<-par(mar=c(10,4.1,1,1))
barplot(sr$ratio, names.arg=substr(sr$description,1,10), ylim = c(0,Rtop),
        main="Ratios in ascending order by commodity",
        ylab="Ratio", col="red", las=2
)
abline(h=seq(0,30,by=5), lty = 6, col = "gray")
#grid (NULL,NULL, lty = 4, col = "cornsilk2") 
par(op)

#plot cost per car
op<-par(mar=c(10,4.1,1,1))
barplot(sc$costpcar, names.arg=substr(sc$description,1,10), ylim = c(0,3000),
        main="Cost per Car in descending order by commodity",
        ylab="Cost per car", col="green", las=2
)
abline(h=seq(0,1000,by=100), lty = 6, col = "gray")
par(op)

#####################################################################


# plot ratio vs cost per car to see if there is a relation
plot(sr[,"ratio"], 
     log10(sr[,"costpcar"]),
     ylim=c(0,4), xlim=c(0,Rtop),
     xlab="Ratio", ylab="Cost per car per trip",
     main="Ratio vs Cost per car per trip", 
     )

plot(sc[,"ratio"], 
     log10(sc[,"costpcar"]),
     ylim=c(0,4), 
     xlab="ratio", ylab="Cost per car per trip",
     main="Ratio vs Cost per car per trip", 
)

# try regression

cor(sr$ratio, sr$costpcar)
cor(sc$ratio, sc$costpcar)
cor(report2$ratio, report2$costpcar)

sr.lm<-lm(log10(sr$costpcar)~sr$ratio)
summary(sr.lm)
sr[c(1,5,14),]
op<-par(mfrow=c(2,2))
plot(sr.lm, main="Regression for sr")
par(op)
outlierTest(sr.lm) # Bonferonni p-value for most extreme obs
# qqPlot(sr.lm, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(sr.lm) # leverage plots
# Influence Plot 
# influencePlot(sr.lm,  id.method="identify", main="Influence Plot sr.lm", sub="Circle size is proportial to Cook's Distance" )
summary(gvmodel <- gvlma(sr.lm)) 


sc.lm<-lm(log10(sc$costpcar)~sc$ratio)
summary(sc.lm)
op<-par(mfrow=c(2,2))
plot(sc.lm, main="Regression for sc")
par(op)
outlierTest(sc.lm) # Bonferonni p-value for most extreme obs
sc[c(1,4,21),] #display outliers if necessary
# qqPlot(sc.lm, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(sc.lm) # leverage plots
crPlots(sc.lm) #tests nonlinearity
# Influence Plot 
# influencePlot(sc.lm,  id.method="identify", main="Influence Plot sc.lm", sub="Circle size is proportial to Cook's Distance" )
gvmodel <- gvlma(sc.lm) 
summary(gvmodel)

report2.lm<-lm(log10(report2$costpcar)~report2$ratio)
summary(report2.lm)
op<-par(mfrow=c(2,2))
plot(report2.lm, main="Regression for report2")
par(op)
report2[c(2,68,92),]
outlierTest(report2.lm) # Bonferonni p-value for most extreme obs
# qqPlot(report2.lm, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(report2.lm) # leverage plots
crPlots(report2.lm) #tests nonlinearity
# Influence Plot 
# influencePlot(report2.lm,  id.method="identify", main="Influence Plot report2.lm", sub="Circle size is proportial to Cook's Distance" )
gvmodel <- gvlma(report2.lm)
gvmodelDel<-deletion.gvlma(gvmodel)
summary(gvmodel)
plot(gvmodel)
summary(gvmodelDel)
plot(gvmodelDel, which = 1:2, TukeyStyle = TRUE, 
     ask = TRUE, pointlabels)
################################################################################

# outlier detection
# library(DMwR)
# lofactor function re-implements the code previously made available 
# in the dprep package (Acuna et.al., 2009) that was removed from CRAN. 
# This code in turn is an implementation of the LOF method
# by Breunig et. al. (2000). See this reference to understand the full details on how these local outlier factors are calculated for each case in a data set.

#function to identify outliers
find_outliers <- function (dataf, collist, noutliers=10) {
  cat(sprintf("Outliers by LOF method of %s\n",as.character(substitute(dataf))))
  x<-dataf
  outlier.scores <- lofactor(x[,collist], k=noutliers)
  plot(density(outlier.scores))
  # pick top 5 as outliers
  outliers <- order(outlier.scores, decreasing=T)[1:noutliers]
  # who are outliers
  # print(outliers)
  sx<-data.frame(x,outlier.scores)
  return(sx[outliers,]) 
} # end function find_outliers
find_outliers(sr,c(3:4),10)
find_outliers(sc,c(3:4),10)
find_outliers(report2,c(3:4),10)
##########################################################################
# This function uses hierarchical clustering to obtain a ranking of outlierness for a set of cases. The ranking is obtained on the basis of the path each case follows within the merging steps of a agglomerative hierarchical clustering method. See the references for further technical details on how these rankings are obtained.
# function find_hcl_outliers
find_hcl_outliers <- function (dataf, collist) {
  cat(sprintf("Outliers by hierarchical clustering of %s\n",as.character(substitute(dataf))))
  j<-outliers.ranking(dataf[,collist])
  hc_outliers <- data.frame(dataf[,1:2],j$rank.outliers, j$prob.outliers)
  hc_outliers_ordered <- hc_outliers[order(hc_outliers$j.rank.outliers),]
  return(hc_outliers_ordered)
}
#end function find_hcl_outliers
find_hcl_outliers(sr,c(3:4))
find_hcl_outliers(sc,c(3:4))
find_hcl_outliers(report2,c(3:4))


#################################################################

#use clustering to find groups in 
head(report1)
head(report2)
head(sr)
head(sc)

op<-par(ask=FALSE)
for (i in 2:5) { #cannot cluster more than 5
  cat(sprintf("Number of clusters: %d\n",i))
  plot(pam(sr[,3:4], i), cex=1.2)  
}
par(op)

pam(sr[3:4],3)
plot(pam(sr[,3:4],3), cex=1.2)

op<-par(ask=FALSE)
for (i in 2:5) { #cannot cluster more than 5
  cat(sprintf("Number of clusters: %d\n",i))
  plot(pam(sc[,3:4], i), cex=1.2)  
}
par(op)

#model based clustering
# Model based approaches assume a variety of data models and apply maximum likelihood estimation and Bayes criteria to identify the most likely model and number of clusters. Specifically, the Mclust( ) function in the mclust package selects the optimal model according to BIC for EM initialized by hierarchical clustering for parameterized Gaussian mixture models. (phew!). One chooses the model and number of clusters with the largest BIC. See help(mclustModelNames) to details on the model chosen as best.
# library(mclust)
help(mclustModelNames)
fit <- Mclust(sr[,3:4])
# plot results
plot(fit, what="classification") 
summary(fit) # display the best model
fit$data
fit$parameters$mean

#a little riff on the clusters and the potential of a linear regression (or in this case on the log.)
f<-glm(log10(fit$parameters$mean["costpcar",])~fit$parameters$mean["ratio",],
      family=gaussian)
summary(f)
f$coefficients
##################################

# plot the glm fit
op<-par(mfrow=c(2,2))
plot(f)
par(op)

plot(fit$parameters$mean["ratio",],log10(fit$parameters$mean["costpcar",]),
     xlab="Ratio", ylab = "log(Cost per Car)",
     main="Cost per car is an exponential function of Ratio" )
abline(coef=f$coefficients, col="red")

#end of the regression riff
####################################################
round(fit$z,3)
fit$classification
barplot(round(fit$parameters$pro,2))
#list the commodities in each classification
for (j in 1:6) {
  cat(sprintf("Classification: %d\n",j));print(sr[names(fit$classification)[fit$classification==j],1:2])}
fit$parameters$pro
###########################################################################

# n <- nrow(report2)
# labels <- 1:n
# labels[-outliers] <- "."
#does not work, no principal components
# biplot(prcomp(report2), cex=.8, xlabs=labels)

#this nice plot shows outliers in red
# pch <- rep(".", n)
# pch[outliers] <- "+"
# col <- rep("black", n)
# col[outliers] <- "red"
# pairs(report2[,c(1,3,4)], pch=pch, col=col)

################################################################################
