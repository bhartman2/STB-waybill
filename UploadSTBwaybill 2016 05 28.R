# upload the flat file to a database
# 2016 05 25
# 2016 05 27 upload any year
# 2016 05 28 corrected the error in the size of the no_TOFC_COFC_units from 3 to 4
#  now need to refill all databases. also appended the year on the database, will start tomorrow

#use list with keys
system_key <- c("TP-W700","BRUCE-THINK")
system_wd <- c(rep("C:/Users/Bruce Hartman/OneDrive/Documents/My Latex/METRANS Proposal/Chicago Restructuring/FAF data/STB-Rail Car loadings",2))
setwd(setNames(as.list(system_wd), system_key)[[Sys.info()["nodename"]]]); getwd()

######################################
# find out if packages are installed and library them
package_list <- c("RODBC","ggmap","stringr","car","gvlma","qcc","DMwR","cluster","mclust")
for (package in package_list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package); library(package, character.only=T) }
}
#######################################
# general description and procedure
# set the year you wish to work with
# retrieve the column names and sizes and types from waybillinput table
# create info needed about columns, widths, types
# get the correct fixed with formatted file into a data frame
#      this step takes an hour or so for the full 600000+- records
# next assign the autoincrement id numer for the records, also put the timestamp as null
# define the new table name and temp table name
# use sqlSave to upload the local data into the temp table in db
#      this step takes overnight
# check it out to make sure it got there
# insert data from temp table into real one, this one corrects the column sizes to the real ones
# drop the temporary table when you are sure 

#####################################################################################
# set year
(year<-"2013")
(yy<-substr(year,3,4))
# here we do some reconnoitering to upload the `year` database
# read in fixed width format using data from the PUByyD file
# all fields to be loaded character
(filename<-paste("PublicUseWaybillSample",year,"/PUB",yy,"A.txt",sep="") )

#####################################
#retrieve the column names and sizes and types from waybillinput table
# the data is all character; the column widths and types are defined here
ODBC_NAME <- "Just Host" ; #ODBC #no new database,just use the waybillinput table which is a dummy for the fields.
stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE)
wdbinfo <- sqlColumns(stecher,"waybillinput",errors=TRUE)
(columnnames <- wdbinfo$COLUMN_NAME[1:65])
(columnwidths<-list( wdbinfo$COLUMN_SIZE[1:65] ) )
(realcolumnwidths<-
                c(6,4,4,1,4,4,2,3,4,1,       1,1,5,7,7,9,9,9,1,1,
                1,1,1,1,1,4,1,1,5,3,       1,3,1,2,2,2,2,2,2,2,
                2,2,3,1,1,5,3,4,5,4,       4,4,1,1,2,1,4,46,1,6,
                9,11,6,10,19))
( columntypes<-list( wdbinfo$TYPE_NAME[1:65] ) )

# # this is the correct format for varTypes in the sqlSave command, but does not set the column widths
# # so we cannot really use this
# vtype<-paste(wdbinfo$TYPE_NAME,"(",wdbinfo$COLUMN_SIZE,")",sep="")
# varTypes <- as.character(vtype) 
# names(varTypes) <- as.character(wdbinfo$COLUMN_NAME) 
# varTypes
# here re the default types if you allow sqlSave to create the database
getSqlTypeInfo("MySQL")

#############
# now read in the data from the fixed width format file
w.fixed.data <- read.fwf(filename,header=FALSE,sep="\t",
                       colClasses="character",
                       col.names=columnnames,
                       width=columnwidths ) # , n=10
# how many rows are there? should check with the documentation 
nrow(w.fixed.data)
head(w.fixed.data[,c(-58)])#this omits the blank field 57-A but you can check data here with file.
# but actually to load data we include the blank field
# we assume the column names have not changed. check the PUByyD first

##################
# next assign the autoincrement id number for the records
# the update_timestamp shoudl go in as <na> or as zero
# NA goes to database as NULL which is fine, 0 goes in as 000000, but that cannot be read back in R
range<-as.integer(1): as.integer(nrow(w.fixed.data))
w.fixed.data$idwaybillinput <- range
# next assign the timestamp last_update field to be 0
#if we put it in as <NA> in file it gets converted to NULL in db table, and then query works.
# this is easy and gets it to load
# w.fixed.data$update_timestamp <- "null"
head(w.fixed.data)

#########################################################################
# creates the new year table with the correct type and width from the template table
# no need to create the temptable, the sqlSave does it
(newtablename<-paste("waybillinput",year,sep=""))
(newtemptable<-paste("waybillinput",year,"tmp",sep=""))
(newtablecreatequery<-paste("CREATE TABLE `",newtablename,
                    "` LIKE `waybillinput`", sep=""))
#just in case we need it
stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE)
#create the new destination table
sqlQuery(stecher, newtablecreatequery)
# go to phpmyadmin and make idwaybillinput an index
###############################################################################


#saves all the data from the data frame w.fixed.data into the table. 
#It will create the temp table if it does not exist but the types will be 63*varchar(255), int(11) double
#just in case we need it
stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE)
#creates the temp table and loads the data, takes a long time
sqlSave(stecher, w.fixed.data, tablename=newtemptable, rownames=FALSE, colnames=FALSE, safer=TRUE,
        test=FALSE)
#saves time if you make a mistake above, drop table before trying again
(dropquery <- paste("DROP TABLE ",newtemptable,sep=""))
sqlQuery(stecher, dropquery )
#################################################

# now use INSERT INTO newtablename SELECT * FROM newtemptable WHERE 1 to move records to real table
(movequery <- paste("INSERT INTO ", newtablename, " SELECT * FROM ", newtemptable, sep=""))
stecher<-odbcConnect(ODBC_NAME, uid="drbruce2_stecher", pwd="saints", believeNRows=FALSE) #probably needed
sqlQuery(stecher, movequery )
#this one works for sure

# make sure they are all there in proper format
#this query works in myphpadmin
# you can change the sql to check specific problematic parts of the data
(lastbut20 <- nrow(w.fixed.data)) # target number is 
(checkquery <- paste("SELECT `waybill_date`,`idwaybillinput`, `no_TOFC_COFC_units` FROM `",newtablename,"` WHERE `idwaybillinput` >= ",lastbut20,sep=""))
(check <- sqlQuery(stecher, checkquery))
# make sure we have the right no of records
(countquery <- paste("SELECT count(*) FROM `",newtablename,"` WHERE 1",sep=""))
(count <- sqlQuery(stecher, countquery))

# if the check works, do this to drop the temp table
(dropquery <- paste("DROP TABLE ",newtemptable,sep=""))
sqlQuery(stecher, dropquery )
################

#check an anomaly in the results here. write your own queries of R statements depending on where the problem is
sample(w.fixed.data$no_TOFC_COFC_units,10,replace=FALSE,prob=NULL)
w.fixed.data$no_TOFC_COFC_units[600000:640100]
#yes, they are different

# dont do this till you are sure you are done with the data you read in, 
# it manages memory so the space isn't huge
rm(w.fixed.data)
gc()
