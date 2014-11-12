###########################################
#read-in & setup data frame & clean up data
###########################################
can <- read.csv("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/Downloads/curr.csv", header=TRUE, skip=6)
plan.data <- read.csv("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/Downloads/plans.csv", header=TRUE)
plan.data <- as.data.table(plan.data)
plan.grower <- read.csv("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/Downloads/plansbyga.csv", header=TRUE)
plan.grower <- as.data.table(plan.grower)

yr <- as.numeric(format(Sys.Date(),"%y"))

#Get only pertinent columns
loc <- which(colnames(can)=="Loc." )
grw <- which(colnames(can)=="Grower")
cat <- which(colnames(can)=="Category")
bsz <- which(colnames(can)=="Base.Size")
osd <- which(colnames(can)=="Orig..Str.Dt")
ppl <- which(colnames(can)=="Prod..Plan")
ppq <- which(colnames(can)=="Prod..Plan.EQU")
grp <- which(colnames(can)=="Gross..Prod.Qty")
gre <- which(colnames(can)=="EQU..Prod.Qty")
sto <- which(colnames(can)=="Std.Oper")
ldt <- which(colnames(can)=="Labor.Date")
wos <- which(colnames(can)=="WO.Stat")
opc <- which(colnames(can)=="Oper..Complete")
qty <- which(colnames(can)=="Orig..Ord.Qty")
equ <- which(colnames(can)=="Orig..Ord.EQU")
wip <- which(colnames(can)=="WIP.Ord.")

#put column numbers in vector
clmns <- c(loc,grw,cat,bsz,osd,ppl,sto,ldt,wos,opc,qty,equ,wip,grp,gre,ppq)
#create new matrix with just pertinent columns 
can.1 <- can[,clmns]
#delete unneeded rows
can.2 <- can.1[!(can.1$Base.Size == ""),]
can.2 <- can.2[!(can.2$Grower == 500051),]
can.2 <- can.2[!(can.2$Grower == 500008),]
#change VH to CA
can.2$Loc.[can.2$Loc. == 180000]<-170000
can.2$WO.Stat[is.na(can.2$WO.Stat)]<-0
#create columns for month and year
###find length of date and use if to get dates
dt.1 <- as.numeric(substring(as.character(can.2$Orig..Str.Dt[1]),7,8))
#to handle whether date is held like "xx/xx/xx" or like "xxxxxxxxx"
if(dt.1 + 1 == yr|dt.1 == yr){
  #function to separate month from string
  unlst.mo <- function(x){
    newcol <- unlist(strsplit(x, "[/]"))[1]
  }
  unlst.yr <- function(x){
    newcol <- substring(unlist(strsplit(x, "[/]"))[3],3,4)
  }
  can.2$st.dt <- as.character(can.2$Orig..Str.Dt)
  can.2$mo <- lapply(can.2$st.dt,unlst.mo)
  can.2$yr <- lapply(can.2$st.dt,unlst.yr)
  can.2$lb.mo <- lapply(as.character(can.2$Labor.Date),unlst.mo)
  can.2$lb.yr <- lapply(as.character(can.2$Labor.Date),unlst.yr)
}else{
  library(date)
  can.2$st.dt <- as.Date(can.2$Orig..Str.Dt,origin="1899-12-30") 
  #start month
  can.2$mo <- format(can.2$st.dt,"%m")
  #start year
  can.2$yr <- format(can.2$st.dt,"%y")
}
#remove comma characters so that columns are no longer factors and can have functions applied
can.2$Prod..Plan <- as.numeric(gsub(",","", can.2$Prod..Plan))
can.2$Prod..Plan.EQU <- as.numeric(gsub(",","", can.2$Prod..Plan.EQU))
can.2$Gross..Prod.Qty <- as.numeric(gsub(",","", can.2$Gross..Prod.Qty))
can.2$EQU..Prod.Qty <- as.numeric(gsub(",","", can.2$EQU..Prod.Qty))
can.2$Orig..Ord.Qty <- as.numeric(gsub(",","", can.2$Orig..Ord.Qty))
can.2$Orig..Ord.EQU <- as.numeric(gsub(",","", can.2$Orig..Ord.EQU))

##################################
#setup df with only necessary data
#######################################################
#to run this for production plan put "plan" for sum, for completed put "qty" for sum
library(data.table)

a <- data.table(loc=can.2$Loc., 
                cat=can.2$Category, 
                qty=can.2$Orig..Ord.Qty, 
                month=can.2$mo, 
                year=can.2$yr,
                grw=can.2$Grower,
                plan=can.2$Prod..Plan,
                grpr=can.2$Gross..Prod.Qty,
                gre=can.2$EQU..Prod.Qty,
                equ.pr=can.2$Prod..Plan.EQU,
                size=can.2$Base.Size,
                equ=can.2$Orig..Ord.EQU,
                wo = can.2$WO.Stat,
                lb.mo = can.2$lb.mo,
                lb.yr = can.2$lb.yr,
                st.op = can.2$Std.Oper)

a <- a[grw!=0,]

##################################
#variables
#########################################
###################################
grow.loc <- c(160000,170000,550000) 
grow.loc.rng <- 1:length(grow.loc)
GA.or.ca <- c(500001,500003,500004)
GA.ga <- c(500001,500002)
or.ca.rng <- 1:length(GA.or.ca)
ga.rng <- 1:length(GA.ga)
#prior month
mo.1 <- as.numeric(format(Sys.Date(),"%m"))-1 
mo = mo.1
#december handler
if(mo.1 == 0) mo <- 12

#function to create dataframe for a given location and growing area for complete YTD or just month
createframe <- function(location, GA, n=1, year=FALSE){
  #n is 1 for "size" or 2 for "cat"
  kbys = c("size","cat")
  if(year==TRUE){
    temp.a <- a[loc==location & year==yr & month<=mo & grw==GA & wo!=15 & wo!=99 & wo != 0,
                list(orig.qty=sum(qty),prod=sum(plan)),keyby=eval(kbys[n])]
    
    temp.b <- a[loc==location & lb.yr == yr & lb.mo <= mo & grw==GA & wo != 15 & wo != 99 & wo != 0, 
                list(grss=sum(grpr)), keyby = eval(kbys[n])]
    
    temp.a <- merge(temp.a,temp.b,by=eval(kbys[n]),all=TRUE)
    temp.a[is.na(temp.a)]<-0
    temp.a$perc.comp <- temp.a$grss/temp.a$prod
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$orig.qty)
    c3 <- sum(temp.a$prod)
    c4 <- sum(temp.a$grss)
    c5 <- c4/c3
    if(n==1){
      newrow <- data.frame(size=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    }else newrow <- data.frame(cat=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    
    temp.a <- rbind(temp.a,newrow)
  }else{
    temp.a <- a[loc==location & year==yr & month==mo & grw==GA & wo!=15 & wo!=99 & wo != 0,
                list(orig.qty=sum(qty),prod=sum(plan),grss=sum(grpr)),keyby=eval(kbys[n])]
    temp.a$perc.comp <- temp.a$grss/temp.a$prod
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$orig.qty)
    c3 <- sum(temp.a$prod)
    c4 <- sum(temp.a$grss)
    c5 <- c4/c3
    if(n==1){
      newrow <- data.frame(size=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    }else newrow <- data.frame(cat=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    
    temp.a <- rbind(temp.a,newrow)
  }
  temp.a
}

yr.ca.sz.500001 <- createframe(170000,500001,year=TRUE)
yr.ca.sz.500003 <- createframe(170000,500003,year=TRUE)
yr.ca.sz.500004 <- createframe(170000,500004,year=TRUE)
yr.ca.cat.500001 <- createframe(170000,500001,n=2,year=TRUE)
yr.ca.cat.500003 <- createframe(170000,500003,n=2,year=TRUE)
yr.ca.cat.500004 <- createframe(170000,500004,n=2,year=TRUE)
yr.or.sz.500001 <- createframe(160000,500001,year=TRUE)
yr.or.sz.500003 <- createframe(160000,500003,year=TRUE)
yr.or.sz.500004 <- createframe(160000,500004,year=TRUE)
yr.or.cat.500001 <- createframe(160000,500001,n=2,year=TRUE)
yr.or.cat.500003 <- createframe(160000,500003,n=2,year=TRUE)
yr.or.cat.500004 <- createframe(160000,500004,n=2,year=TRUE)
yr.ga.sz.500001 <- createframe(550000,500001,year=TRUE)
yr.ga.sz.500002 <- createframe(550000,500002,year=TRUE)
yr.ga.cat.500001 <- createframe(550000,500001,n=2,year=TRUE)
yr.ga.cat.500002 <- createframe(550000,500002,n=2,year=TRUE)

mo.ca.sz.500001 <- createframe(170000,500001)
mo.ca.sz.500003 <- createframe(170000,500003)
mo.ca.sz.500004 <- createframe(170000,500004)
mo.ca.cat.500001 <- createframe(170000,500001,n=2)
mo.ca.cat.500003 <- createframe(170000,500003,n=2)
mo.ca.cat.500004 <- createframe(170000,500004,n=2)
mo.or.sz.500001 <- createframe(160000,500001)
mo.or.sz.500003 <- createframe(160000,500003)
mo.or.sz.500004 <- createframe(160000,500004)
mo.or.cat.500001 <- createframe(160000,500001,n=2)
mo.or.cat.500003 <- createframe(160000,500003,n=2)
mo.or.cat.500004 <- createframe(160000,500004,n=2)
mo.ga.sz.500001 <- createframe(550000,500001)
mo.ga.sz.500002 <- createframe(550000,500002)
mo.ga.cat.500001 <- createframe(550000,500001,n=2)
mo.ga.cat.500002 <- createframe(550000,500002,n=2)
########################################
#by future
########################################
#########################################
createfuture <- function(location, GA, n=1){
  kbys = c("size","cat")
  mo.1 <- as.numeric(format(Sys.Date(),"%m")) 
  mo <- mo.1
  yr <- as.numeric(format(Sys.Date(),"%y"))
  
  temp.a <- a[loc==location & year==yr & month==mo & grw==GA,
              list(prod=sum(plan)),keyby=eval(kbys[n])]
  
  mo <- mo.1 + 1
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.b <- a[loc==location & year==yr & month==mo & grw==GA,
              list(prod=sum(plan)),keyby=eval(kbys[n])]
  
  mo <- mo.1 + 2
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.c <- a[loc==location & year==yr & month==mo & grw==GA,
              list(prod=sum(plan)),keyby=eval(kbys[n])]
  
  temp.a <- merge(temp.a,temp.b,keyby=eval(kbys[n]),all=TRUE)
  temp.a <- merge(temp.a, temp.c,keyby=eval(kbys[n]),all=TRUE)
  temp.a[is.na(temp.a)]<-0
  temp.a$Total <- temp.a$prod.x + temp.a$prod.y + temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$prod.x)
  c3 <- sum(temp.a$prod.y)
  c4 <- sum(temp.a$prod)
  c5 <- sum(temp.a$Total)
  if(n==1){
    newrow <- data.frame(size = c1, prod.x = c2, prod.y = c3, prod = c4, Total = c5)
  }else newrow <- data.frame(cat = c1, prod.x = c2, prod.y = c3, prod = c4, Total = c5)
  
  temp.a <- rbind(temp.a, newrow)  
  return(temp.a)
}

ft.ca.sz.500001 <- createfuture(170000,500001)
ft.ca.sz.500003 <- createfuture(170000,500003)
ft.ca.sz.500004 <- createfuture(170000,500004)
ft.ca.cat.500001 <- createfuture(170000,500001,n=2)
ft.ca.cat.500003 <- createfuture(170000,500003,n=2)
ft.ca.cat.500004 <- createfuture(170000,500004,n=2)
ft.or.sz.500001 <- createfuture(160000,500001)
ft.or.sz.500003 <- createfuture(160000,500003)
ft.or.sz.500004 <- createfuture(160000,500004)
ft.or.cat.500001 <- createfuture(160000,500001,n=2)
ft.or.cat.500003 <- createfuture(160000,500003,n=2)
ft.or.cat.500004 <- createfuture(160000,500004,n=2)
ft.ga.sz.500001 <- createfuture(550000,500001)
ft.ga.sz.500002 <- createfuture(550000,500002)
ft.ga.cat.500001 <- createfuture(550000,500001,n=2)
ft.ga.cat.500002 <- createfuture(550000,500002,n=2)

###########################################
#Propagation
############################################
##########################################
for(i in grow.loc.rng){
  #mo data
  mo.1 <- as.numeric(format(Sys.Date(),"%m")) - 1
  #december handler
  if(mo.1 == 0) mo.1 <- 12
  mo <- mo.1
  
  nam <- paste("prop.mo.",grow.loc[i],sep="")
  p.mo <- a[loc == grow.loc[i] & year == yr & month == mo & grw == 500007 & wo != 15 & wo != 99 & wo != 0, 
            list(orig.qty=sum(qty),prod=sum(plan),grss=sum(grpr)), 
            keyby = st.op]
  p.mo[is.na(p.mo)]<-0
  p.mo$perc <- p.mo$grss/p.mo$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(p.mo$orig.qty)
  c3 <- sum(p.mo$prod)
  c4 <- sum(p.mo$grss)
  c5 <- c4/c3
  newrow <- data.frame(st.op = c1, orig.qty = c2, prod = c3, grss = c4, perc = c5)
  p.mo <- rbind(p.mo, newrow)
  assign(nam,p.mo)
  
  #yr data
  nam <- paste("prop.yr.",grow.loc[i],sep="")
  p.yr <- a[loc == grow.loc[i] & year == yr & month <= mo & grw == 500007 & wo != 15 & wo != 99 & wo != 0, 
            list(orig.qty=sum(qty),prod=sum(plan)), 
            keyby = st.op]
  temp.a <- a[loc == grow.loc[i] & lb.yr == yr & lb.mo <= mo & grw == 500007 & wo != 15 & wo != 99 & wo != 0, 
              list(grss=sum(grpr)), 
              keyby = st.op]
  p.yr <- merge(p.yr,temp.a,by="st.op",all=TRUE)
  p.yr[is.na(p.yr)]<-0
  p.yr$perc <- p.yr$grss/p.yr$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(p.yr$orig.qty)
  c3 <- sum(p.yr$prod)
  c4 <- sum(p.yr$grss)
  c5 <- c4/c3
  newrow <- data.frame(st.op = c1, orig.qty = c2, prod = c3, grss = c4, perc = c5)
  p.yr <- rbind(p.yr, newrow)
  
  assign(nam,p.yr)
  
  #ft data
  mo <- as.numeric(mo.1) + 1
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  nam <- paste("prop.ft.",grow.loc[i],sep="")
  temp.a <- a[loc == grow.loc[i] & year == yr & month == mo & grw == 500007, 
              list(prod=sum(plan)), 
              keyby = st.op]
  
  mo <- as.numeric(mo.1) + 2
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.b <- a[loc == grow.loc[i] & year == yr & month == mo & grw == 500007, 
              list(prod=sum(plan)), 
              keyby = st.op]
  
  mo <- as.numeric(mo.1) + 3
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.c <- a[loc == grow.loc[i] & year == yr & month == mo & grw == 500007, 
              list(prod=sum(plan)), 
              keyby = st.op]
  
  temp.a <- merge(temp.a,temp.b,by="st.op",all=TRUE)
  temp.a <- merge(temp.a, temp.c,by="st.op",all=TRUE)
  temp.a[is.na(temp.a)]<-0
  temp.a$sums <- temp.a$prod.x + temp.a$prod.y + temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$prod.x)
  c3 <- sum(temp.a$prod.y)
  c4 <- sum(temp.a$prod)
  c5 <- c2 + c3 + c4
  newrow <- data.frame(st.op = c1, prod.x = c2, prod.y = c3, prod = c4, sums = c5)
  temp.a <- rbind(temp.a, newrow)
  
  assign(nam,temp.a)
}
###########################################
#write files to folder
###########################################
#############################################
#ca by size
##ga 1
ca.sz.1 <- merge(mo.ca.sz.500001,yr.ca.sz.500001,by="size",all=TRUE)
ca.sz.1 <- merge(ca.sz.1,ft.ca.sz.500001,by="size",all=TRUE)
ca.sz.1[is.na(ca.sz.1)] <- 0
write.csv(ca.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.1.csv")
##ga 3
ca.sz.3 <- merge(mo.ca.sz.500003,yr.ca.sz.500003,by="size",all=TRUE)
ca.sz.3 <- merge(ca.sz.3,ft.ca.sz.500003,by="size",all=TRUE)
ca.sz.3[is.na(ca.sz.3)] <- 0
write.csv(ca.sz.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.3.csv")
##ga 4
ca.sz.4 <- merge(mo.ca.sz.500004,yr.ca.sz.500004,by="size",all=TRUE)
ca.sz.4 <- merge(ca.sz.4,ft.ca.sz.500004,by="size",all=TRUE)
ca.sz.4[is.na(ca.sz.4)] <- 0
write.csv(ca.sz.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.4.csv")
#ca by cat
##ga 1
ca.cat.1 <- merge(mo.ca.cat.500001,yr.ca.cat.500001,by="cat",all=TRUE)
ca.cat.1 <- merge(ca.cat.1,ft.ca.cat.500001,by="cat",all=TRUE)
ca.cat.1[is.na(ca.cat.1)] <- 0
write.csv(ca.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.1.csv")
##ga 3
ca.cat.3 <- merge(mo.ca.cat.500003,yr.ca.cat.500003,by="cat",all=TRUE)
ca.cat.3 <- merge(ca.cat.3,ft.ca.cat.500003,by="cat",all=TRUE)
ca.cat.3[is.na(ca.cat.3)] <- 0
write.csv(ca.cat.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.3.csv")
##ga 4
ca.cat.4 <- merge(mo.ca.cat.500004,yr.ca.cat.500004,by="cat",all=TRUE)
ca.cat.4 <- merge(ca.cat.4,ft.ca.cat.500004,by="cat",all=TRUE)
ca.cat.4[is.na(ca.cat.4)] <- 0
write.csv(ca.cat.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.4.csv")
##prop
ca.prop <- merge(prop.mo.170000,prop.yr.170000,by="st.op",all=TRUE)
ca.prop <- merge(ca.prop,prop.ft.170000,by="st.op",all=TRUE)
ca.prop[is.na(ca.prop)] <- 0
write.csv(ca.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.zprop.csv")

#or by size
or.sz.1 <- merge(mo.or.sz.500001,yr.or.sz.500001,by="size",all=TRUE)
or.sz.1 <- merge(or.sz.1,ft.or.sz.500001,by="size",all=TRUE)
or.sz.1[is.na(or.sz.1)] <- 0
write.csv(or.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.1.csv")
##ga 3
or.sz.3 <- merge(mo.or.sz.500003,yr.or.sz.500003,by="size",all=TRUE)
or.sz.3 <- merge(or.sz.3,ft.or.sz.500003,by="size",all=TRUE)
or.sz.3[is.na(or.sz.3)] <- 0
write.csv(or.sz.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.3.csv")
##ga 4
or.sz.4 <- merge(mo.or.sz.500004,yr.or.sz.500004,by="size",all=TRUE)
or.sz.4 <- merge(or.sz.4,ft.or.sz.500004,by="size",all=TRUE)
or.sz.4[is.na(or.sz.4)] <- 0
write.csv(or.sz.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.4.csv")
#or by cat
##ga 1
or.cat.1 <- merge(mo.or.cat.500001,yr.or.cat.500001,by="cat",all=TRUE)
or.cat.1 <- merge(or.cat.1,ft.or.cat.500001,by="cat",all=TRUE)
or.cat.1[is.na(or.cat.1)] <- 0
write.csv(or.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.1.csv")
##ga 3
or.cat.3 <- merge(mo.or.cat.500003,yr.or.cat.500003,by="cat",all=TRUE)
or.cat.3 <- merge(or.cat.3,ft.or.cat.500003,by="cat",all=TRUE)
or.cat.3[is.na(or.cat.3)] <- 0
write.csv(or.cat.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.3.csv")
##ga 4
or.cat.4 <- merge(mo.or.cat.500004,yr.or.cat.500004,by="cat",all=TRUE)
or.cat.4 <- merge(or.cat.4,ft.or.cat.500004,by="cat",all=TRUE)
or.cat.4[is.na(or.cat.4)] <- 0
write.csv(or.cat.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.4.csv")
##prop
or.prop <- merge(prop.mo.160000,prop.yr.160000,by="st.op",all=TRUE)
or.prop <- merge(or.prop,prop.ft.160000,by="st.op",all=TRUE)
or.prop[is.na(or.prop)] <- 0
write.csv(or.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.zprop.csv")

#ga by size
#ga 1
ga.sz.1 <- merge(mo.ga.sz.500001,yr.ga.sz.500001,by="size",all=TRUE)
ga.sz.1 <- merge(ga.sz.1,ft.ga.sz.500001,by="size",all=TRUE)
ga.sz.1[is.na(ga.sz.1)] <- 0
write.csv(ga.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.sz.1.csv")
##ga 2
ga.sz.2 <- merge(mo.ga.sz.500002,yr.ga.sz.500002,by="size",all=TRUE)
ga.sz.2 <- merge(ga.sz.2,ft.ga.sz.500002,by="size",all=TRUE)
ga.sz.2[is.na(ga.sz.2)] <- 0
write.csv(ga.sz.2,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.sz.2.csv")
#ga by cat
##ga 1
ga.cat.1 <- merge(mo.ga.cat.500001,yr.ga.cat.500001,by="cat",all=TRUE)
ga.cat.1 <- merge(ga.cat.1,ft.ga.cat.500001,by="cat",all=TRUE)
ga.cat.1[is.na(ga.cat.1)] <- 0
write.csv(ga.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.cat.1.csv")
##ga 2
ga.cat.2 <- merge(mo.ga.cat.500002,yr.ga.cat.500002,by="cat",all=TRUE)
ga.cat.2 <- merge(ga.cat.2,ft.ga.cat.500002,by="cat",all=TRUE)
ga.cat.2[is.na(ga.cat.2)] <- 0
write.csv(ga.cat.2,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.cat.2.csv")
##prop
ga.prop <- merge(prop.mo.550000,prop.yr.550000,by="st.op",all=TRUE)
ga.prop <- merge(ga.prop,prop.ft.550000,by="st.op",all=TRUE)
ga.prop[is.na(ga.prop)] <- 0
write.csv(ga.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.zprop.csv")
###########################################
#SUMMARIES
####################################
####################################
mo.1 <- as.numeric(format(Sys.Date(),"%m"))-1 
#december handler
if(mo.1 == 0) mo.1 <- 12
mo <- mo.1
##################################
#summary by mo 
########################################
#####################################

for(i in grow.loc.rng){
  nam <- paste("mo.", grow.loc[i],".summary",sep="")
  temp.a <- a[loc == grow.loc[i] & year == yr & month == mo & wo != 15 & wo != 99 & wo != 0, 
              list(orig.qty=sum(equ),grss=sum(gre)), 
              keyby = grw]
  #get plan from planbygr.csv file
  temp.a1 <- plan.grower[LOCATION==grow.loc[i] & MONTH==mo, list(prod=sum(PLAN)),by=grw]
  
  temp.a <- data.table(grw=temp.a$grw,orig.qty=temp.a$orig.qty,prod=temp.a1$prod,grss=temp.a$grss)
  
  temp.a$perc.comp <- temp.a$grss/temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$orig.qty)
  c3 <- sum(temp.a$prod)
  c4 <- sum(temp.a$grss)
  c5 <- c4/c3
  newrow <- data.frame(grw = c1, orig.qty = c2, prod = c3, grss = c4, perc.comp = c5)
  temp.a <- rbind(temp.a, newrow)
  
  assign(nam,temp.a)
}
##################################
#summary by yr 
########################################
#########################################
for(i in grow.loc.rng){
  nam <- paste("yr.", grow.loc[i],".summary",sep="")
  temp.a <- a[loc == grow.loc[i] & year == yr & month <= mo & wo != 15 & wo != 99 & wo != 0, 
              list(orig.qty=sum(equ)), 
              keyby = grw]
  #get plan from planbygr.csv file
  temp.a1 <- plan.grower[LOCATION==grow.loc[i] & MONTH<=mo, list(prod=sum(PLAN)),by=grw]
  
  temp.b <- a[loc == grow.loc[i] & lb.yr == yr & lb.mo <= mo & wo != 15 & wo != 99 & wo != 0, 
              list(grss=sum(gre)), 
              keyby = grw]
  
  temp.a <- data.table(grw=temp.a$grw,orig.qty=temp.a$orig.qty,prod=temp.a1$prod,grss=temp.b$grss)
    
  temp.a$perc.comp <- temp.a$grss/temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$orig.qty)
  c3 <- sum(temp.a$prod)
  c4 <- sum(temp.a$grss)
  c5 <- c4/c3
  newrow <- data.frame(grw = c1, orig.qty = c2, prod = c3, grss = c4, perc.comp = c5)
  temp.a <- rbind(temp.a, newrow)
  
  assign(nam,temp.a)
}
##################################
#summary by ft 
########################################
######################################
for(i in or.ca.rng){
  
  mo.1 <- as.numeric(format(Sys.Date(),"%m")) 
  mo <- mo.1
  
  nam <- paste("ft.",grow.loc[i],".summary",sep="")
  temp.a <- a[loc == grow.loc[i] & year == yr & month == mo, 
              list(prod=sum(equ.pr)), 
              keyby = grw]
  
  mo <- as.numeric(mo.1) + 1
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.b <- a[loc == grow.loc[i] & year == yr & month == mo, 
              list(prod=sum(equ.pr)), 
              keyby = grw]
  
  mo <- as.numeric(mo.1) + 2
  if(mo == 13){
    mo <- 1
  }else if(mo == 14){
    mo <- 2
  }else if(mo == 15){
    mo <- 3
  }
  
  temp.c <- a[loc == grow.loc[i] & year == yr & month == mo, 
              list(prod=sum(equ.pr)), 
              keyby = grw]
  
  temp.a <- merge(temp.a,temp.b,by="grw",all=TRUE)
  temp.a <- merge(temp.a, temp.c,by="grw",all=TRUE)
  temp.a[is.na(temp.a)]<-0
  temp.a$Total <- temp.a$prod.x + temp.a$prod.y + temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$prod.x)
  c3 <- sum(temp.a$prod.y)
  c4 <- sum(temp.a$prod)
  c5 <- sum(temp.a$Total)
  newrow <- data.frame(grw = c1, prod.x = c2, prod.y = c3, prod = c4, Total = c5)
  temp.a <- rbind(temp.a, newrow)
  assign(nam,temp.a)
}
##################################
#merge summaries and write to csv
##################################
##################################

ca.summary <- merge(mo.170000.summary,yr.170000.summary,by="grw",all=TRUE)
ca.summary <- merge(ca.summary,ft.170000.summary,by="grw",all=TRUE)
ca.summary[is.na(ca.summary)] <- 0
write.csv(ca.summary,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.summary.csv")

or.summary <- merge(mo.160000.summary,yr.160000.summary,by="grw",all=TRUE)
or.summary <- merge(or.summary,ft.160000.summary,by="grw",all=TRUE)
or.summary[is.na(or.summary)] <- 0
write.csv(or.summary,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.summary.csv")

ga.summary <- merge(mo.550000.summary,yr.550000.summary,by="grw",all=TRUE)
ga.summary <- merge(ga.summary,ft.550000.summary,by="grw",all=TRUE)
ga.summary[is.na(ga.summary)] <- 0
write.csv(ga.summary,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.summary.csv")
######################################
#all locations summary
######################################
loc.sums <- list(or.summary,ca.summary,ga.summary)
loc.nams <- c("Oregon","California","Georgia")
temp.tab <- or.summary[orig.qty.x < 0,]
for(i in 1:length(loc.sums)){
  temp.a <- loc.sums[[i]][nrow(loc.sums[[i]]),]
  temp.a[1, 1] <- loc.nams[i]
  temp.tab <- rbind(temp.tab,temp.a)
}

####READ IN PLAN DATA AND REPLACE CURRENT PLAN####

#GET ACTUAL PLAN DATA AS OF BEGINNING OF YEAR
curr.month <- as.numeric(format(Sys.Date(),"%m"))-1
temp.tab <- as.data.table(temp.tab)
#replace current month plan numbers
temp.tab[[3]][1] <- plan.data$OR_SEP[curr.month]
temp.tab[[3]][2] <- plan.data$CA_SEP[curr.month]
temp.tab[[3]][3] <- plan.data$GA_SEP[curr.month]
temp.tab[[7]][1] <- plan.data$OREGON[curr.month]
temp.tab[[7]][2] <- plan.data$CALIFORNIA[curr.month]
temp.tab[[7]][3] <- plan.data$GEORGIA[curr.month]
#update percent complete colums
temp.tab[[5]] <- temp.tab[[4]]/temp.tab[[3]]
temp.tab[[9]] <- temp.tab[[8]]/temp.tab[[7]]
#Add totals row
totals.row <- data.table("Totals",sum(temp.tab[[2]]),sum(temp.tab[[3]]),sum(temp.tab[[4]]),
                         round(sum(temp.tab[[4]])/sum(temp.tab[[3]]),4),
                         sum(temp.tab[[6]]),sum(temp.tab[[7]]),sum(temp.tab[[8]]),
                         round(sum(temp.tab[[8]])/sum(temp.tab[[7]]),4),
                         sum(temp.tab[[10]]),sum(temp.tab[[11]]),sum(temp.tab[[12]]),sum(temp.tab[[13]]))
all.locs.totals <- rbind(temp.tab,totals.row,use.names=FALSE)
write.csv(all.locs.totals,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/all.locs.total.csv")
###########################################
#CUMULATIVE CHARTS
###########################################
#change year to last year
yr <- as.numeric(format(Sys.Date(),"%y")) - 1

mo.labs <-c("1","2","3","4","5","6","7","8","9","10","11","12")
gr.areas <- c(500001,500003,500004,500007)
ga.gr.areas <- c(500001,500002,500007)
gr.labs <- c("1","3","4","7")
ga.gr.labs <- c("1","2","7")
#function for all months last year completed
moFunc <- function(locs, yr, mo.labs, a){
  for(i in 1:12){
    oldProd <- a[loc == locs & lb.yr == yr & lb.mo == mo.labs[i] & wo != 15 & wo != 99, 
                 list(gross=sum(gre)), 
                 keyby = grw]
    if(i == 1){
      one <- oldProd
    } else one <- merge(one, oldProd,by="grw",all=TRUE)
    
    newnm <- as.character(mo.labs[i])
    setnames(one,"gross",newnm)
  }
  one[is.na(one)]<-0
  return(one)
}
#function for all months this year planned
moFuncEQU <- function(locs, yr, mo.labs, a){
  for(i in 1:mo){
    oldProd <- plan.grower[LOCATION == locs & MONTH == mo.labs[i], 
                 list(plan=sum(PLAN)), keyby = grw]
    if(i == 1){
      one <- oldProd
    } else one <- merge(one, oldProd,by="grw",all=TRUE)
    newnm <- as.character(mo.labs[i])
    setnames(one,"plan",newnm)
  }
  
  mo <- as.numeric(format(Sys.Date(),"%m")) - 1
  mo.1 <- mo + 1  
  for(i in mo.1:12){
    oldProd <- a[loc == locs & year == yr & month == mo.labs[i], 
                 list(plan=sum(equ.pr)), 
                 keyby = grw]
    
    one <- merge(one, oldProd,by="grw",all=TRUE)
    
    newnm <- as.character(mo.labs[i])
    setnames(one,"plan",newnm)
  }
  one[is.na(one)]<-0
  return(one)
}

#ca
ca.old <- moFunc(170000,yr,mo.labs,a)
#or
or.old <- moFunc(160000,yr,mo.labs,a)
#ga
ga.old <- moFunc(550000,yr,mo.labs,a)

######################################
#last year Oregon
######################################
#separate growing areas into own vector
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(or.old,grw==gr.areas[i],select=mo.labs)))
  gra <- gr.areas[i]
  nam <- paste("or.LY.",gr.labs[i],sep="")
  assign(nam,area.num)
}

######################################
#last year california
######################################
#separate growing areas into own vector
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(ca.old,grw==gr.areas[i],select=mo.labs)))
  gra <- gr.areas[i]
  nam <- paste("ca.LY.",gr.labs[i],sep="")
  assign(nam,area.num)
}

######################################
#last year georgia
######################################
#separate growing areas into own vector
for(i in 1:length(ga.gr.areas)){
  area.num <- as.vector(t(subset(ga.old,grw==ga.gr.areas[i],select=mo.labs)))
  gra <- gr.areas[i]
  nam <- paste("ga.LY.",ga.gr.labs[i],sep="")
  assign(nam,area.num)
}
######################################
#this year prod plan
######################################
#setup all locations
#change year to current year
yr <- as.numeric(format(Sys.Date(),"%y"))

mo <- as.numeric(format(Sys.Date(),"%m"))-1 

#ca
ca.plan <- moFuncEQU(170000,yr,mo.labs,a)
#or
or.plan <- moFuncEQU(160000,yr,mo.labs,a)
#ga
ga.plan <- moFuncEQU(550000,yr,mo.labs,a)

#separate growing areas into own vector
#oregon
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(or.plan,grw==gr.areas[i],select=mo.labs)))
  nam <- paste("or.plan.",gr.labs[i],sep="")
  assign(nam,area.num)
}
#california
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(ca.plan,grw==gr.areas[i],select=mo.labs)))
  nam <- paste("ca.plan.",gr.labs[i],sep="")
  assign(nam,area.num)
}
#georgia
for(i in 1:length(ga.gr.areas)){
  area.num <- as.vector(t(subset(ga.plan,grw==ga.gr.areas[i],select=mo.labs)))
  nam <- paste("ga.plan.",ga.gr.labs[i],sep="")
  assign(nam,area.num)
}
######################################
#YTD numbers
######################################
mo.cnt <- as.numeric(format(Sys.Date(),"%m"))-1
morng <- 1:mo.cnt
mo.labs.2 <- mo.labs[1:mo.cnt]
yr <- 14
#function to get YTD numbers
moFuncYTD <- function(locs, yr, mo.labs, a, rng){
  for(i in rng){
    oldProd <- a[loc == locs & lb.yr == yr & lb.mo == mo.labs[i] & wo != 15 & wo != 99, 
                 list(gross=sum(gre)), 
                 keyby = grw]
    if(i == 1){
      one <- oldProd
    } else one <- merge(one, oldProd,by="grw",all=TRUE)
    
    newnm <- mo.labs[i]
    setnames(one,"gross",newnm)
  }
  one[is.na(one)]<-0
  return(one)
}

or.YTD <- moFuncYTD(160000, yr, mo.labs, a, morng)
ca.YTD <- moFuncYTD(170000, yr, mo.labs, a, morng)
ga.YTD <- moFuncYTD(550000, yr, mo.labs, a, morng)

##separate growing areas into own vector
#oregon
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(or.YTD,grw==gr.areas[i],select=mo.labs.2)))
  nam <- paste("or.YTD.",gr.labs[i],sep="")
  assign(nam,area.num)
}
#california
for(i in 1:length(gr.areas)){
  area.num <- as.vector(t(subset(ca.YTD,grw==gr.areas[i],select=mo.labs.2)))
  nam <- paste("ca.YTD.",gr.labs[i],sep="")
  assign(nam,area.num)
}
#georgia
for(i in 1:length(ga.gr.areas)){
  area.num <- as.vector(t(subset(or.YTD,grw==ga.gr.areas[i],select=mo.labs.2)))
  nam <- paste("ga.YTD.",ga.gr.labs[i],sep="")
  assign(nam,area.num)
}

################################
#for plotting
################################
addsums <- function(area){
  i = 1
  sums <- list()
  newone <- 0
  while(i < 13){
    newone <- area[[i]] + newone
    sums[[i]] <- newone
    i = i + 1
  }
  sums <- as.numeric(sums)
}

####################################
#LY cumulative
####################################

#oregon LY cumulative vectors
or.loc.vec <- list()
or.loc.vec[[1]] <- or.LY.1
or.loc.vec[[2]] <- or.LY.3
or.loc.vec[[3]] <- or.LY.4
or.loc.vec[[4]] <- or.LY.7

for(i in 1:length(gr.areas)){
  temp <- addsums(or.loc.vec[[i]])
  nam <- paste("or.ly.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(or.loc.vec)

#california LY cumulative vectors
ca.loc.vec <- list()
ca.loc.vec[[1]] <- ca.LY.1
ca.loc.vec[[2]] <- ca.LY.3
ca.loc.vec[[3]] <- ca.LY.4
ca.loc.vec[[4]] <- ca.LY.7

for(i in 1:length(gr.areas)){
  temp <- addsums(ca.loc.vec[[i]])
  nam <- paste("ca.ly.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ca.loc.vec)

#georgia LY cumulative vectors
ga.loc.vec <- list()
ga.loc.vec[[1]] <- ga.LY.1
ga.loc.vec[[2]] <- ga.LY.2
ga.loc.vec[[3]] <- ga.LY.7

for(i in 1:length(ga.gr.areas)){
  temp <- addsums(ga.loc.vec[[i]])
  nam <- paste("ga.ly.",ga.gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ga.loc.vec)

#######################################
#plan cumulative
#######################################
#oregon plan cumulative vectors
or.loc.vec <- list()
or.loc.vec[[1]] <- or.plan.1
or.loc.vec[[2]] <- or.plan.3
or.loc.vec[[3]] <- or.plan.4
or.loc.vec[[4]] <- or.plan.7

for(i in 1:length(gr.areas)){
  temp <- addsums(or.loc.vec[[i]])
  nam <- paste("or.plan.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(or.loc.vec)

#california plan cumulative vectors
ca.loc.vec <- list()
ca.loc.vec[[1]] <- ca.plan.1
ca.loc.vec[[2]] <- ca.plan.3
ca.loc.vec[[3]] <- ca.plan.4
ca.loc.vec[[4]] <- ca.plan.7

for(i in 1:length(gr.areas)){
  temp <- addsums(ca.loc.vec[[i]])
  nam <- paste("ca.plan.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ca.loc.vec)

#georgia plan cumulative vectors
ga.loc.vec <- list()
ga.loc.vec[[1]] <- ga.plan.1
ga.loc.vec[[2]] <- ga.plan.2
ga.loc.vec[[3]] <- ga.plan.7

for(i in 1:length(ga.gr.areas)){
  temp <- addsums(ga.loc.vec[[i]])
  nam <- paste("ga.plan.",ga.gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ga.loc.vec)
#######################################
#YTD cumulative
#######################################
addsumsYTD <- function(area, months.cnt){
  i = 1
  sums <- list()
  newone <- 0
  while(i <= months.cnt){
    newone <- area[[i]] + newone
    sums[[i]] <- newone
    i = i + 1
  }
  sums <- as.numeric(sums)
}

#oregon YTD cumulative vectors
or.loc.vec <- list()
or.loc.vec[[1]] <- or.YTD.1
or.loc.vec[[2]] <- or.YTD.3
or.loc.vec[[3]] <- or.YTD.4
or.loc.vec[[4]] <- or.YTD.7

for(i in 1:length(gr.areas)){
  temp <- addsumsYTD(or.loc.vec[[i]],mo.cnt)
  nam <- paste("or.YTD.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(or.loc.vec)

#california YTD cumulative vectors
ca.loc.vec <- list()
ca.loc.vec[[1]] <- ca.YTD.1
ca.loc.vec[[2]] <- ca.YTD.3
ca.loc.vec[[3]] <- ca.YTD.4
ca.loc.vec[[4]] <- ca.YTD.7

for(i in 1:length(gr.areas)){
  temp <- addsumsYTD(ca.loc.vec[[i]],mo.cnt)
  nam <- paste("ca.YTD.",gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ca.loc.vec)

#georgia YTD cumulative vectors
ga.loc.vec <- list()
ga.loc.vec[[1]] <- ga.YTD.1
ga.loc.vec[[2]] <- ga.YTD.2
ga.loc.vec[[3]] <- ga.YTD.7

for(i in 1:length(ga.gr.areas)){
  temp <- addsumsYTD(ga.loc.vec[[i]],mo.cnt)
  nam <- paste("ga.YTD.",ga.gr.labs[[i]],".cum",sep="")
  assign(nam,temp)
}

remove(ga.loc.vec)

#############################################
#location totals
#############################################
##Function for getting cumulative by month for each location
loc.tot.vec <- function(dframe){
  mos <- length(names(dframe))
  cols <- list()  
  for(i in 2:mos){
    #total each month into list of twelve totals
    colasn <- i - 1
    cols[[colasn]] <- sum(as.numeric(dframe[[i]]))
  }
  colsCum <- list()
  for(i in 2:mos-1){
    n <- i - 1
    colsCum[[i]] <- sum(as.numeric(cols[1:n])) + cols[[i]]
  }
  colsCum[[1]] <- cols[[1]]
  colsCum <- unlist(colsCum)
}

#or.plan.tots <- loc.tot.vec(or.plan)
or.plan.tots <- as.vector(plan.data$OREGON)
or.ly.tots <- loc.tot.vec(or.old)
or.YTD.tots <- loc.tot.vec(or.YTD)

#ca.plan.tots <- loc.tot.vec(ca.plan)
ca.plan.tots <- as.vector(plan.data$CALIFORNIA)
ca.ly.tots <- loc.tot.vec(ca.old)
ca.YTD.tots <- loc.tot.vec(ca.YTD)

#ga.plan.tots <- loc.tot.vec(ga.plan)
ga.plan.tots <- as.vector(plan.data$GEORGIA)
ga.ly.tots <- loc.tot.vec(ga.old)
ga.YTD.tots <- loc.tot.vec(ga.YTD)

all.locs.ly <- or.ly.tots + ca.ly.tots + ga.ly.tots
all.locs.ytd <- or.YTD.tots + ca.YTD.tots + ga.YTD.tots
all.locs.plan <- or.plan.tots + ca.plan.tots + ga.plan.tots

##############################################
#plotting 
##############################################
#Get YTD % complete for each location
or.perc.comp <- paste(round((all.locs.totals[[9]][1])*100,digits=2),"%",sep="")
ca.perc.comp <- paste(round((all.locs.totals[[9]][2])*100,digits=2),"%",sep="")
ga.perc.comp <- paste(round((all.locs.totals[[9]][3])*100,digits=2),"%",sep="")
all.perc.comp <- paste(round((all.locs.totals[[9]][4])*100,digits=2),"%",sep="")

canplots <- function(plan,ly,ytd,perc.comp){
  #create empty plot
  mxplan <- max(plan)
  mxly <- max(ly)
  
  if(mxplan > mxly){
    plotmat <- plan
  } else plotmat <- ly
  
  par(mar=c(5,4,5,2)+0.1)
  
  plot(plotmat, 
       type = "n", 
       lwd = 2, 
       lty = 1,
       yaxt = "n",
       ylab = "",
       xlab = "Months")
  
  locs <- substr(deparse(substitute(plan)),1,2)
  if(locs=="or"){
    loc = "Oregon"
  } else if(locs=="ca"){
    loc = "California"
  } else if(locs=="ga"){
    loc = "Georgia"
  } else loc = "All Locations"
  
  topTitle <- paste(loc," Canning by Month",sep="")
  lowTitle <- paste(perc.comp," of Total Year Plan Complete")
  
  title(topTitle, cex.main = 1.8, font.main = 1.5, col.main = "darkgreen")
  mtext(lowTitle,cex = 1.1)
  
  #add ablines and ticks
  ylns <- c(1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,10000000,11000000,12000000,13000000,14000000,15000000,16000000,
            17000000,18000000,19000000,20000000,21000000,22000000,23000000,24000000,25000000,26000000,27000000,28000000,29000000,30000000,31000000,32000000,
            33000000,34000000,35000000,36000000,37000000,38000000,39000000,40000000)
  ytxt <- c("1M","2M","3M","4M","5M","6M","7M","8M","9M","10M","11M","12M","13M","14M","15M","16M",
            "17M","18M","19M","20M","21M","22M","23M","24M","25M","26M","27M","28M","29M","30M","31M","32M",
            "33M","34M","35M","36M","37M","38M","39M","40M")
  axis(2,at=ylns,labels=ytxt, las=1)
  abline(h=ylns,col="gray88",lty="dashed")
  xlns <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  abline(v=xlns,col="gray88",lty="dashed")
  #add lines
  lines(ly, col = "grey", lwd = 2, lty="dotted")
  lines(plan, col = "azure4", lwd = 2)
  lines(ytd, col = "red", lwd = 5)
  
  legend("topleft", 
         c("2013","2014 Planned","2014 YTD"), 
         col=c("grey","azure4","red"), 
         lty=c(3,1,1),
         lwd=c(2,2,5),
         cex=0.5)    
}

orplt <- canplots(or.plan.tots,or.ly.tots,or.YTD.tots,or.perc.comp)
caplt <- canplots(ca.plan.tots,ca.ly.tots,ca.YTD.tots,ca.perc.comp)
gaplt <- canplots(ga.plan.tots,ga.ly.tots,ga.YTD.tots,ga.perc.comp)
allplt <- canplots(all.locs.plan,all.locs.ly,all.locs.ytd,all.perc.comp)
#############################################
#EQU DATA
#############################################
#############################################
yr <- as.numeric(format(Sys.Date(),"%y"))
#function to create dataframe for a given location and growing area for complete YTD or just month
createframeEQU <- function(location, GA, n=1, year=FALSE){
  #n is 1 for "size" or 2 for "cat"
  kbys = c("size","cat")
  if(year==TRUE){
    temp.a <- a[loc==location & year==yr & month<=mo & grw==GA & wo!=15 & wo!=99 & wo!=0,
                list(orig.qty=sum(equ),prod=sum(equ.pr)),keyby=eval(kbys[n])]
    
    temp.b <- a[loc==location & lb.yr==yr & lb.mo<=mo & grw==GA & wo!=15 & wo!=99 & wo!=0,
                list(grss=sum(gre)),keyby=eval(kbys[n])]
    temp.a <- merge(temp.a,temp.b,by=eval(kbys[n]),all=TRUE)
    temp.a[is.na(temp.a)] <- 0
    temp.a$perc.comp <- temp.a$grss/temp.a$prod
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$orig.qty)
    c3 <- sum(temp.a$prod)
    c4 <- sum(temp.a$grss)
    c5 <- c4/c3
    if(n==1){
      newrow <- data.frame(size=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    }else newrow <- data.frame(cat=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    
    temp.a <- rbind(temp.a,newrow)
  }else{
    temp.a <- a[loc==location & year==yr & month==mo & grw==GA & wo!=15 & wo!=99 & wo!=0,
                list(orig.qty=sum(equ),prod=sum(equ.pr),grss=sum(gre)),keyby=eval(kbys[n])]
    temp.a$perc.comp <- temp.a$grss/temp.a$prod
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$orig.qty)
    c3 <- sum(temp.a$prod)
    c4 <- sum(temp.a$grss)
    c5 <- c4/c3
    if(n==1){
      newrow <- data.frame(size=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    }else newrow <- data.frame(cat=c1,orig.qty=c2,prod=c3,grss=c4,perc.comp=c5)
    
    temp.a <- rbind(temp.a,newrow)
  }
  temp.a
}

yr.ca.sz.500001 <- createframeEQU(170000,500001,year=TRUE)
yr.ca.sz.500003 <- createframeEQU(170000,500003,year=TRUE)
yr.ca.sz.500004 <- createframeEQU(170000,500004,year=TRUE)
yr.ca.cat.500001 <- createframeEQU(170000,500001,n=2,year=TRUE)
yr.ca.cat.500003 <- createframeEQU(170000,500003,n=2,year=TRUE)
yr.ca.cat.500004 <- createframeEQU(170000,500004,n=2,year=TRUE)
yr.or.sz.500001 <- createframeEQU(160000,500001,year=TRUE)
yr.or.sz.500003 <- createframeEQU(160000,500003,year=TRUE)
yr.or.sz.500004 <- createframeEQU(160000,500004,year=TRUE)
yr.or.cat.500001 <- createframeEQU(160000,500001,n=2,year=TRUE)
yr.or.cat.500003 <- createframeEQU(160000,500003,n=2,year=TRUE)
yr.or.cat.500004 <- createframeEQU(160000,500004,n=2,year=TRUE)
yr.ga.sz.500001 <- createframeEQU(550000,500001,year=TRUE)
yr.ga.sz.500002 <- createframeEQU(550000,500002,year=TRUE)
yr.ga.cat.500001 <- createframeEQU(550000,500001,n=2,year=TRUE)
yr.ga.cat.500002 <- createframeEQU(550000,500002,n=2,year=TRUE)

mo.ca.sz.500001 <- createframeEQU(170000,500001)
mo.ca.sz.500003 <- createframeEQU(170000,500003)
mo.ca.sz.500004 <- createframeEQU(170000,500004)
mo.ca.cat.500001 <- createframeEQU(170000,500001,n=2)
mo.ca.cat.500003 <- createframeEQU(170000,500003,n=2)
mo.ca.cat.500004 <- createframeEQU(170000,500004,n=2)
mo.or.sz.500001 <- createframeEQU(160000,500001)
mo.or.sz.500003 <- createframeEQU(160000,500003)
mo.or.sz.500004 <- createframeEQU(160000,500004)
mo.or.cat.500001 <- createframeEQU(160000,500001,n=2)
mo.or.cat.500003 <- createframeEQU(160000,500003,n=2)
mo.or.cat.500004 <- createframeEQU(160000,500004,n=2)
mo.ga.sz.500001 <- createframeEQU(550000,500001)
mo.ga.sz.500002 <- createframeEQU(550000,500002)
mo.ga.cat.500001 <- createframeEQU(550000,500001,n=2)
mo.ga.cat.500002 <- createframeEQU(550000,500002,n=2)
###########################################
#Propagation
############################################

for(i in grow.loc.rng){
  #mo data
  mo.1 <- as.numeric(format(Sys.Date(),"%m")) - 1
  #december handler
  if(mo.1 == 0) mo.1 <- 12
  mo <- mo.1
  
  nam <- paste("prop.mo.",grow.loc[i],sep="")
  p.mo <- a[loc == grow.loc[i] & year == 14 & month == mo & grw == 500007 & wo != 15 & wo != 99 & wo != 0, 
            list(orig.qty=sum(equ),prod=sum(equ.pr),grss=sum(gre)), 
            keyby = st.op]
  p.mo[is.na(p.mo)]<-0
  p.mo$perc <- p.mo$grss/p.mo$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(p.mo$orig.qty)
  c3 <- sum(p.mo$prod)
  c4 <- sum(p.mo$grss)
  c5 <- c4/c3
  newrow <- data.frame(st.op = c1, orig.qty = c2, prod = c3, grss = c4, perc = c5)
  p.mo <- rbind(p.mo, newrow)
  assign(nam,p.mo)
  
  #yr data
  nam <- paste("prop.yr.",grow.loc[i],sep="")
  p.yr <- a[loc == grow.loc[i] & year == 14 & month <= mo & grw == 500007 & wo != 15 & wo != 99 & wo != 0, 
            list(orig.qty=sum(equ),prod=sum(equ.pr)), 
            keyby = st.op]
  temp.a <- a[loc == grow.loc[i] & lb.yr == 14 & lb.mo <= mo & grw == 500007 & wo != 15 & wo != 99 & wo!=0, 
              list(grss=sum(gre)), 
              keyby = st.op]
  p.yr <- merge(p.yr,temp.a,by="st.op",all=TRUE)
  p.yr[is.na(p.yr)]<-0
  p.yr$perc <- p.mo$grss/p.mo$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(p.yr$orig.qty)
  c3 <- sum(p.yr$prod)
  c4 <- sum(p.yr$grss)
  c5 <- c4/c3
  newrow <- data.frame(st.op = c1, orig.qty = c2, prod = c3, grss = c4, perc = c5)
  p.yr <- rbind(p.yr, newrow)
  
  assign(nam,p.yr)
  
  #ft data
  mo <- as.numeric(mo.1) + 1
  
  nam <- paste("prop.ft.",grow.loc[i],sep="")
  temp.a <- a[loc == grow.loc[i] & year == 14 & month == mo & grw == 500007, 
              list(prod=sum(equ.pr)), 
              keyby = st.op]
  
  mo <- as.numeric(mo.1) + 2
  
  temp.b <- a[loc == grow.loc[i] & year == 14 & month == mo & grw == 500007, 
              list(prod=sum(equ.pr)), 
              keyby = st.op]
  
  mo <- as.numeric(mo.1) + 3
  
  temp.c <- a[loc == grow.loc[i] & year == 14 & month == mo & grw == 500007, 
              list(prod=sum(equ.pr)), 
              keyby = st.op]
  
  temp.a <- merge(temp.a,temp.b,by="st.op",all=TRUE)
  temp.a <- merge(temp.a, temp.c,by="st.op",all=TRUE)
  temp.a[is.na(temp.a)]<-0
  temp.a$sums <- temp.a$prod.x + temp.a$prod.y + temp.a$prod
  #add totals row
  c1 <- "zTotals"
  c2 <- sum(temp.a$prod.x)
  c3 <- sum(temp.a$prod.y)
  c4 <- sum(temp.a$prod)
  c5 <- c2 + c3 + c4
  newrow <- data.frame(st.op = c1, prod.x = c2, prod.y = c3, prod = c4, sums = c5)
  temp.a <- rbind(temp.a, newrow)
  
  assign(nam,temp.a)
}

###########################################
#write files to folder
###########################################
#ca by size
##ga 1
ca.sz.1 <- merge(mo.ca.sz.500001,yr.ca.sz.500001,by="size",all=TRUE)
ca.sz.1 <- merge(ca.sz.1,ft.ca.sz.500001,by="size",all=TRUE)
ca.sz.1[is.na(ca.sz.1)] <- 0
write.csv(ca.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.1.EQU.csv")
##ga 3
ca.sz.3 <- merge(mo.ca.sz.500003,yr.ca.sz.500003,by="size",all=TRUE)
ca.sz.3 <- merge(ca.sz.3,ft.ca.sz.500003,by="size",all=TRUE)
ca.sz.3[is.na(ca.sz.3)] <- 0
write.csv(ca.sz.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.3.EQU.csv")
##ga 4
ca.sz.4 <- merge(mo.ca.sz.500004,yr.ca.sz.500004,by="size",all=TRUE)
ca.sz.4 <- merge(ca.sz.4,ft.ca.sz.500004,by="size",all=TRUE)
ca.sz.4[is.na(ca.sz.4)] <- 0
write.csv(ca.sz.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.sz.4.EQU.csv")
#ca by cat
##ga 1
ca.cat.1 <- merge(mo.ca.cat.500001,yr.ca.cat.500001,by="cat",all=TRUE)
ca.cat.1 <- merge(ca.cat.1,ft.ca.cat.500001,by="cat",all=TRUE)
ca.cat.1[is.na(ca.cat.1)] <- 0
write.csv(ca.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.1.EQU.csv")
##ga 3
ca.cat.3 <- merge(mo.ca.cat.500003,yr.ca.cat.500003,by="cat",all=TRUE)
ca.cat.3 <- merge(ca.cat.3,ft.ca.cat.500003,by="cat",all=TRUE)
ca.cat.3[is.na(ca.cat.3)] <- 0
write.csv(ca.cat.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.3.EQU.csv")
##ga 4
ca.cat.4 <- merge(mo.ca.cat.500004,yr.ca.cat.500004,by="cat",all=TRUE)
ca.cat.4 <- merge(ca.cat.4,ft.ca.cat.500004,by="cat",all=TRUE)
ca.cat.4[is.na(ca.cat.4)] <- 0
write.csv(ca.cat.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.cat.4.EQU.csv")
##prop
ca.prop <- merge(prop.mo.170000,prop.yr.170000,by="st.op",all=TRUE)
ca.prop <- merge(ca.prop,prop.ft.170000,by="st.op",all=TRUE)
ca.prop[is.na(ca.prop)] <- 0
write.csv(ca.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ca.zprop.EQU.csv")

#or by size
or.sz.1 <- merge(mo.or.sz.500001,yr.or.sz.500001,by="size",all=TRUE)
or.sz.1 <- merge(or.sz.1,ft.or.sz.500001,by="size",all=TRUE)
or.sz.1[is.na(or.sz.1)] <- 0
write.csv(or.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.1.EQU.csv")
##ga 3
or.sz.3 <- merge(mo.or.sz.500003,yr.or.sz.500003,by="size",all=TRUE)
or.sz.3 <- merge(or.sz.3,ft.or.sz.500003,by="size",all=TRUE)
or.sz.3[is.na(or.sz.3)] <- 0
write.csv(or.sz.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.3.EQU.csv")
##ga 4
or.sz.4 <- merge(mo.or.sz.500004,yr.or.sz.500004,by="size",all=TRUE)
or.sz.4 <- merge(or.sz.4,ft.or.sz.500004,by="size",all=TRUE)
or.sz.4[is.na(or.sz.4)] <- 0
write.csv(or.sz.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.sz.4.EQU.csv")
#or by cat
##ga 1
or.cat.1 <- merge(mo.or.cat.500001,yr.or.cat.500001,by="cat",all=TRUE)
or.cat.1 <- merge(or.cat.1,ft.or.cat.500001,by="cat",all=TRUE)
or.cat.1[is.na(or.cat.1)] <- 0
write.csv(or.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.1.EQU.csv")
##ga 3
or.cat.3 <- merge(mo.or.cat.500003,yr.or.cat.500003,by="cat",all=TRUE)
or.cat.3 <- merge(or.cat.3,ft.or.cat.500003,by="cat",all=TRUE)
or.cat.3[is.na(or.cat.3)] <- 0
write.csv(or.cat.3,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.3.EQU.csv")
##ga 4
or.cat.4 <- merge(mo.or.cat.500004,yr.or.cat.500004,by="cat",all=TRUE)
or.cat.4 <- merge(or.cat.4,ft.or.cat.500004,by="cat",all=TRUE)
or.cat.4[is.na(or.cat.4)] <- 0
write.csv(or.cat.4,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.cat.4.EQU.csv")
##prop
or.prop <- merge(prop.mo.160000,prop.yr.160000,by="st.op",all=TRUE)
or.prop <- merge(or.prop,prop.ft.160000,by="st.op",all=TRUE)
or.prop[is.na(or.prop)] <- 0
write.csv(or.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/or.zprop.EQU.csv")

#ga by size
#ga 1
ga.sz.1 <- merge(mo.ga.sz.500001,yr.ga.sz.500001,by="size",all=TRUE)
ga.sz.1 <- merge(ga.sz.1,ft.ga.sz.500001,by="size",all=TRUE)
ga.sz.1[is.na(ga.sz.1)] <- 0
write.csv(ga.sz.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.sz.1.EQU.csv")
##ga 2
ga.sz.2 <- merge(mo.ga.sz.500002,yr.ga.sz.500002,by="size",all=TRUE)
ga.sz.2 <- merge(ga.sz.2,ft.ga.sz.500002,by="size",all=TRUE)
ga.sz.2[is.na(ga.sz.2)] <- 0
write.csv(ga.sz.2,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.sz.2.EQU.csv")
#ga by cat
##ga 1
ga.cat.1 <- merge(mo.ga.cat.500001,yr.ga.cat.500001,by="cat",all=TRUE)
ga.cat.1 <- merge(ga.cat.1,ft.ga.cat.500001,by="cat",all=TRUE)
ga.cat.1[is.na(ga.cat.1)] <- 0
write.csv(ga.cat.1,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.cat.1.EQU.csv")
##ga 2
ga.cat.2 <- merge(mo.ga.cat.500002,yr.ga.cat.500002,by="cat",all=TRUE)
ga.cat.2 <- merge(ga.cat.2,ft.ga.cat.500002,by="cat",all=TRUE)
ga.cat.2[is.na(ga.cat.2)] <- 0
write.csv(ga.cat.2,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.cat.2.EQU.csv")
##prop
ga.prop <- merge(prop.mo.550000,prop.yr.550000,by="st.op",all=TRUE)
ga.prop <- merge(ga.prop,prop.ft.550000,by="st.op",all=TRUE)
ga.prop[is.na(ga.prop)] <- 0
write.csv(ga.prop,file="C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Canning_Reports/csv_files/ga.zprop.EQU.csv")
