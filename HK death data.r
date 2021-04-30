library(data.table)
library(dplyr)
library(ggplot2)

setwd("C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Data/CS&D Known Deaths")

# Combine Death Data from 1976 to 2019----------------------------------------------------------------------------------------
## 76-78
## 79-94
## 95-00
## 01-05
## 06
## 07-09
## 10-19

## ICD code data
ICD8 <- fread("D:\\New Folder\\Desktop\\Life expectancy\\Data\\ICD8 (sorted).txt",header=TRUE,
              colClasses="character")
names(ICD8)[5] <- "cod"

ICD9 <- fread("D:\\New Folder\\Desktop\\Life expectancy\\Data\\ICD9 (sorted).txt",header=TRUE,
              colClasses="character")
names(ICD9)[5] <- "cod"

ICD10 <- fread("D:\\New Folder\\Desktop\\Life expectancy\\Data\\ICD10 (sorted).txt",header=TRUE,
               colClasses="character")
names(ICD10)[5] <- "cod"

# write.csv(cbind(unique(ICD9$level3),unique(ICD10$level3)),'C:\\Users\\tomli\\Desktop\\Life expectancy\\Data\\ICD9 to ICD10.csv')
# write.csv(cbind(unique(ICD8$level3),unique(ICD9$level3)),'C:\\Users\\tomli\\Desktop\\Life expectancy\\Data\\ICD8 to ICD9.csv')

ICD8_9_10 <- read.csv('D:\\New Folder\\Desktop\\Life expectancy\\Data\\ICD8_9_10.csv')

## TPU data
TPU <- read.csv('D:\\New Folder\\Desktop\\Life expectancy\\Data\\TPU\\Death_TPU.csv')
TPU$TPU <- paste0(TPU$TPU)

## 76-78
format_DeathData7678 <- function(yy){
	death_HK <- fread(paste0("death",yy,".DAT"),header=FALSE,sep="\t")
	death_HK <- death_HK %>% 
				data.table() %>%
				# Decode, see file "Kd_sale_201712.doc"
				mutate(Year=paste0('19',substring(V1,8,9)), # Year of Death
					   Month=substring(V1,6,7), # Month of Death
					   Sex=factor(substring(V1,5,5)), # Sex
					   age_def=substring(V1,4,4), # Age definition
					   Age=as.numeric(substring(V1,1,3)), # Age
					   cod=factor(substring(V1,10,13)), # Cause of Death
					   E_cod=NA,
					   PrevResidence=NA,
					   HK_sinceBirth=NA,
					   LengthHK=substring(V1,24,25),
					   Length_Not_HK = Age - as.numeric(LengthHK),
					   TPU = substring(V1,20,22))

	levels(death_HK$Sex) <- c("Male","Female",NA)

	death_HK <- data.table(death_HK)
	death_HK[age_def==0,Age:=NA]
	death_HK[age_def %in% c(2,3),Age:=0]			
	
	# Categorise cause of death (ICD8)
	death_HK <- merge(death_HK,ICD8[,.(level2,level3,cod,COD)],"cod",all.x=TRUE) %>%
		rename(ICD8_level3=level3)
		
	# TPU
	death_HK = merge(death_HK, TPU, 'TPU', all.x=TRUE)
					
	return(death_HK)
}
DeathData7678 <- do.call(rbind,lapply(paste0(7,6:8),format_DeathData7678))

## 10-19
format_DeathData1019 <- function(yy){
	death_HK <- fread(paste0("death",yy,".DAT"),header=FALSE,sep="\t")
	death_HK <- data.table(substring(death_HK$V1,1,33)) %>%
				# Decode, see file "Kd-sale[1]"
				mutate(Year=substring(V1,12,15), # Year of Death
					   Month=substring(V1,9,10), # Month of Death
					   Sex=factor(substring(V1,5,5)), # Sex
					   age_def=substring(V1,1,1), # Age definition
					   Age=as.numeric(substring(V1,2,4)), # Age
					   cod=factor(substring(V1,16,19)), # Cause of Death
					   E_cod=factor(substring(V1,20,23)), # External cause of Death
					   PrevResidence=substring(V1,32,33),
					   HK_sinceBirth=substring(V1,30,31)=="90",
					   LengthHK=substring(V1,30,31),
					   TPU = NA,
					   Area = NA) 

	levels(death_HK$Sex) <- c("Female","Male",NA)
	# Add '.' to cod
	label <- levels(death_HK$cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$cod) <- label	
	label <- levels(death_HK$E_cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$E_cod) <- label	
	
	death_HK <- data.table(death_HK)
	death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]	
	
	death_HK[age_def=="X",Age:=NA]
	death_HK[age_def %in% c("M","D"),Age:=0]			
	death_HK[LengthHK!='90',Length_Not_HK := Age - as.numeric(LengthHK)]
	
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
	# Categorise cause of death (ICD10)
	death_HK <- merge(death_HK,ICD10[,.(level2,level3,cod,COD)],"cod",all.x=TRUE)

	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD10), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)
					
	return(death_HK)
}
DeathData1019 <- do.call(rbind,lapply(paste0(1,0:9),format_DeathData1019))

## 07-09
format_DeathData0709 <- function(yy){
	death_HK <- fread(paste0("death",yy,".DAT"),header=FALSE,sep="\t")
	death_HK <- data.table(substring(death_HK$V1,1,33)) %>%
				# Decode, see file "Kd-sale[1]"
				mutate(Year=substring(V1,12,15), # Year of Death
					   Month=substring(V1,9,10), # Month of Death
					   Sex=factor(substring(V1,5,5)), # Sex
					   age_def=substring(V1,1,1), # Age definition
					   Age=as.numeric(substring(V1,2,4)), # Age
					   cod=factor(substring(V1,16,19)), # Cause of Death
					   E_cod=factor(substring(V1,20,23)), # External cause of Death
					   PrevResidence=substring(V1,31,32),
					   HK_sinceBirth=substring(V1,29,30)=="90",
					   LengthHK=substring(V1,29,30),
					   TPU = NA,
					   Area = NA) 

	levels(death_HK$Sex) <- c("Female","Male",NA)
	# Add '.' to cod
	label <- levels(death_HK$cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$cod) <- label	
	label <- levels(death_HK$E_cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$E_cod) <- label	
	
	death_HK <- data.table(death_HK)
	death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]

	
	death_HK[age_def=="X",Age:=NA]
	death_HK[age_def %in% c("M","D"),Age:=0]			
	death_HK[LengthHK!='90',Length_Not_HK := Age - as.numeric(LengthHK)]
	
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
	# Categorise cause of death (ICD10)
	death_HK <- merge(death_HK,ICD10[,.(level2,level3,cod,COD)],"cod",all.x=TRUE)

	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD10), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)
				
	return(death_HK)
}
DeathData0709 <- do.call(rbind,lapply(paste0(0,7:9),format_DeathData0709))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
## 06
format_DeathData06 <- function(yy){
	death_HK <- fread("DEATH06.csv")
	names(death_HK) <- gsub(" ","_",names(death_HK))
	death_HK <- death_HK  %>%
				mutate(V1=NA,
					   Year=substrRight(date_of_death,4),
					   Month=NA,
					   Sex=factor(sex,labels=c("Female","Male",NA)),
					   age_def=age_definition,
					   Age=age,
					   cod=factor(cause_of_death),
					   E_cod=factor(external_cause_of_death),
					   PrevResidence=country_of_previous_residence,
					   HK_sinceBirth=length_of_stay_in_HK=="90",
					   LengthHK=length_of_stay_in_HK,
					   TPU = NA,
					   Area = NA) %>%
				select(V1,Year,Month,Sex,age_def,Age,cod,E_cod,PrevResidence,HK_sinceBirth,LengthHK, TPU, Area)
	# Add '.' to cod
	label <- levels(death_HK$cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$cod) <- label	
	label <- levels(death_HK$E_cod)
	three_digits <- setdiff((1:length(label)),grep(' ',label))
	label[three_digits] <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label[three_digits])
	label <- gsub(' ','',label)
	levels(death_HK$E_cod) <- label	
	
	death_HK <- data.table(death_HK)			
	death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]
	
	death_HK[age_def=="X",Age:=NA]
	death_HK[age_def %in% c("M","D"),Age:=0]			
	death_HK[LengthHK!='90',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
	
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
	# Categorise cause of death (ICD10)
	death_HK <- merge(death_HK,ICD10[,.(level2,level3,cod,COD)],"cod",all.x=TRUE)

	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD10), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)
					
	return(death_HK)
}
DeathData06 <- do.call(rbind,lapply(paste0(06),format_DeathData06))

## 01-05
format_DeathData0105 <- function(yy){
	death_HK <- fread(paste0("DEATH",yy,".DAT"),header=FALSE,sep="\t")
	death_HK <- data.table(substring(death_HK$V1,1,32)) %>%
				# Decode, see file "Kd-sale[1]"
				mutate(Year=substring(V1,12,15), # Year of Death
					   Month=substring(V1,9,10), # Month of Death
					   Sex=factor(substring(V1,5,5)), # Sex
					   age_def=substring(V1,1,1), # Age definition
					   Age=as.numeric(substring(V1,2,4)), # Age
					   cod=factor(substring(V1,16,19)), # Cause of Death
					   E_cod=factor(substring(V1,20,23)), # External cause of Death
					   PrevResidence=substring(V1,31,32),
					   HK_sinceBirth=substring(V1,29,30)=="90",
					   LengthHK=substring(V1,29,30),
					   TPU = NA,
					   Area = NA) 
					   
	levels(death_HK$Sex) <- c("Female","Male",NA)
	# Add '.' to cod
	label <- levels(death_HK$cod)
	label <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label)
	label <- gsub(' ','',label)
	levels(death_HK$cod) <- label
	label <- levels(death_HK$E_cod)
	label <- gsub('([A-Z][0-9]{2})([0-9])','\\1.\\2',label)
	label <- gsub(' ','',label)
	levels(death_HK$E_cod) <- label
	death_HK <- data.table(death_HK)

	death_HK[!E_cod %in% c('EXXX','E00.0'), cod:=E_cod]
	
	death_HK[age_def=="X",Age:=NA]
	death_HK[age_def %in% c("M","D"),Age:=0]			
	death_HK[Age>99,Age:=99]
	death_HK[LengthHK!='90',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
	
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA # assign NA to XXX
	# Categorise cause of death (ICD10)
	death_HK <- merge(death_HK,ICD10[,.(level2,level3,cod,COD)],"cod",all.x=TRUE)
	
	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD10), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)
				
	return(death_HK)
}
DeathData0105 <- do.call(rbind,lapply(paste0(0,1:5),format_DeathData0105))

## 95-00
format_DeathData9500 <- function(yy){
	if(yy %in% c(99,"00")){
		death_HK <- fread(paste0("DEATH",yy,".DAT"),header=FALSE,sep="\t")
		death_HK <- substring(death_HK$V1,1,31)
		death_HK <- data.table(do.call(rbind,strsplit(death_HK," ")))
	} else {
		death_HK <- fread(paste0("DEATH",yy,".DAT"),header=FALSE)
	}
		# Decode, see file "Kd-sale[1]"
			# Year of Death
	death_HK <- death_HK %>% 
				mutate(Year=substring(V1,11,14),
					   Month=substring(V1,8,9),
					   Sex=factor(substring(V1,4,4)),
					   age_def=substring(V1,1,1),
					   Age=as.numeric(substring(V1,2,3)),
					   cod=factor(substring(V1,15,18)),
					   E_cod=factor(substring(V2,1,4)),
					   PrevResidence=substring(V2,12,13),
					   HK_sinceBirth=substring(V2,10,11)=="90",
					   LengthHK=substring(V2,10,11),
					   TPU = NA,
					   Area = NA)
	death_HK <- data.table(death_HK)
	death_HK[!E_cod %in% c('EXXX','E000'), cod:=E_cod]
	
	levels(death_HK$Sex) <- c("Female","Male",NA)
	
	death_HK[age_def=="X",Age:=NA]
	death_HK[age_def %in% c("M","D"),Age:=0]
	death_HK[LengthHK!='90',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
	
				# assign NA to XXX
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA
	
	# Categorise cause of death (ICD9)
	death_HK <- merge(death_HK,ICD9[,.(level2,level3,cod,COD)],"cod",all.x=TRUE) 
		
	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD9), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)
	
	return(death_HK)
}
DeathData9500 <- do.call(rbind,lapply(c(95:99,"00"),format_DeathData9500))

## 79-94
format_DeathData7994 <- function(yy){
	death_HK <- fread(paste0("DEATH",yy,".DAT"),header=FALSE,colClasses="character")
		# Decode, see file "Kd-sale[1]"
			# Year of Death
	death_HK[,Year:=paste0("19",substring(V1,8,9))]
			# Month of Death
	death_HK[,Month:=substring(V1,6,7)]
			# Sex
	death_HK[,Sex:=factor(substring(V1,5,5))]
	levels(death_HK$Sex) <- c("Male","Female",NA)
			# Age definition
	death_HK[,age_def:=substring(V1,4,4)]
			# Age
	death_HK[,Age:=as.numeric(substring(V1,1,3))]
	death_HK[age_def==0,Age:=NA]
	death_HK[age_def %in% c(2,3),Age:=0]
			# Cause of Death
	death_HK[,cod:=factor(V2)]
	death_HK[,E_cod:=paste0('E',substring(V3,1,3))]
	death_HK[!E_cod %in% c('EXXX','E000'), cod:=E_cod]
				# assign NA to XXX
	levels(death_HK$cod)[levels(death_HK$cod)=="XXX"] <- NA
	
	# Categorise cause of death (ICD9)
	death_HK <- merge(death_HK,ICD9[,.(level2,level3,cod,COD)],"cod",all.x=TRUE)
	
			# Country of previous residence
	death_HK[,PrevResidence:=substring(V3,12,13)]
	death_HK[Year<1984,PrevResidence:=NA]
			# Live in HK since born
	death_HK[,HK_sinceBirth:=substring(V3,10,11)=="99"]
	death_HK[,LengthHK:=substring(V3,10,11)]
	death_HK[Year<1984,HK_sinceBirth:=NA]
	death_HK[LengthHK!='99',Length_Not_HK := as.numeric(Age) - as.numeric(LengthHK)]
			# TPU
	death_HK[,TPU:=substring(V3,6,8)]
	death_HK = merge(death_HK, TPU, 'TPU', all.x=TRUE)
	
	# Convert to ICD 8
	death_HK <- death_HK %>%
		merge(ICD8_9_10 %>% 
				rename(level3=ICD9), 'level3', all.x=TRUE) %>%
		rename(ICD8_level3=ICD8)

	return(death_HK)
}
DeathData7994 <- do.call(rbind,lapply(79:94,format_DeathData7994))


DeathData7905 <- rbind(DeathData7994[,-c("V1","V2","V3",'ICD10','level3'),with=FALSE],DeathData9500[,-c("V1","V2",'ICD10','level3'),with=FALSE],DeathData0105[,-c('V1','ICD9','level3'),with=FALSE])
DeathData7905$Age <- factor(DeathData7905$Age)

DeathData7919 <- rbind(DeathData7905,DeathData06[,-c('V1','level3','ICD9'),with=FALSE],DeathData0709[,-c('V1','level3','ICD9'),with=FALSE],DeathData1019[,-c('V1','level3','ICD9'),with=FALSE])
DeathData7919$Age <- as.numeric(paste(DeathData7919$Age))
DeathData7919[Age>99,Age:=99]

DeathData7619 <- rbind(DeathData7678[,-c('V1'),with=FALSE],DeathData7919)
save(DeathData7619,"C:/Users/FreyaYu/Documents/Tasks/Life Expectancy Task/Data/CS&D Known Deaths/DeathData7619.rda")


