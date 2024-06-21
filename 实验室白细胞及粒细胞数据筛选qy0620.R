#Eligible lab date identification gy 0619
##Finding eligible date from lab_file_1
#Extract the quantity of white blood cells and neutrophils
#observe original data structure.#Prepared by Ying Qu on 0619 and validated by jifang zhou on 0620
#Set up the working directory
setwd("D:/常州一院第一批次淋巴瘤数据")
#set up chinese language environment
Sys.setlocale(category="LC_ALL",locale="chinese")

#Load needed package
library(readxl)
library(tidyr)
library(dplyr)
library("lubridate",exclude =c("date","intersect","setdiff"))
library(lubridate)
library(data.table)
library(tidyverse)

#Import raw data set from respective files
lab_file_1<-fread("检验结果1.csv",encoding="UTF-8")
#NobS=xxx Number of unique patients xxx covered years xxxx
#select required columns
lab_file_1<-lab_file_1[,c("cardno","ex_proj_name","no_rpt","rpt_unit","test_dt")]
#Extract by key words from raw lab result files
wbc_neu_record_1<-lab_file_1%>%
  filter(grepl("白细胞|中性粒细胞",lab_file_1$ex_proj_name))
#Nobs=xxx   Number of unique patients xxx covevered years xxx 

#Identify search keywords and unit,then filter out the target inspection results
#Exclude the abnormal date by rpt_unit and ex_proj_code
table_wbc_neu_name<-as.data.frame(table(wbc_neu_record_1$ex_proj_name))

#Exclude invalid lab test names by key words
wbc_neu_record_1<-wbc_neu_record_1%>%
  filter(!grepl("尿|便|白带|阴道|灌洗液|痰液|cD|黏附|镜检|HPLP|高倍|视野|精液|介素|粪|沉渣|定性|百分|比值|比例|比率|团异常|脑脊液|脂酶|抗体|蛋自|异常|沉|于|分叶核|杆状核|浓度|涂片|红细胞|指数|酶|管型|淋巴细胞|手工|脑脊液|浆膜|体液|血片!形态|抗原|悬浮|酯酶|胸腹水",ex_proj_name))
  wbc_neu_record_1<-wbc_neu_record_1 %>%
  filter(!grepl("%",ex_proj_name))
#Nobs=xxx

table_wbc_neu_name <- as.data.frame(table(wbc_neu_record_1$ex_proj_name))
#Extract valid results with meaningful unit names
table_wbc_neu_unit<-as.data.frame(table(wbc_neu_record_1$rpt_unit))
wbc_neu_record_1<-wbc_neu_record_1%>%
filter(!grepl("fl|HP|PH|/mm3|/uLlLeu/uLlulluLlcellce11luLlPLlhpul|fLlumo]/Llpglumol/Llmglu/Llng|秒|IU/L|S/colu]高倍|glcoIls|pg/m1|/HP|检验结果单位|个|Ku/Llslpmo1/LlmmHg mol %/LP|mIu/mL copies/m] mIU/LPF PEIU c.0.IlG/L min ms/cm Ru拷贝|Ppwldegl无|%|%|MMoL/L|ms|mPa.sld/sc|型|-|HFR|min",rpt_unit))
#Nobs=xxx

#Remove non-numericalvalues
wbc_neu_record_1$no_rpt<-as.numeric(wbc_neu_record_1$no_rpt)
wbc_neu_record_1<-wbc_neurecord_1%>%
filter(!is.na(no_rpt))
#Nobs=xxx NA pts=xxxx
#After inspection the records with 10^12 were all valid ones
#and 10^6 needs further conversion if the value above 1000
wbc_neu_record_temp6<-wbc_neu_record_1%>%
filter(grepl("6",rpt_unit))
wbc_neu_record_temp12<-wbc_neu_record_1%>%
filter(grepl("12",rpt_unit))
wbc_neu_record_1$no_rpt<-if_else(grepl("6",wbc_neu_record_1$rpt_unit)& wbc_neu_record_1$no_rpt>200,wbc_neu_record_1$no_rpt/1000,wbc_neu_record_1$no_rpt)
#Nobs=xxxx

#Remove artifact values above 200 or below or negative values
wbc_neu_record_1<-wbc_neu_record_1%>%
filter(no_rpt<=200 & no_rpt>=0)
#Nobs=xxxx

#Remove invalid test time
wbc_neu_record_1$test_dt<-as.Date(wbc_neu_record_1$test_dt)
wbc_neu_record_1<-wbc_neu_record_1%>%
filter(year(wbc_neu_record_1$test_dt)>=2018)
#Nobs=xxxxx

#Remove unnecessary columns
wbc_neu_record_<-wbc_neu_record_1[,c("cardno","ex_proj_name","no_rpt","test_dt")]
#Nobs=xxx
#Extract clean WBc & neutrophile filesand store locally
wbc_record_clean_1<-wbc_neu_record_1%>%
filter(grepl("白细胞",ex_proj_name))
#Nobs=xxxx
record_clean_1<- wbc_neu_record_1 %>%
  filter(grepl("中性粒细胞",ex_proj_name))
#Nobs=xxxxx

#save eligible wBc & neautrophils lists
write.csv(wbc_record_clean_1, file = "Lab_1_wbc_clean_Z]F620.csv",row.nameS=FALSE)
write.csv(neu_record_clean_1, file = "Lab_1_neu_clean_Z]F620.csv",row.nameS=FALSE)