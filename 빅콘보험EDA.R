####################################
###보험+한화생명 EDA
####################################

#데이터 불러오기
getwd()
bigcon <- read.csv("Data_set.csv",stringsAsFactors = F) #factor로 취급 x

#보험관련변수확인
dim(bigcon)
View(head(bigcon))
str(bigcon)
colnames(bigcon)[17:52] #보험관련 column 36개


####################################
### 변수 확인
### No 변수영문명 변수명 변수설명 비고 
####################################

#16	OCCP_NAME_G	직업	산출일 기준 대분류 직업 정보	NULL, *(비식별처리)
unique(bigcon$OCCP_NAME_G) #비식별, null 제외 16개 직업군
sort(table(bigcon$OCCP_NAME_G),decreasing = T) #빈도수 주부, 사무직, 2차산업 종사자, 자영업, 3차산업종사자, 공무원,전문직 순


#17	CUST_JOB_INCM	추정소득	직업정보기반 추정 소득 금액	
table(bigcon$CUST_JOB_INCM) #소득 없는 사람 40338명, 약 40%

boxplot(bigcon$CUST_JOB_INCM) #전체 소득 boxplot
summary(bigcon$CUST_JOB_INCM) 
  #통계량 요약 median 3600, mean 2788, 3rd 4700 Max 10000

boxplot(bigcon$CUST_JOB_INCM[-which(bigcon$CUST_JOB_INCM == 0)]) #소득 0 추정인원 제외, boxplot
summary(bigcon$CUST_JOB_INCM[-which(bigcon$CUST_JOB_INCM == 0)]) 
  #소득 0 추정인원 제외, 통계량 요약 min 400, 1st 3800, median 4500, mean 4666, 3rd 5300, Max 10000
table(bigcon$OCCP_NAME_G[which(bigcon$CUST_JOB_INCM == 0)]) #소득 0 추정인원, 직업과 연관성 낮아보임

  ### 직업별 추정소득 boxplot
boxplot(bigcon$CUST_JOB_INCM~bigcon$OCCP_NAME_G) #주부, 학생, 기타 추정치 낮음
boxplot(bigcon$CUST_JOB_INCM[-which(bigcon$CUST_JOB_INCM == 0)]~bigcon$OCCP_NAME_G[-which(bigcon$CUST_JOB_INCM == 0)])


#18	HSHD_INFR_INCM	가구추정소득	가계 합산 추정 소득
boxplot(bigcon$HSHD_INFR_INCM)
table(bigcon$HSHD_INFR_INCM)


#19	ACTL_FMLY_NUM	실가족원수	산출일 기준 입력된 가족원 수	
table(bigcon$ACTL_FMLY_NUM)
cor(bigcon$ACTL_FMLY_NUM, bigcon$HSHD_INFR_INCM) #실가족원수 ~ 가구추정소득 , corr = 0.24


#20	CUST_FMLY_NUM	보험가입가족원수	산출일 기준 보험가입이력이 있는 가족원 수	
table(bigcon$CUST_FMLY_NUM) #가입이력존재 인원 수 min 1, Max 5 


#21	LAST_CHLD_AGE	막내자녀나이	산출일 기준 입력된 막내 자녀의 나이	0 = NULL 
table(bigcon$LAST_CHLD_AGE) #NULL+0 = 51152명, 아이가 없는 것으로 추정


#22	MATE_OCCP_NAME_G	배우자직업	산출일 기준 배우자의 대분류 직업 정보	NULL, *(비식별처리)
sort(table(bigcon$MATE_OCCP_NAME_G),decreasing = T) #배우자 없는 것으로 추정되는 NULL = 45709 , * = 1027
   #개인직업과 분류 동일


#23	MATE_JOB_INCM	배우자추정소득	배우자 직업 또는 주소 기반 추정 소득 금액	
table(bigcon$MATE_JOB_INCM) #수입 0 추정 = 65574
summary(bigcon$MATE_JOB_INCM[-which(bigcon$MATE_JOB_INCM==0)])
   #수입 0 추정 제외 통계량, min 2500, 1st 4300 median 4900 Mean 5002 3rd 5500 Max 10000
boxplot(bigcon$MATE_JOB_INCM[-which(bigcon$MATE_JOB_INCM==0)])


#24	CRDT_LOAN_CNT	신용대출건수	산출일 기준 한화생명에서 실행된 총 신용대출 건수	
table(bigcon$CRDT_LOAN_CNT) #0= 90366, 1=6256, 2= 2156 ~~~ 전체 약 10% 정도의 대출건수


#25	MIN_CNTT_DATE	최초대출날짜	한화생명에서 실행된 최초의 신용대출의 년월	
table(bigcon$MIN_CNTT_DATE) #min = 99년 9월 ~  max = 16년 4월


##26	TOT_CRLN_AMT	한화생명신용대출금액	산출일 기준 한화생명에서 실행된 총 신용대출 금액	
table(bigcon$TOT_CRLN_AMT) #대출금액 0 = 90402
summary(bigcon$TOT_CRLN_AMT[-which(bigcon$TOT_CRLN_AMT == 0)])
 #min 100만원 1st 500만, medain 800만, mean 1205만 , 3rd 1500만, Max 1억 1천만
boxplot(bigcon$TOT_CRLN_AMT[-which(bigcon$TOT_CRLN_AMT == 0)])


##27	TOT_REPY_AMT	한화생명신용상환금액	산출일 기준 한화생명에서 실행된 총 신용대출 금액 중 총 상환된 상환금액	
table(bigcon$TOT_REPY_AMT)
summary(bigcon$TOT_REPY_AMT[-which(bigcon$TOT_REPY_AMT == 0)])

  ###상환율을 확인해보자
a <- which(bigcon$TOT_CRLN_AMT == 0) #대출받지 않은 사라
money <- bigcon[-a,] #9831명

plot(money$TOT_CRLN_AMT, money$TOT_REPY_AMT) # x = 대출금액, y= 상환금액
abline(a=0,b=1) #100% 상환한 사람

table(money$TOT_REPY_AMT/money$TOT_CRLN_AMT) #상환율 100% = 6559명 , 0% 1406명 , 중간인원 1866명
plot(table(money$TOT_REPY_AMT/money$TOT_CRLN_AMT)) #상환율 100% 아닌 사람들
x = money$TOT_REPY_AMT/money$TOT_CRLN_AMT #상환율
summary(x[-which(x==1)]) #상환율 100% 제외 통계량 min 0.01, 1st 0.3, med 0.5, mean 0.48, 3rd 0.66, Max 0.99
table(x[-which(x==1)])


#28	CRLN_OVDU_RATE	신용대출연체율	한화생명에서 실행된 신용대출이후 경과월수 중 연체경험월수의 비율	
table(bigcon$CRLN_OVDU_RATE)
summary(bigcon$CRLN_OVDU_RATE)
summary(bigcon$CRLN_OVDU_RATE[-which(bigcon$CRLN_OVDU_RATE==0)]/100) #연체를 안한 인원 제외 통계량
   #min 0.01 , 1st 0.19, med 0.4, mean 0.43, 3rd 0.66, max 1


#29	CRLN_30OVDU_RATE	30일이내신용대출연체율	한화생명에서 실행된 30일이내 연체경험월수/ 30일이내 신용대출월수*100	
table(bigcon$CRLN_30OVDU_RATE)
summary(bigcon$CRLN_30OVDU_RATE[-which(bigcon$CRLN_30OVDU_RATE==0)]/100) #연체를 안한 인원 제외 통계량
   #min 0.01 , 1st 0.035, med 0.14, mean 0.225, 3rd 0.36, max 1


#30	LT1Y_CLOD_RATE	최근1년신용대출연체율	한화생명에서 실행된  최근1년 연체경험월수/ 최근1년 신용대출월수*100	
table(bigcon$LT1Y_CLOD_RATE)
summary(bigcon$LT1Y_CLOD_RATE[-which(bigcon$LT1Y_CLOD_RATE==0)]/100) #연체를 안한 인원 제외 통계량
   #min 0.1 , 1st 0.1, med 0.2, mean 0.3153, 3rd 0.4, max 1


#31	STRT_CRDT_GRAD	최초신용등급	한화생명에서 실행된 가장 오래된 대출시점의 신용등급	0(등급없음)
table(bigcon$STRT_CRDT_GRAD)


#32	LTST_CRDT_GRAD	최근신용등급	한화생명에서 실행된 가장 최근 대출시점의 신용등급	0(등급없음)
table(bigcon$LTST_CRDT_GRAD)

b <- which(bigcon$LTST_CRDT_GRAD == 0) #최근신용등급 없는 인원 제거
rank <- bigcon[-b,]
table(rank$STRT_CRDT_GRAD,rank$LTST_CRDT_GRAD) #처음 -> 나중 신용등급 변화
boxplot(rank$LTST_CRDT_GRAD~rank$STRT_CRDT_GRAD)
abline(a=0,b=1)

rank1 <- rank[-which(rank$STRT_CRDT_GRAD==0),] #새로 유입되는 인원의 직업별 신용등급 경향
boxplot(rank1$LTST_CRDT_GRAD~rank1$OCCP_NAME_G)
sort(table(rank1$OCCP_NAME_G),decreasing = T) #신규진입자 직업별 빈도수

#33	PREM_OVDU_RATE	보험료연체율	총납입보험료 횟수 중 연체한 보험료 횟수의 비율	
table(bigcon$PREM_OVDU_RATE/100) #연체 0회 우수고객 27233명


#34	LT1Y_PEOD_RATE	최근1년보험료연체율	최근1년 연체납입횟수/총납입횟수*100	
table(bigcon$LT1Y_PEOD_RATE) #연체율 0% 73274, 10%미만 12462명


#35	AVG_STLN_RATE	평균약대율	월별 약관대출가능 금액 중 약관대출 받은 금액의 비율의 연중 평균 	
table(bigcon$AVG_STLN_RATE/100)


#36	STLN_REMN_AMT	약관대출가능잔액	약관대출 받은 금액	
#37	LT1Y_STLN_AMT	최근1년약대금액	최근1년 약관대출 받은 금액	
#38	LT1Y_SLOD_RATE	최근1년약대연체율	최근1년 약관대출연체경험월수/ 최근1년 약관대출월수*100	
#39	GDINS_MON_PREM	非연금저축상품월납입보험료	유효한 계약 중 납입중인 보장성 상품의 월납환산보험료(일시납 제외)	
#40	SVINS_MON_PREM	연금저축상품월납입보험료	유효한 계약 중 납입중인 저축성 상품의 월납환산보험료(일시납 제외)	
#41	FMLY_GDINS_MNPREM	非가구연금저축상품월납입보험료	가계 합산 기준 유효한 계약 중 납입중인 보장성 상품의 월납환산보험료(일시납 제외)	
#42	FMLY_SVINS_MNPREM	가구非연금저축상품월납입보험료	가계 합산 기준 유효한 계약 중 납입중인 저축성 상품의 월납환산보험료(일시납 제외)	
#43	MAX_MON_PREM	최대월납입보험료	기준일 이전 납입한 월납입보험료 中 최대보험료	
#44	TOT_PREM	기납입보험료	유효한 계약의 총납입보험료	
#45	FMLY_TOT_PREM	가구기납입보험료	가계 합산 기준 유효한 계약의 총납입보험료	
#46	CNTT_LAMT_CNT	실효해지건수	계약해지 또는실효난 계약건수	
#47	LT1Y_CTLT_CNT	최근1년 실효해지건수	최근1년 계약해지 또는 실효난 계약건수	
#48	AUTR_FAIL_MCNT	자동이체실패월수	산출일 기준 총 자동이체실패월수	
#49	FYCM_PAID_AMT	가구총지급보험금액	가계 합산 보험금지급 총액	
#50	FMLY_CLAM_CNT	가구총보험금청구건수	가계 합산 총 보험금청구 건수	
#51	FMLY_PLPY_CNT	가구만기완납경험횟수	가구단위 만기까지 보험료를 완납한 증번의 갯수	



###변수간 상관계수
relation <- cor(bigcon[,c(18:21,24:34,36:52)]) 
  #일반적으로 예상가능한 cor 값들, 의외의 결과는 x 단, 0이 나온 수치를 제외할 경우 달라지지 않을까



## 보험변수 회귀결과
colnames(bigcon)[17:52]
variable <- paste(c(colnames(bigcon)[17:52]), collapse = "+")
fmla <- as.formula(paste("TARGET ~", variable)) 


#단순회귀
lm.out <- lm(fmla, data = bigcon)
summary(lm.out) #막내나이, 최근 1년 보험료 연체율, 최대월납입, 기납입, 가구기납입보험료, 실효해지, 자동이체실패, 가구만기완납


#로지스틱
lm.out2 <- glm(fmla, data=bigcon , family = "binomial")
summary(lm.out2)
pR2(lm.out2) #변동 3%정도 설명 ㅋㅋ



