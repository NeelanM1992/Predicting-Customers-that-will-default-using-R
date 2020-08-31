
#Model 1
glm(formula = default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
      AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + Zero_Values + 
      Positive_Values + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY_AMT5 + 
      PAY_AMT6 + rev + nopay3 + completepay4 + completepay5 + completepay6 + 
      PAY_2_five + PAY_4_five + PAY_5_five + PAY_RATIO_APR + TwoMonth_BillAmount_Ratio + 
      ThreeMonth_BillAmount_Ratio + FiveMonth_BillAmount_Ratio + 
      OneMonth_PaymentAmount_Ratio + FiveMonth_PaymentAmount_Ratio + 
      def, family = binomial(link = "logit"), data = training)

#Model 2
glm(formula = default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
      AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + Zero_Values + 
      Positive_Values + BILL_AMT2 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + 
      PAY_AMT2 + PAY_AMT4 + PAY_AMT6 + rev + nopay3 + completepay4 + 
      completepay5 + PAY_2_five + PAY_4_five + PAY_5_five + agegroup1 + 
      PAY_RATIO_APR + TwoMonth_BillAmount_Ratio + ThreeMonth_BillAmount_Ratio + 
      FiveMonth_BillAmount_Ratio + OneMonth_PaymentAmount_Ratio + 
      ThreeMonth_PaymentAmount_Ratio + FourMonth_PaymentAmount_Ratio + 
      FiveMonth_PaymentAmount_Ratio + def + Limit_Amount2 + Limit_Amount5 + 
      Limit_Amount6, family = binomial(link = "logit"), data = training)


#Add this to the code and comment out log of limit_bal

full$Limit_Amount1<-round((full$BILL_AMT1/full$LIMIT_BAL)*100,3)
full$Limit_Amount2<-round((full$BILL_AMT2/full$LIMIT_BAL)*100,3)
full$Limit_Amount3<-round((full$BILL_AMT3/full$LIMIT_BAL)*100,3)
full$Limit_Amount4<-round((full$BILL_AMT4/full$LIMIT_BAL)*100,3)
full$Limit_Amount5<-round((full$BILL_AMT5/full$LIMIT_BAL)*100,3)
full$Limit_Amount6<-round((full$BILL_AMT6/full$LIMIT_BAL)*100,3)

#full$LIMIT_BAL <- log(full$LIMIT_BAL)
