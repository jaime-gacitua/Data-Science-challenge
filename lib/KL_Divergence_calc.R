#### Calculation of KL Divergence features ####

df=read.csv(file="/Users/taliatseriotou/Desktop/CU-Fall2016/INFORMS/dataframe_kostas.csv", header=TRUE, fill=TRUE, row.names=NULL)

df["Norm_V_Yield"] <- NA
df["Norm_C_Yield"] <- NA
df["KL_DIV"] <- NA

#convert to probability distributions
norm_var = aggregate(df[,6]~ df[,2],df,FUN = sum)
norm_check = aggregate(df[,7]~ df[,2],df,FUN = sum)

?match

for (i in 1:nrow(df)){
  k1 = match(df[i,2],norm_var[,1])
  k2 = match(df[i,2],norm_check[,1])
  df[i,10] = df[i,6]/norm_var[k1,2]
  df[i,11] = df[i,7]/norm_check[k2,2]
  df[i,12] = df[i,10]*log(df[i,10]/df[i,11]) #calculate the number of KL Divergence
}

#sum over all for each variety
result = aggregate(df[,12]~ df[,2],df,FUN = sum)
bagsold = aggregate(df[,8]~ df[,2],df,FUN = mean)
tab = merge(result,bagsold)

lm.fit= lm(tab[-6,3]~tab[-6,2], data = tab)

summary(lm.fit)
plot(x=tab[-6,2] ,y=tab[-6,3])

