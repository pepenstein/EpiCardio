# Annualised change of ratio by sex, age and cause

ratio <- function(age, sex, cause){
  data <- db %>% 
    filter(Cause == cause,
           Age == age,
           Sex == sex)
  
  acr <- data.frame()
  for(p in 2:11) {
    logaritm <- data.frame(ARC = (log(data[["Rate"]][1]/data[["Rate"]][p])/11)*100)
    acr <- rbind.data.frame(acr, logaritm)
  }
  
  acr$Lower <- acr$ARC - sd(acr$ARC)
  acr$Upper <- acr$ARC + sd(acr$ARC)
  
  acr <- acr[10,]
  return(acr)
}

variables <- data.frame(
  Age = c(rep(unique(db$Age),3)),
  Sex = c(rep("Both",6), rep("Female",6), rep("Male",6))
)

cause <- data.frame()

for(n in 1:9){
  c <- data.frame(Cause = rep(unique(db$Cause)[n],18))
  cause <- rbind.data.frame(cause,c)
}


final_tab1 <- data.frame(Age = rep(variables$Age,9),
                         Sex = rep(variables$Sex,9),
                         Cause = cause$Cause)


final_tab2 <- data.frame()

for(l in 1:162){
  ac <- ratio(age = final_tab1$Age[l],
                   sex = final_tab1$Sex[l],
                   cause = final_tab1$Cause[l])
  
  final_tab2 <- rbind.data.frame(final_tab2, ac)
}

final_tab <- cbind.data.frame(final_tab1, round(final_tab2, 2))


readr::write_csv(final_tab, "acr.csv")


# Annualised change of ratio by province

ratio_province <- function(p){
  data <- provincias %>% 
    filter(Province == p)
  
  acr <- data.frame()
  for(p in 2:11) {
    logaritm <- data.frame(ARC = (log(data[["Total"]][1]/data[["Total"]][p])/11)*100)
    acr <- rbind.data.frame(acr, logaritm)
  }
  
  acr$Lower <- acr$ARC - sd(acr$ARC)
  acr$Upper <- acr$ARC + sd(acr$ARC)
  
  acr <- acr[10,]
  return(acr)
}

provinces_tab1 <- data.frame(Provinces = unique(provincias$Province))
provinces_tab2 <- data.frame()

for(n in 1:16){
  ac_province <-ratio_province(p = provinces_tab1$Provinces[n])
  provinces_tab2 <- rbind.data.frame(provinces_tab2, ac_province)
}

provinces_final <- cbind.data.frame(provinces_tab1, round(provinces_tab2,2))

readr::write_csv(provinces_final, "provinces_final.csv")
