data1 <- read.csv(file.choose(), header=T)
dim(data1)

# to convert 1:NO 2:YES to 0:NO 1:YES
library(dplyr)
data <- data1 %>% mutate(across(3:15, ~ ifelse(. == 1, 0, 1)))

## Preprocessing Data

# check if there is unused variable
str(data)

# check if there is missing data
summary(data)

# check if there is duplicate data 
duplicate <- sum(duplicated(data))
cat("Total Duplicate Rows :" , duplicate, "\n")
data2 <- data[!duplicated(data),]
duplicate2 <- sum(duplicated(data2))
cat("Total Duplicate Rows :" , duplicate2, "\n")

library(ggplot2)

## AGE

data2A <- data2 %>%
  mutate(AGE_GROUP = cut(AGE, breaks = c(21, 40, 60, 80, Inf), 
                         labels = c("21-40", "41-60", "61-80", "80+"), 
                         right = FALSE))
datAge <- xtabs(~LUNG_CANCER + AGE_GROUP, data=data2A)
datAge
summary(data2A$AGE)

## GENDER 
0 = Male 1 = Female

datGen <- xtabs(~LUNG_CANCER + GENDER, data=data2)
print(datGen)

chisq1 <- chisq.test(datGen)
print(chisq1)

cont_GenM <- matrix(c(17 ,125 ), nrow=1)
p_genM <- chisq.test(cont_GenM)$p.value

cont_GenF <- matrix(c(21 ,113 ), nrow=1)
p_genF <- chisq.test(cont_GenF)$p.value

df <- data.frame(
      Gen = rep(c("Gen = M" ,"Gen = F"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(19, 125, 21, 113)
)

p_gen <- ggplot(df, aes(x = Gen, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 117, yend = 117), color = "black") +  
  geom_text(aes(x = 1, y = 125, label = sprintf("p = %.3e", p_genM)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 113, yend = 117), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 113, yend = 117), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 130, yend = 130), color = "black") + 
  geom_text(aes(x = 2, y = 138, label = sprintf("p = %.3e", p_genF)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 126, yend = 130), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 126, yend = 130), color = "black", size = 0.1) +

  scale_fill_manual(values = c("burlywood", "darkred")) +
  labs(title = "Bar Plot of LC and GEN with P-Values",
       x = "GENDER",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## SMOKING 

datSM <- xtabs(~LUNG_CANCER + SMOKING, data=data2)
dimnames(datSM) <- list("LC" = c("LC = 0", "LC = 1"),
                        "SM" = c("SM = 0", "SM = 1"))
print(datSM)

chisq2 <- chisq.test(datSM)
print(chisq2)

cont_SM0 <- matrix(c(19 ,107 ), nrow=1)
p_sm0 <- chisq.test(cont_SM0)$p.value

cont_SM1 <- matrix(c(19 ,131 ), nrow=1)
p_sm1 <- chisq.test(cont_SM1)$p.value

df <- data.frame(
      SM = rep(c("SM = 0", "SM = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(19, 107, 19, 131)
)

p_sm <- ggplot(df, aes(x = SM, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 110, yend = 110), color = "black") +  
  geom_text(aes(x = 1, y = 118, label = sprintf("p = %.3e", p_sm0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 106, yend = 110), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 106, yend = 110), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 135, yend = 135), color = "black") + 
  geom_text(aes(x = 2, y = 143, label = sprintf("p = %.3e", p_sm1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 131, yend = 135), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 131, yend = 135), color = "black", size = 0.1) +

  scale_fill_manual(values = c("burlywood", "darkred")) +
  labs(title = "Bar Plot of LC and SM with P-Values",
       x = "SMOKING",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## Yellow Fingers

datYF <- xtabs(~LUNG_CANCER + YELLOW_FINGERS, data=data2)
dimnames(datYF) <- list("LC" = c("LC = 0", "LC = 1"),
                        "YF" = c("YF = 0", "YF = 1"))
print(datYF)

chisq3 <- chisq.test(datYF)
print(chisq3)

cont_YF0 <- matrix(c(25 ,92 ), nrow=1)
p_yf0 <- chisq.test(cont_YF0)$p.value

cont_YF1 <- matrix(c(13 ,146 ), nrow=1)
p_yf1 <- chisq.test(cont_YF1)$p.value

df <- data.frame(
      YF = rep(c("YF = 0", "YF = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(25, 92, 13, 146)
)

p_yf <- ggplot(df, aes(x = YF, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 95, yend = 95), color = "black") +  
  geom_text(aes(x = 1, y = 103, label = sprintf("p = %.3e", p_yf0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 91, yend = 95), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 91, yend = 95), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 149, yend = 149), color = "black") + 
  geom_text(aes(x = 2, y = 157, label = sprintf("p = %.3e", p_yf1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 145, yend = 149), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 145, yend = 149), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and YF with P-Values",
       x = "YELLOW FINGERS",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## ANXIETY

datAnx <- xtabs(~LUNG_CANCER + ANXIETY, data=data2)
dimnames(datAnx) <- list("LC" = c("LC = 0", "LC = 1"),
                        "Anx" = c("Anx = 0", "Anx = 1"))
print(datAnx)

chisq4 <- chisq.test(datAnx)
print(chisq4)

cont_ANX0 <- matrix(c(26 ,113 ), nrow=1)
p_anx0 <- chisq.test(cont_ANX0)$p.value

cont_ANX1 <- matrix(c(12 ,125 ), nrow=1)
p_anx1 <- chisq.test(cont_ANX1)$p.value

df <- data.frame(
      ANX = rep(c("ANX = 0", "ANX = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(26, 113, 12, 125)
)

p_anx <- ggplot(df, aes(x = ANX, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 116, yend = 116), color = "black") +  
  geom_text(aes(x = 1, y = 124, label = sprintf("p = %.3e", p_anx0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 112, yend = 116), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 112, yend = 116), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 128, yend = 128), color = "black") + 
  geom_text(aes(x = 2, y = 136, label = sprintf("p = %.3e", p_anx1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 124, yend = 128), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 124, yend = 128), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and ANX with P-Values",
       x = "ANXIETY",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## PEER PRESURRE

datPP <- xtabs(~LUNG_CANCER + PEER_PRESSURE, data=data2)
dimnames(datPP) <- list("LC" = c("LC = 0", "LC = 1"),
                        "PP" = c("PP = 0", "PP = 1"))
print(datPP)

chisq5 <- chisq.test(datPP)
print(chisq5)

cont_PP0 <- matrix(c(28 ,108 ), nrow=1)
p_peer0 <- chisq.test(cont_PP0)$p.value

cont_PP1 <- matrix(c(10 ,130 ), nrow=1)
p_peer1 <- chisq.test(cont_PP1)$p.value

df <- data.frame(
      PP = rep(c("PP = 0", "PP = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(28, 108, 10, 130)
)

p_peer <- ggplot(df, aes(x = PP, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 111, yend = 111), color = "black") +  
  geom_text(aes(x = 1, y = 119, label = sprintf("p = %.3e", p_peer0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 107, yend = 111), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 107, yend = 111), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 133, yend = 133), color = "black") + 
  geom_text(aes(x = 2, y = 141, label = sprintf("p = %.3e", p_peer1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 129, yend = 132), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 129, yend = 133), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and PP with P-Values",
       x = "PEER PRESSURE",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## CHRONIC DISEASE

datCD <- xtabs(~LUNG_CANCER + CHRONIC.DISEASE, data=data2)
dimnames(datCD) <- list("LC" = c("LC = 0", "LC = 1"),
                        "CD" = c("CD = 0", "CD = 1"))
print(datCD)

chisq6 <- chisq.test(datCD)
print(chisq6)

cont_CD0 <- matrix(c(25 ,107 ), nrow=1)
p_cd0 <- chisq.test(cont_CD0)$p.value

cont_CD1 <- matrix(c(13 ,131 ), nrow=1)
p_cd1 <- chisq.test(cont_CD1)$p.value

df <- data.frame(
      CD = rep(c("CD = 0", "CD = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(25, 107, 13, 131)
)

p_cd <- ggplot(df, aes(x = CD, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 111, yend = 111), color = "black") +  
  geom_text(aes(x = 1, y = 119, label = sprintf("p = %.3e", p_cd0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 107, yend = 111), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 107, yend = 111), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 134, yend = 134), color = "black") + 
  geom_text(aes(x = 2, y = 142, label = sprintf("p = %.3e", p_cd1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 130, yend = 134), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 130, yend = 134), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and CD with P-Values",
       x = "CHRONIC DISEASE",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## FATIGUE

datFt <- xtabs(~LUNG_CANCER + FATIGUE, data=data2)
dimnames(datFt) <- list("LC" = c("LC = 0", "LC = 1"),
                        "Ft" = c("Ft = 0", "Ft = 1"))
print(datFt)

chisq7 <- chisq.test(datFt)
print(chisq7)

cont_Ft0 <- matrix(c(20 ,73 ), nrow=1)
p_ft0 <- chisq.test(cont_Ft0)$p.value

cont_Ft1 <- matrix(c(18 ,165 ), nrow=1)
p_ft1 <- chisq.test(cont_Ft1)$p.value

df <- data.frame(
      Ft = rep(c("Ft = 0", "Ft = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(20, 73, 18, 165)
)

p_ft <- ggplot(df, aes(x = Ft, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 76, yend = 76), color = "black") +  
  geom_text(aes(x = 1, y = 84.5, label = sprintf("p = %.3e", p_ft0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 72, yend = 76), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 72, yend = 76), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 168, yend = 168), color = "black") + 
  geom_text(aes(x = 2, y = 177, label = sprintf("p = %.3e", p_ft1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 164, yend = 168), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 164, yend = 168), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and FT with P-Values",
       x = "FATIGUE",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## ALLERGY 

datAll <- xtabs(~LUNG_CANCER + ALLERGY, data=data2)
dimnames(datAll) <- list("LC" = c("LC = 0", "LC = 1"),
                        "All" = c("All = 0", "All = 1"))
print(datAll)

chisq8 <- chisq.test(datAll)
print(chisq8)

cont_All0 <- matrix(c(33 ,92 ), nrow=1)
p_all0 <- chisq.test(cont_All0)$p.value

cont_All1 <- matrix(c(5 ,146 ), nrow=1)
p_all1 <- chisq.test(cont_All1)$p.value

df <- data.frame(
      All = rep(c("All = 0", "All = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(33, 92, 5, 146)
)

p_all <- ggplot(df, aes(x = All, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 95, yend = 95), color = "black") +  
  geom_text(aes(x = 1, y = 103, label = sprintf("p = %.3e", p_all0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 91, yend = 95), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 91, yend = 95), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 149, yend = 149), color = "black") + 
  geom_text(aes(x = 2, y = 157, label = sprintf("p = %.3e", p_all1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 145, yend = 149), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 145, yend = 149), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and All with P-Values",
       x = "ALLERGY",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## WHEEZING 

datWH <- xtabs(~LUNG_CANCER + WHEEZING, data=data2)
dimnames(datWH) <- list("LC" = c("LC = 0", "LC = 1"),
                        "WH" = c("WH = 0", "WH = 1"))
print(datWH)

chisq9 <- chisq.test(datWH)
print(chisq9)

cont_WH0 <- matrix(c(29 ,96 ), nrow=1)
p_wh0 <- chisq.test(cont_WH0)$p.value

cont_WH1 <- matrix(c(9 ,142 ), nrow=1)
p_wh1 <- chisq.test(cont_WH1)$p.value

df <- data.frame(
      WH = rep(c("WH = 0", "WH = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(29, 96, 9, 142)
)

p_wh <- ggplot(df, aes(x = WH, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 99, yend = 99), color = "black") +  
  geom_text(aes(x = 1, y = 107, label = sprintf("p = %.3e", p_wh0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 95, yend = 99), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 95, yend = 99), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 145, yend = 145), color = "black") + 
  geom_text(aes(x = 2, y = 153, label = sprintf("p = %.3e", p_wh1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 141, yend = 145), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 141, yend = 145), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and WH with P-Values",
       x = "WHEEZING",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## ALCOHOL CONSUMING 

datACS <- xtabs(~LUNG_CANCER + ALCOHOL.CONSUMING, data=data2)
dimnames(datACS) <- list("LC" = c("LC = 0", "LC = 1"),
                        "ACS" = c("ACS = 0", "ACS = 1"))
print(datACS)

chisq10 <- chisq.test(datACS)
print(chisq10)

cont_ACS0 <- matrix(c(31 ,93 ), nrow=1)
p_acs0 <- chisq.test(cont_ACS0)$p.value

cont_ACS1 <- matrix(c(7 ,145 ), nrow=1)
p_acs1 <- chisq.test(cont_ACS1)$p.value

df <- data.frame(
      ACS = rep(c("ACS = 0", "ACS = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(31, 93, 7, 145)
)

p_acs <- ggplot(df, aes(x = ACS, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 96, yend = 96), color = "black") +  
  geom_text(aes(x = 1, y = 104, label = sprintf("p = %.3e", p_acs0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 92, yend = 96), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 92, yend = 96), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 148, yend = 148), color = "black") + 
  geom_text(aes(x = 2, y = 156, label = sprintf("p = %.3e", p_acs1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 144, yend = 148), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 144, yend = 148), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and ACS with P-Values",
       x = "ALCOHOL CONSUMING",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## COUGHING 

datCGH <- xtabs(~LUNG_CANCER + COUGHING, data=data2)
dimnames(datCGH) <- list("LC" = c("LC = 0", "LC = 1"),
                        "CGH" = c("CGH = 0", "CGH = 1"))
print(datCGH)

chisq11 <- chisq.test(datCGH)
print(chisq11)

cont_CGH0 <- matrix(c(28 ,89 ), nrow=1)
p_cgh0 <- chisq.test(cont_CGH0)$p.value

cont_CGH1 <- matrix(c(10 ,149 ), nrow=1)
p_cgh1 <- chisq.test(cont_CGH1)$p.value

df <- data.frame(
      CGH = rep(c("CGH = 0", "CGH = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(28, 89, 10, 149)
)

p_cgh <- ggplot(df, aes(x = CGH, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 92, yend = 92), color = "black") +  
  geom_text(aes(x = 1, y = 100, label = sprintf("p = %.3e", p_cgh0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 88, yend = 92), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 88, yend = 92), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 152, yend = 152), color = "black") + 
  geom_text(aes(x = 2, y = 160, label = sprintf("p = %.3e", p_cgh1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 148, yend = 152), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 148, yend = 152), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and CGH with P-Values",
       x = "COUGHING",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## SHORTNESS OF BREATH 

datSOB <- xtabs(~LUNG_CANCER + SHORTNESS.OF.BREATH, data=data2)
dimnames(datSOB) <- list("LC" = c("LC = 0", "LC = 1"),
                        "SOB" = c("SOB = 0", "SOB = 1"))
print(datSOB)

chisq12 <- chisq.test(datSOB)
print(chisq12)

cont_SOB0 <- matrix(c(17 ,85 ), nrow=1)
p_sob0 <- chisq.test(cont_SOB0)$p.value

cont_SOB1 <- matrix(c(21 ,153 ), nrow=1)
p_sob1 <- chisq.test(cont_SOB1)$p.value

df <- data.frame(
      SOB = rep(c("SOB = 0", "SOB = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(17, 85, 21, 153)
)

p_sob <- ggplot(df, aes(x = SOB, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 88, yend = 88), color = "black") +  
  geom_text(aes(x = 1, y = 96, label = sprintf("p = %.3e", p_sob0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 84, yend = 88), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 84, yend = 88), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 156, yend = 156), color = "black") + 
  geom_text(aes(x = 2, y = 164, label = sprintf("p = %.3e", p_sob1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 152, yend = 156), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 152, yend = 156), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and SOB with P-Values",
       x = "SHORTNESS OF BREATH",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## SWALLOWING DIFFICULTY

datSWD <- xtabs(~LUNG_CANCER + SWALLOWING.DIFFICULTY, data=data2)
dimnames(datSWD) <- list("LC" = c("LC = 0", "LC = 1"),
                        "SWD" = c("SWD = 0", "SWD = 1"))
print(datSWD)

chisq13 <- chisq.test(datSWD)
print(chisq13)

cont_SWD0 <- matrix(c(33 ,114 ), nrow=1)
p_swd0 <- chisq.test(cont_SWD0)$p.value

cont_SWD1 <- matrix(c(5 ,124 ), nrow=1)
p_swd1 <- chisq.test(cont_SWD1)$p.value

df <- data.frame(
      SWD = rep(c("SWD = 0", "SWD = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(33, 114, 5, 124)
)

p_swd <- ggplot(df, aes(x = SWD, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 117, yend = 117), color = "black") +  
  geom_text(aes(x = 1, y = 120, label = sprintf("p = %.3e", p_swd0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 113, yend = 117), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 113, yend = 117), color = "black", size = 0.1) +  

  geom_segment(aes(x = 1.8, xend = 2.2, y = 127, yend = 127), color = "black") + 
  geom_text(aes(x = 2, y = 130, label = sprintf("p = %.3e", p_swd1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 123, yend = 127), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 123, yend = 127), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and SWD with P-Values",
       x = "SWALLOWING DIFFICULTY",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)

## CHEST PAIN 

datCHP <- xtabs(~LUNG_CANCER + CHEST.PAIN, data=data2)
dimnames(datCHP) <- list("LC" = c("LC = 0", "LC = 1"),
                        "CHP" = c("CHP = 0", "CHP = 1"))
print(datCHP)

chisq14 <- chisq.test(datCHP)
print(chisq14)

cont_CHP0 <- matrix(c(26 ,96 ), nrow=1)
p_chp0 <- chisq.test(cont_CHP0)$p.value

cont_CHP1 <- matrix(c(12 ,142 ), nrow=1)
p_chp1 <- chisq.test(cont_CHP1)$p.value

df <- data.frame(
      CHP = rep(c("CHP = 0", "CHP = 1"), each = 2),
      LC = rep(c("LC = 0", "LC = 1"), 2),
      Frequency = c(26, 96, 12, 142)
)

p_chp <- ggplot(df, aes(x = CHP, y = Frequency, fill = LC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  
  geom_segment(aes(x = 0.8, xend = 1.2, y = 99, yend = 99), color = "black") +  
  geom_text(aes(x = 1, y = 102, label = sprintf("p = %.3e", p_chp0)), size = 3) +  
  geom_segment(aes(x = 0.8, xend = 0.8, y = 95, yend = 99), color = "black", size = 0.1) + 
  geom_segment(aes(x = 1.2, xend = 1.2, y = 95, yend = 99), color = "black", size = 0.1) +

  geom_segment(aes(x = 1.8, xend = 2.2, y = 145, yend = 145), color = "black") + 
  geom_text(aes(x = 2, y = 148, label = sprintf("p = %.3e", p_chp1)), size = 3) +  
  geom_segment(aes(x = 1.8, xend = 1.8, y = 141, yend = 145), color = "black", size = 0.1) + 
  geom_segment(aes(x = 2.2, xend = 2.2, y = 141, yend = 145), color = "black", size = 0.1) +

  scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(title = "Bar Plot of LC and CHP with P-Values",
       x = "CHEST PAIN",
       y = "Frequency") +
  theme_minimal()
  theme(aspect.ratio = 1)


library(gridExtra)
theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold",hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 8),
    theme(aspect.ratio = 1)
  )

p_gen <- p_gen + theme_custom
p_sm <- p_sm + theme_custom
p_yf <- p_yf + theme_custom
p_anx <- p_anx + theme_custom
p_peer <- p_peer + theme_custom
p_cd <- p_cd + theme_custom
p_ft <- p_ft + theme_custom
p_all <- p_all + theme_custom
p_wh <- p_wh + theme_custom
p_acs <- p_acs + theme_custom
p_cgh <- p_cgh + theme_custom
p_sob <- p_sob + theme_custom
p_swd <- p_swd + theme_custom
p_chp <- p_chp + theme_custom

library(patchwork)
plots <- list(p_gen, p_sm, p_yf, p_anx, p_peer, p_cd, p_ft, p_all, p_wh, p_acs, p_cgh, p_sob,p_swd, p_chp)
combined_plot <- wrap_plots(plots, ncol = 2, nrow = 7)
ggsave("Desc_Data.pdf", combined_plot, width = 16, height = 24, device = "pdf")























