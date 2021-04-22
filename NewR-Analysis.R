lipid=read.table("Computer\E:\Franziska\Eigene Dokumente\Uni\Bachelorarbeit\Ergebnistabelle.txt",sep="\t",h=TRUE,dec=",")

lipid$Replicate <- as.factor(lipid$Replicate)
lipid$Wet_Weight_mg <- as.numeric(as.character(lipid$Wet_Weight_mg))
lipid$Protein_ug <- as.numeric(as.character(lipid$Protein_ug))
lipid$Lipid_ug <- as.numeric(as.character(lipid$Lipid_ug))

lipid$Wet_Weight_mg[lipid$Wet_Weight_mg <0] <- NA

lipid$LperP <- lipid$Lipid_ug/lipid$Protein_ug
lipid$Lperwt <-  lipid$Lipid_ug/lipid$Wet_Weight_mg
lipid$LperFly <-  lipid$Lipid_ug/lipid$num_flies

lipid$Lperwt[lipid$Lperwt >100] <- NA

# RUNS ALL ADULTS

lipid_adult <- subset (lipid, lipid$Age == "Adult")

boxplot(lipid_adult$Lipid_ug[lipid_adult$Population == 'B' & lipid_adult$Sex == 'F'],lipid_adult$Lipid_ug[lipid_adult$Population == 'C' & lipid_adult$Sex == 'F'],lipid_adult$Lipid_ug[lipid_adult$Population == 'H' & lipid_adult$Sex == 'F'],
        lipid_adult$Lipid_ug[lipid_adult$Population == 'B' & lipid_adult$Sex == 'M'],lipid_adult$Lipid_ug[lipid_adult$Population == 'C' & lipid_adult$Sex == 'M'],lipid_adult$Lipid_ug[lipid_adult$Population == 'H' & lipid_adult$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_adult$Protein_ug[lipid_adult$Population == 'B' & lipid_adult$Sex == 'F'],lipid_adult$Protein_ug[lipid_adult$Population == 'C' & lipid_adult$Sex == 'F'],lipid_adult$Protein_ug[lipid_adult$Population == 'H' & lipid_adult$Sex == 'F'],
        lipid_adult$Protein_ug[lipid_adult$Population == 'B' & lipid_adult$Sex == 'M'],lipid_adult$Protein_ug[lipid_adult$Population == 'C' & lipid_adult$Sex == 'M'],lipid_adult$Protein_ug[lipid_adult$Population == 'H' & lipid_adult$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug Protein', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_adult$LperP[lipid_adult$Population == 'B' & lipid_adult$Sex == 'F'],lipid_adult$LperP[lipid_adult$Population == 'C' & lipid_adult$Sex == 'F'],lipid_adult$LperP[lipid_adult$Population == 'H' & lipid_adult$Sex == 'F'],
        lipid_adult$LperP[lipid_adult$Population == 'B' & lipid_adult$Sex == 'M'],lipid_adult$LperP[lipid_adult$Population == 'C' & lipid_adult$Sex == 'M'],lipid_adult$LperP[lipid_adult$Population == 'H' & lipid_adult$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/Protein', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_adult$LperFly[lipid_adult$Population == 'B' & lipid_adult$Sex == 'F'],lipid_adult$LperFly[lipid_adult$Population == 'C' & lipid_adult$Sex == 'F'],lipid_adult$LperFly[lipid_adult$Population == 'H' & lipid_adult$Sex == 'F'],
        lipid_adult$LperFly[lipid_adult$Population == 'B' & lipid_adult$Sex == 'M'],lipid_adult$LperFly[lipid_adult$Population == 'C' & lipid_adult$Sex == 'M'],lipid_adult$LperFly[lipid_adult$Population == 'H' & lipid_adult$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/fly', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_adult$Lperwt[lipid_adult$Population == 'B' & lipid_adult$Sex == 'F'],lipid_adult$Lperwt[lipid_adult$Population == 'C' & lipid_adult$Sex == 'F'],lipid_adult$Lperwt[lipid_adult$Population == 'H' & lipid_adult$Sex == 'F'],
        lipid_adult$Lperwt[lipid_adult$Population == 'B' & lipid_adult$Sex == 'M'],lipid_adult$Lperwt[lipid_adult$Population == 'C' & lipid_adult$Sex == 'M'],lipid_adult$Lperwt[lipid_adult$Population == 'H' & lipid_adult$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/ug wet weight', at=c(1,2,3,5,6,7), outline = FALSE)

#separate datasets for males and females
# !!! ATTENTION: ALWAYS RE_RUN THE SUBSETS BEFORE DOING PLOTS, BECAUSE THE NAMES lipid_F and lipid_M ARE IN DOUBLE USE!!!

lipid_F <- subset(lipid,lipid$Sex == 'F')
lipid_M <- subset(lipid,lipid$Sex == 'M')

boxplot(lipid_F$LperP[lipid_F$Population == 'B'],
        lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$LperP[lipid_M$Population == 'B'],
        lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Protein', at= c(1:16, 19:34), outline = FALSE)
legend("topleft", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$LperFly[lipid_F$Population == 'B'],
        lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$LperFly[lipid_M$Population == 'B'],
        lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Fly', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$Lperwt[lipid_F$Population == 'B'],
        lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$Lperwt[lipid_M$Population == 'B'],
        lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Wet Weight', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$Lipid_ug[lipid_F$Population == 'B'],
        lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$Lipid_ug[lipid_M$Population == 'B'],
        lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid$Wet_Weight_mg[lipid$Population == 'B' & lipid$Sex == 'F'],lipid$Wet_Weight_mg[lipid$Population == 'C' & lipid$Sex == 'F'],lipid$Wet_Weight_mg[lipid$Population == 'H' & lipid$Sex == 'F'],
        lipid$Wet_Weight_mg[lipid$Population == 'B' & lipid$Sex == 'M'],lipid$Wet_Weight_mg[lipid$Population == 'C' & lipid$Sex == 'M'],lipid$Wet_Weight_mg[lipid$Population == 'H' & lipid$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='weight of 8 flies (mg)', at= c(1,2,3,5,6,7), outline=FALSE)


#run a GLM for females
library(lme4)
library(car)
lipF <- lm(formula = LperP ~ Population + Wet_Weight_mg , data = lipid_F)
summary(lipF)
Anova(lipF,type = 3)

lipM <- lm(formula = LperP ~ Population + Wet_Weight_mg , data = lipid_M)
summary(lipM)
Anova(lipM,type = 3)

#significant
lipF <- lm(formula = LperFly ~ Population +Wet_Weight_mg , data = lipid_F)
summary(lipF)
Anova(lipF,type = 3)

lipM <- lm(formula = LperFly ~ Population +Wet_Weight_mg , data = lipid_M)
summary(lipM)
Anova(lipM,type = 3)

#none significant 
lipF <- lm(formula = Lperwt ~ Population  , data = lipid_F)
summary(lipF)
Anova(lipF,type = 3)

lipM <- lm(formula = Lperwt ~ Population  , data = lipid_M)
summary(lipM)
Anova(lipM,type = 3)



# LARVAL PLOTS

lipid_L <- subset (lipid, lipid$Age == "Larvae")

# Inclusive Plots

boxplot(lipid_L$Protein_ug[lipid_L$Population == "B"], lipid_L$Protein_ug[lipid_L$Population == "C"], lipid_L$Protein_ug[lipid_L$Population == "H"], 
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="ug Protein")

boxplot(lipid_L$Protein_ug[lipid_L$Population == "B"],
        lipid_L$Protein_ug[lipid_L$Replicate == "11"],lipid_L$Protein_ug[lipid_L$Replicate == "14"],lipid_L$Protein_ug[lipid_L$Replicate == "19"],lipid_L$Protein_ug[lipid_L$Replicate == "20"],
        lipid_L$Protein_ug[lipid_L$Replicate == "1"], lipid_L$Protein_ug[lipid_L$Replicate == "2"],lipid_L$Protein_ug[lipid_L$Replicate == "3"],lipid_L$Protein_ug[lipid_L$Replicate == "4"],lipid_L$Protein_ug[lipid_L$Replicate == "5"],
        lipid_L$Protein_ug[lipid_L$Replicate == "6"],lipid_L$Protein_ug[lipid_L$Replicate == "7"],lipid_L$Protein_ug[lipid_L$Replicate == "8"],lipid_L$Protein_ug[lipid_L$Replicate == "9"],lipid_L$Protein_ug[lipid_L$Replicate == "10"],
        col = c(rep("green",1),rep("blue",4),rep("red",10)),names =c("B", "11","14","19","20", 1:10), ylab ="ug Protein")


boxplot(lipid_L$Lipid_ug[lipid_L$Population == "B"], lipid_L$Lipid_ug[lipid_L$Population == "C"], lipid_L$Lipid_ug[lipid_L$Population == "H"], 
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="ug Lipid")

boxplot(lipid_L$Lipid_ug[lipid_L$Population == "B"],
        lipid_L$Lipid_ug[lipid_L$Replicate == "1"], lipid_L$Lipid_ug[lipid_L$Replicate == "2"],lipid_L$Lipid_ug[lipid_L$Replicate == "3"],lipid_L$Lipid_ug[lipid_L$Replicate == "4"],lipid_L$Lipid_ug[lipid_L$Replicate == "5"],
        lipid_L$Lipid_ug[lipid_L$Replicate == "6"],lipid_L$Lipid_ug[lipid_L$Replicate == "7"],lipid_L$Lipid_ug[lipid_L$Replicate == "8"],lipid_L$Lipid_ug[lipid_L$Replicate == "9"],lipid_L$Lipid_ug[lipid_L$Replicate == "10"],
        lipid_L$Lipid_ug[lipid_L$Replicate == "11"],lipid_L$Lipid_ug[lipid_L$Replicate == "14"],lipid_L$Lipid_ug[lipid_L$Replicate == "19"],lipid_L$Lipid_ug[lipid_L$Replicate == "20"],
        col = c(rep("green",1),rep("red",10),rep("blue",4)),names =c("B", 1:10, "11","14","19","20"), ylab ="ug Lipid")

  
       #Not exported plot LperLarva, including the strange ones
boxplot(lipid_L$LperFly[lipid_L$Population == "B"], lipid_L$LperFly[lipid_L$Population == "H"], lipid_L$LperFly[lipid_L$Population == "C"],
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="Lipid/Larva")
legend("topleft", c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE


#exclude the weird ones

lipid_Lex <- subset (lipid_L, lipid_L$Lipid_ug < 100)

#Lipid alone
boxplot(lipid_Lex$Lipid_ug[lipid_Lex$Population == "B"], lipid_Lex$Lipid_ug[lipid_Lex$Population == "C"], lipid_Lex$Lipid_ug[lipid_Lex$Population == "H"],
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="ug Lipid", outline=FALSE)


boxplot(lipid_Lex$Lipid_ug[lipid_Lex$Population == "B"],
        lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "11"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "14"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "19"],
        lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "1"], lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "2"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "3"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "4"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "5"],
        lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "6"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "7"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "8"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "9"],lipid_Lex$Lipid_ug[lipid_Lex$Replicate == "10"],
        col = c(rep("green",1),rep("blue",3),rep("red",10)),names =c("B","11","14","19",1:10), ylab ="ug Lipid", at=c(1, 4:6, 9:18), outline=FALSE)


#other stuff
#Population Plots

boxplot(lipid_Lex$LperP[lipid_Lex$Population == "B"], lipid_Lex$LperP[lipid_Lex$Population == "C"], lipid_Lex$LperP[lipid_Lex$Population == "H"],
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="Lipid/Protein", outline=FALSE)


boxplot(lipid_Lex$LperFly[lipid_Lex$Population == "B"], lipid_Lex$LperFly[lipid_Lex$Population == "C"], lipid_Lex$LperFly[lipid_Lex$Population == "H"],
        col = c("green","blue","red"),names =c("Base","Cold","Hot"),ylab ="Lipid/Larva", outline=FALSE)



#Replicate Plots

boxplot(lipid_Lex$LperP[lipid_Lex$Population == "B"],
       lipid_Lex$LperP[lipid_Lex$Replicate == "11"],lipid_Lex$LperP[lipid_Lex$Replicate == "14"],lipid_Lex$LperP[lipid_Lex$Replicate == "19"],
       lipid_Lex$LperP[lipid_Lex$Replicate == "1"], lipid_Lex$LperP[lipid_Lex$Replicate == "2"],lipid_Lex$LperP[lipid_Lex$Replicate == "3"],lipid_Lex$LperP[lipid_Lex$Replicate == "4"],lipid_Lex$LperP[lipid_Lex$Replicate == "5"],
       lipid_Lex$LperP[lipid_Lex$Replicate == "6"],lipid_Lex$LperP[lipid_Lex$Replicate == "7"],lipid_Lex$LperP[lipid_Lex$Replicate == "8"],lipid_Lex$LperP[lipid_Lex$Replicate == "9"],lipid_Lex$LperP[lipid_Lex$Replicate == "10"],
       col = c(rep("green",1),rep("blue",3),rep("red",10)), ylab ="Lipid/Protein",names =c("B","11","14","19",1:10), at= c(1, 4:6, 9:18), outline=FALSE)



boxplot(lipid_Lex$LperFly[lipid_Lex$Population == "B"],
        lipid_Lex$LperFly[lipid_Lex$Replicate == "11"],lipid_Lex$LperFly[lipid_Lex$Replicate == "14"],lipid_Lex$LperFly[lipid_Lex$Replicate == "19"],
        lipid_Lex$LperFly[lipid_Lex$Replicate == "1"], lipid_Lex$LperFly[lipid_Lex$Replicate == "2"],lipid_Lex$LperFly[lipid_Lex$Replicate == "3"],lipid_Lex$LperFly[lipid_Lex$Replicate == "4"],lipid_Lex$LperFly[lipid_Lex$Replicate == "5"],
        lipid_Lex$LperFly[lipid_Lex$Replicate == "6"],lipid_Lex$LperFly[lipid_Lex$Replicate == "7"],lipid_Lex$LperFly[lipid_Lex$Replicate == "8"],lipid_Lex$LperFly[lipid_Lex$Replicate == "9"],lipid_Lex$LperFly[lipid_Lex$Replicate == "10"],
        col = c(rep("green",1),rep("blue",3),rep("red",10)), names =c("B","11","14","19",1:10),ylab ="Lipid/Larva", at= c(1, 4:6, 9:18), outline=FALSE)


#ADULT PLOTS NEW ONLY (20.04.2017)

lipid_20 <- subset (lipid, lipid$Date == "20.04.2017")

boxplot(lipid_20$Lipid_ug[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$Lipid_ug[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$Lipid_ug[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$Lipid_ug[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$Lipid_ug[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$Lipid_ug[lipid_20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_20$Protein_ug[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$Protein_ug[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$Protein_ug[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$Protein_ug[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$Protein_ug[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$Protein_ug[lipid_20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug Protein', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_20$LperP[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$LperP[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$LperP[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$LperP[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$LperP[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$LperP[lipid20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/Protein', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_20$LperFly[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$LperFly[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$LperFly[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$LperFly[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$LperFly[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$LperFly[lipid_20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/fly', at=c(1,2,3,5,6,7), outline = FALSE)

boxplot(lipid_20$Lperwt[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$Lperwt[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$Lperwt[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$Lperwt[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$Lperwt[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$Lperwt[lipid_20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='ug TAG/ug wet weight', at=c(1,2,3,5,6,7), outline = FALSE)

#separate datasets for males and females 
# !!!! ATTENTION: OVERWRITES PREVIOUS SUBSETS OF lipid_F AND lipid_M !!!!

lipid_F <- subset(lipid_20,lipid_20$Sex == 'F')
lipid_M <- subset(lipid_20,lipid_20$Sex == 'M')

boxplot(lipid_F$LperP[lipid_F$Population == 'B'],
        lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$LperP[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$LperP[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$LperP[lipid_M$Population == 'B'],
        lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$LperP[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$LperP[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Protein', at= c(1:16, 19:34), outline = FALSE)
legend("topleft", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$LperFly[lipid_F$Population == 'B'],
        lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$LperFly[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$LperFly[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$LperFly[lipid_M$Population == 'B'],
        lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$LperFly[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$LperFly[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Fly', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$Lperwt[lipid_F$Population == 'B'],
        lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$Lperwt[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$Lperwt[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$Lperwt[lipid_M$Population == 'B'],
        lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$Lperwt[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$Lperwt[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG/Wet Weight', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_F$Lipid_ug[lipid_F$Population == 'B'],
        lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '11'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '14'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '17'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '19'],lipid_F$Lipid_ug[lipid_F$Population == 'C' & lipid_F$Replicate == '20'],
        lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '1'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '2'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '3'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '4'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '5'],
        lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '6'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '7'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '8'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '9'],lipid_F$Lipid_ug[lipid_F$Population == 'H' & lipid_F$Replicate == '10'],
        lipid_M$Lipid_ug[lipid_M$Population == 'B'],
        lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '11'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '14'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '17'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '19'],lipid_M$Lipid_ug[lipid_M$Population == 'C' & lipid_M$Replicate == '20'],
        lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '1'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '2'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '3'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '4'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '5'],
        lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '6'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '7'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '8'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '9'],lipid_M$Lipid_ug[lipid_M$Population == 'H' & lipid_M$Replicate == '10'],
        col = (c(rep('green',1),rep('blue',5),rep('red',10),rep('green',1),rep('blue',5),rep('red',10))),ylab ='ug TAG', at= c(1:16, 19:34), outline = FALSE)
legend("topright", inset=.00001, c('Base','Cold','Hot'), fill=c('green','blue','red'), horiz=TRUE, bty='n')# horiz=TRUE
axis(side = 1,at = c(1:16, 19:34),labels = c(c("B"),c(11,14,17,19,20),c(1:10),c("B"),c(11,14,17,19,20),c(1:10)),tck=-.02,font = 5,las = 1 )

boxplot(lipid_20$Wet_Weight_mg[lipid_20$Population == 'B' & lipid_20$Sex == 'F'],lipid_20$Wet_Weight_mg[lipid_20$Population == 'C' & lipid_20$Sex == 'F'],lipid_20$Wet_Weight_mg[lipid_20$Population == 'H' & lipid_20$Sex == 'F'],
        lipid_20$Wet_Weight_mg[lipid_20$Population == 'B' & lipid_20$Sex == 'M'],lipid_20$Wet_Weight_mg[lipid_20$Population == 'C' & lipid_20$Sex == 'M'],lipid_20$Wet_Weight_mg[lipid_20$Population == 'H' & lipid_20$Sex == 'M'],
        col = c('green','blue','red','green','blue','red'),names =c('Base','Cold','Hot','Base','Cold','Hot'),ylab ='weight of 8 flies (mg)', at= c(1,2,3,5,6,7), outline=FALSE)



