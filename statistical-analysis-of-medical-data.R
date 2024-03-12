options(encoding = "UTF-8")

list.of.packages <- c("car", "dplyr", "Hmisc", "dunn.test", "FSA", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("Hmisc")
library("dplyr")
library("car")
library("dunn.test")
library("FSA")
library("ggpubr")

#wczytanie pliku
pdf("ProjektBKwykresy.pdf")
write(paste("=== RAPORT Z WPROWADZONYCH DANYCH === \n\n"), file = "ProjektBartoszKasprzak.txt")

args <- commandArgs(TRUE)
file_path <- args[1]
file_path
data <- read.csv2(file_path, sep = ";")

elementy <- colnames(data)

#usuwanie NA
braki <- which(is.na(data))
braki

suma <- length(braki)
suma

brakujace <- which(is.na(data), arr.ind = TRUE)
brakujace

if(suma != 0){
  for(x in 1:suma){
    kolumna <- brakujace[x, "col"]
    mediana <- median(data[, kolumna], na.rm = TRUE)
    data[brakujace[x, "row"], kolumna] <- mediana
  }
  
}


which(is.na(data))

#sprawdzenie wartosci odstających + boxploty

datapodzial <- split(data, data[1])

grupy <- names(datapodzial)

staty <- list()

write("=== WARTOŚCI ODSTAJĄCE ===\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

par(mfrow = c(1, length(grupy)))
for(i in 1:length(elementy)){

  for(j in 1:length(grupy)){
    if(is.numeric(datapodzial[[j]][[i]] )){
      stats <- boxplot.stats(datapodzial[[j]][[i]])
      boxplot(datapodzial[[j]][[i]], main = paste(names(datapodzial)[[j]], names(datapodzial[[j]])[[i]]), outcol = "red", outcex = 1.5)
      staty[[paste0(grupy[j], " ", elementy[i])]] <- stats
    }
  }
}


for(i in 1:length(staty)){
  write(paste("=== ", names(staty)[[i]], " ==="), file = "ProjektBartoszKasprzak.txt", append = TRUE)
  if(length(staty[[i]][["out"]]) == 0){
    write("Wartości odstające: brak", file = "ProjektBartoszKasprzak.txt", append = TRUE)
  }else{
    write(paste("Wartości odstające: ", paste(staty[[i]]$out, collapse=", ")), file = "ProjektBartoszKasprzak.txt", append = TRUE)
  }
 
}

write("\n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

#charakterystyka badanych grup 

charakterystyka <- list()

for(i in 1:length(datapodzial)){
  ilosc <- length(datapodzial[[i]])
  for(j in 2:ilosc){
    if(is.numeric(datapodzial[[i]][[j]])){
      charakterystyka[[paste0(grupy[i], " ", elementy[j])]] <- c(summary(datapodzial[[i]][[j]]), sd(datapodzial[[i]][[j]]), var(datapodzial[[i]][[j]]))
      names(charakterystyka[[paste0(grupy[i], " ", elementy[j])]])[[7]] <- "standard_deviation"
      names(charakterystyka[[paste0(grupy[i], " ", elementy[j])]])[[8]] <- "variance"
    }else if(is.character(datapodzial[[i]][[j]])){
      charakterystyka[[paste0(grupy[i], " ", elementy[j])]] <- table(datapodzial[[i]][[j]])
    }
  }
}

write("=== CHARAKTERYSTYKA DANYCH ===\n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
write("== LEGENDA ==\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
write(" 'k' - liczba kobiet \n 'm' - liczba mężczyzn \n 'Min.' - minimalna wartość w badaniu \n '1st Qu.' - pierwszy kwartyl \n 'Median' - mediana \n 'Mean' - średnia arytmetyczna \n '3rd Qu.' - trzeci kwartyl \n 'Max.' - maksymalna wartość \n 'standard_deviation' - odchylenie standardowe \n 'variance' - wariancja \n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

for(i in 1:length(charakterystyka)){
  write(paste("\t===", names(charakterystyka)[[i]], "===\n"), file = "ProjektBartoszKasprzak.txt", sep = " ", append = TRUE)
  for(j in 1:length(charakterystyka[[i]])){
    
    write(paste(names(charakterystyka[[i]])[[j]], " = ", charakterystyka[[i]][[j]]), file = "ProjektBartoszKasprzak.txt", append = TRUE, sep = " ") 
  }
 write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
}
write("\n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

#sprawdzenie czy jest rozkład normalny

par(mfrow = c(1, 1))

liczbapvalue <- 0
for(i in 1:length(datapodzial)){
  for(j in 1:length(datapodzial[[i]])){
    if(is.numeric(datapodzial[[i]][[j]])){
      liczbapvalue <- liczbapvalue+1
    }
  }
  if(i!=length(datapodzial)){
    liczbapvalue <- 0
  }
}
liczbapvalue

p_value_rozklad <- data.frame(matrix(NA, nrow = length(grupy), ncol = liczbapvalue))

num_cols <- which(sapply(data, is.numeric)) 
beznazw <- unname(num_cols)

for(i in 1:liczbapvalue){
    colnames(p_value_rozklad)[i] <- elementy[beznazw[i]]
}

for(i in 1:length(grupy)){
  rownames(p_value_rozklad)[i]<-grupy[i]
}


tmp <- 1
for(i in 1:length(datapodzial)){
  ilosc <- length(datapodzial[[i]])
  for(j in 1:ilosc){
    if(is.numeric(datapodzial[[i]][[j]])){
      ShapiroTest<-shapiro.test(datapodzial[[i]][[j]])
      p_value_rozklad[i, tmp] <- ShapiroTest$p.value
      tmp <- tmp + 1
      plot(density(datapodzial[[i]][[j]]), main = paste(names(datapodzial)[[i]], names(datapodzial[[i]])[[j]]), xlab = "Wartości", ylab = "Gęstość")
      curve(dnorm(x, mean(datapodzial[[i]][[j]]), sd(datapodzial[[i]][[j]])), 
            add = TRUE, col = "red", lty = 2)    
      }
  }
  tmp <- 1
}

sprawdzrozklad <- data.frame(matrix(NA, nrow = 1, ncol = liczbapvalue))

tmp <- 0
for(i in 1:liczbapvalue)
{
  for(j in 1:length(grupy)){
    if(p_value_rozklad[j, i] > 0.05){
      if(tmp != 0 && j != 1){
        sprawdzrozklad[i] <- 1
        tmp <- 1
      }
      else if(tmp == 0 && j ==1){
        sprawdzrozklad[i] <- 1
        tmp <- 1
      }
    } else {
      sprawdzrozklad[i] <- 0
      tmp <- 0
    }
    
  }
}
for(i in 1:liczbapvalue){
  colnames(sprawdzrozklad)[i] <- elementy[beznazw[i]]
}

write("=== DANE POSIADAJĄCE ROZKŁAD NORMALNY === \n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

for(i in 1:liczbapvalue){
  if(sprawdzrozklad[i]==1){
    write(paste("Badanie ", colnames(sprawdzrozklad)[i], " posiada rozkład normalny"), file = "ProjektBartoszKasprzak.txt", append = TRUE, sep = "")
  }
  
}
write("\n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)




#sprawdzenie jednorodnosci wariancji
#pvalue musi byc większe od 0.05 i musi byc rozklad normalny

p_value_wariancja <- data.frame(matrix(NA, nrow = 1, ncol = liczbapvalue))


for(i in 1:liczbapvalue){
  colnames(p_value_wariancja)[i] <- elementy[beznazw[i]]
}

rownames(p_value_wariancja)[1] <- "pv"

tmp <- 1

for(i in 1:length(elementy)){
  if(is.numeric(data[,i])){
    wynikLevene <- leveneTest(data[,i] ~ grupa, data = data)
    p_value_wariancja[1, tmp] <- wynikLevene$"Pr(>F)"[1]
    tmp <- tmp + 1
  }
}


sprawdzwariancja <- data.frame(matrix(NA, nrow = 1, ncol = liczbapvalue))

for(i in 1:liczbapvalue)
{
  if(p_value_wariancja[1, i] > 0.05){
    sprawdzwariancja[i] <- 1
  } else {
    sprawdzwariancja[i] <- 0
  }
}
for(i in 1:liczbapvalue){
  colnames(sprawdzwariancja)[i] <- elementy[beznazw[i]]
}

write("=== DANE POSIADAJĄCE WARIANCJĘ HOMOGENICZNĄ === \n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

for(i in 1:liczbapvalue){
  if(sprawdzwariancja[i]==1){
    write(paste("Badanie ", colnames(sprawdzrozklad)[i], " posiada wariancję homogeniczną"), file = "ProjektBartoszKasprzak.txt", append = TRUE, sep = "")
  }
}
write("\n\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)


#przeprowadzanie testow grupowych 

write("=== PORÓWNANIE BADAŃ POMIĘDZY GRUPAMI ===\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)

for(i in 1:liczbapvalue){
  if(length(grupy)==2){
    
    if(sprawdzwariancja[i] == 1 && sprawdzrozklad[i] == 1){ #2TakTak
      
      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      wynikstudent <- t.test(data[,beznazw[i]] ~ grupa, data = data, var.equal = FALSE)
      pvstudent <- wynikstudent$p.value
      if(pvstudent < 0.05){
        write(paste("Wynik testu t-Studenta (dane o homogenicznej wariancji): ", pvstudent, "< 0.05 - istnieją istotne statystycznie różnice pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }else{
        write(paste("Wynik testu t-Studenta (dane o homogenicznej wariancji): ", pvstudent, "> 0.05 - brak istotnie statystyczbych różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
    }else if(sprawdzwariancja[i] == 0 && sprawdzrozklad[i]==1){ #2TakNie
      
      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      wynikstudentnie <- t.test(data[,beznazw[i]] ~ grupa, data = data, var.equal = FALSE)
      pvstudentnie <- wynikstudentnie$p.value
      if(pvstudentnie < 0.05){
        write(paste("Wynik testu t-Studenta (dane o niehomogenicznej wariancji): ", pvstudentnie, "< 0.05 - istnieją istotne statystycznie różnice pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }else{
        write(paste("Wynik testu t-Studenta (dane o niehomogenicznej wariancji): ", pvstudentnie, "> 0.05 - brak istotnie statystyczbych różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
    }else if(sprawdzrozklad[i]==0){ #2Nie
      
      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      wynikWilcoxon <- wilcox.test(data[,beznazw[i]] ~ grupa, data = data)
      pvWilcox <- wynikWilcoxon$p.value
      if(pvWilcox < 0.05){
        write(paste("Wynik testu WIlcoxona: ", pvWilcox, "< 0.05 - istnieją istotne statystycznie różnice pomiędzy grupami. ", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }else{
        write(paste("Wynik testu WIlcoxona: ", pvWilcox, "> 0.05 - brak istotnych statystycznie różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
    }
    
  }else if(length(grupy)>2){
    
    if(sprawdzwariancja[1, i] == 1 && sprawdzrozklad[1, i] == 1){ #>2TakTak
      
      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      pvANOVA <- summary(aov(data[,beznazw[i]] ~ grupa, data = data))[[1]][["Pr(>F)"]][[1]]
      
      if(pvANOVA < 0.05){
        rozniceaov <- TukeyHSD(aov(data[,beznazw[i]] ~ grupa, data = data))
        for(j in 1:length(rozniceaov$grupa[,"p adj"]))
        {
          if(rozniceaov$grupa[,"p adj"][j] < 0.05)
          {
            write(paste("Wynik testu ANOVA: ", pvANOVA, "< 0.05 - istnieją istotne statystycznie różnice pomiedzy grupami:", rownames(rozniceaov$grupa)[j], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          }
        }
      }else{
        write(paste("Wynik testu ANOVA: ", pvANOVA, "> 0.05 - brak istotnych statystycznie różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
      
    }else if(sprawdzwariancja[1, i] == 0 && sprawdzrozklad[1, i] == 1){ #>2TakNie 

      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      wynik <- kruskal.test(data[,beznazw[i]] ~ grupa, data = data)
      pvKruskal <- wynik$p.value
      if(pvKruskal < 0.05){
        roznicekw <- dunnTest(data[,beznazw[i]], data$grupa)
        for(j in 1:length(roznicekw[["res"]][["P.adj"]]))
        {
          if(roznicekw[["res"]][["P.adj"]][j] < 0.05){
            write(paste("Wynik testu Kruskala - Wallisa: ", pvKruskal, "< 0.05 - istnieją znaczące statystycznie różnice pomiędzy grupami: ", roznicekw$res$Comparison[j], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
            
          }
        }
        
      }else if(pvKruskal > 0.05){
        write(paste("Wynik testu Kruskala - Wallisa: ", pvKruskal, "> 0.05 - brak istotnych statystycznie różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
      
    }else if(sprawdzrozklad[1, i] == 0){ #>2Nie
     

      write(paste("=== TESTOWANE DANE: ", elementy[beznazw[i]], " ===\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      wynik <- kruskal.test(data[,beznazw[i]] ~ grupa, data = data)
      pvKruskal <- wynik$p.value
      if(pvKruskal < 0.05){
        roznicekw <- dunnTest(data[,beznazw[i]], data$grupa)
        for(j in 1:length(roznicekw[["res"]][["P.adj"]]))
        {
          if(roznicekw[["res"]][["P.adj"]][j] < 0.05){
            write(paste("Wynik testu Kruskala - Wallisa: ", pvKruskal, "< 0.05 - istnieją znaczące statystycznie różnice pomiędzy grupami: ", roznicekw$res$Comparison[j], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
            
          }
        }
        
      }else if(pvKruskal > 0.05){
        write(paste("Wynik testu Kruskala - Wallisa: ", pvKruskal, "> 0.05 - brak istotnych statystycznie różnic pomiędzy grupami.", "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
      }
      write("\n", file = "ProjektBartoszKasprzak.txt", append = TRUE)
      
    }
    
  }
}

#Analiza Korelacji 

write("=== KORELACJE POMIEDZY BADANIAMI W DANEJ GRUPIE ===", file = "ProjektBartoszKasprzak.txt", append = TRUE)

for(i in 1:length(grupy)){
  grupa1 <- data %>% filter (grupa==grupy[i])
  for(j in 1:(length(beznazw)-1)){
    for(k in (j+1):length(beznazw)){
      wynikkorelacja <- cor.test(grupa1[,beznazw[j]], grupa1[,beznazw[k]], method = "spearman")
      r <- wynikkorelacja$estimate
      pvkor <- wynikkorelacja$p.value
      
      if(pvkor < 0.05){
        
        if(r > -1 && r <= -0.7){
          
          write(paste("bardzo silna korelacja ujemna pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r > -0.7 && r <= -0.5){
          
          write(paste("silna korelacja ujemna pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r > -0.5 && r <= -0.3){
          
          write(paste("korelacja ujemna o średnim natężeniu pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r > -0.3 && r <= -0.2){
          
          write(paste("słaba korelacja ujemna pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r > -0.2 && r < 0.2){
          
          #cat("brak korelacji pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n")
          
        }else if(r >= 0.2 && r < 0.3){
          
          write(paste("słaba korelacja dodatnia pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r >= 0.3 && r < 0.5){
        
          write(paste("korelacja dodatnia o średnim natężeniu pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r >= 0.5 && r < 0.7){
          
          write(paste("silna korelacja dodatnia pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }else if(r >= 0.7 && r < 1){
          
          write(paste("bardzo silna korelacja dodatnia pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n"), file = "ProjektBartoszKasprzak.txt", append = TRUE)
          
        }
        
      }else{
       # cat("brak korelacji pomiędzy badaniami ", elementy[beznazw[j]], " oraz ", elementy[beznazw[k]], " w grupie ", grupy[i], "\n")
      }
      
    }
  }
}

dev.off()

