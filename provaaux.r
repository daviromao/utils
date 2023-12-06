enem = read.csv("~/pee/ENEM22.csv", header = TRUE, sep = ";", dec = ".")

## Questão 1
## a)
descritiva = function(x){
  media = mean(x)
  mediana = median(x)
  cat(" Média: ",media,"\n",
      "Mediana: ",mediana,"\n",
      "Modas: ", as.numeric(names(subset(table(x), table(x)==max(table(x))))),"\n",
      "Quartis:\n")
  quantile(x)
}

descritiva(enem$NU_NOTA_ENEM)

## b)
dispersao = function(x){
  amplitude = max(x) - min(x)
  variancia = var(x)
  desvio = sd(x)
  cv = (sd(x)/mean(x)) * 100
  cat(" Amplitude:", amplitude, "\n",
      "Variância:", variancia, "\n",
      "Desvio Padrão:", desvio, "\n",
      "Coeficiente de Variação:", cv, "\n")
}

dispersao(enem$NU_NOTA_ENEM)

## c)
hist(enem$NU_NOTA_ENEM)
# falar que é uma assimetria positiva

## Questão 2
boxplot(enem$NU_NOTA_CH, enem$NU_NOTA_CN, enem$NU_NOTA_LC, enem$NU_NOTA_MT, enem$NU_NOTA_REDACAO, enem$NU_NOTA_ENEM,
        names=c("Humanas", "Natureza", "Linguagens", "Matemática", "Redação", "Nota Final"))

## Questão 3
amplitude = max(enem$NU_NOTA_ENEM) - min(enem$NU_NOTA_ENEM)
nk = nclass.Sturges(enem$NU_NOTA_ENEM)
ampClasses = amplitude/nk
limiteClas = seq(min(enem$NU_NOTA_ENEM), max(enem$NU_NOTA_ENEM), ampClasses)

Freq = table(cut(enem$NU_NOTA_ENEM, breaks = limiteClas, right = FALSE, include.lowest = TRUE))
FreqAc = cumsum(Freq)
FreqRel = prop.table(Freq)
FreqRelAc = cumsum(FreqRel)
TabResul = cbind(Freq, FreqAc, FreqRel = round(FreqRel*100, digits=2), FreqRelAc = round(FreqRelAc*100, digits=2))

H = hist(enem$NU_NOTA_ENEM, breaks = limiteClas,
         labels =c("[372,405)","[405,438)","[438,471)","[471,504)","[504,537)","[537,570)","[570,603)","[603,636)","[636,669)","[669,702)","[702,735)","[735,768]"))
H

## Questão 4
nota_cidade = table(enem$MUNICIPIO_ESC, cut(enem$NU_NOTA_ENEM, breaks=quantile(enem$NU_NOTA_ENEM), include.lowest = TRUE, right = FALSE))
barplot(nota_cidade, beside = TRUE, legend = TRUE)

## Questão 5
notas = cbind(CH = enem$NU_NOTA_CH, CN = enem$NU_NOTA_CN, LC = enem$NU_NOTA_LC, MT = enem$NU_NOTA_MT, RED =enem$NU_NOTA_REDACAO, FINAL = enem$NU_NOTA_ENEM)
mc = cor(notas)

plot(enem$NU_NOTA_REDACAO, enem$NU_NOTA_ENEM,
     main="FINAL  x RED",
     col="blue", pch=21, lwd=2,
     xlab = "NOTA REDACAO",
     ylab = "NOTA FINAL")
abline(lsfit(enem$NU_NOTA_REDACAO, enem$NU_NOTA_ENEM), col="darkred")