dig.distjd <- function (dat, label = 'None', mod = "ben", lwbound = max(floor(min(abs(dat))) + 
                                            1, (10^(dig - 1))), upbound = ceiling(max(dat)), dig = 1, 
          col = c("#E69F00", "#999999"), colbl = c("#AAFFAA", 
                                                   "#999999"), colbebl = c("#E69F00", "#AAFFAA", 
                                                                           "#999999"), main = "Distribution of digits", 
          No.sd = 0, Sd.pr = 0) 
{
  prep <- function(dat) {
    dat = as.data.frame(dat)
    rownb = dim(dat)[1]
    colnb = dim(dat)[2]
    for (j in 1:colnb) if (is.numeric(dat[, j]) == FALSE) 
      dat[, j] = as.numeric(as.character(dat[, j]))
    for (i in 1:rownb) for (j in 1:colnb) if (is.na(dat[i, 
                                                        j]) == TRUE) 
      dat[i, j] = 0
    return(dat)
  }
  dat = prep(dat)
  rownb = dim(dat)[1]
  colnb = dim(dat)[2]
  num.elig.val = 0
  for (i in 1:rownb) for (j in 1:colnb) if (dat[i, j] >= lwbound) 
    num.elig.val = num.elig.val + 1
  if (num.elig.val == 0) 
    return("No eligible value")
  if (lwbound > upbound) 
    return("lwbound must be less than upbound")
  if (upbound < 10^(dig - 1)) 
    return("upbound does not have enough digits")
  if (lwbound < (10^(dig - 1))) 
    return("lwbound does not have enough digits")
  ond = obs.numb.dig(dat, dig)
  size = sum(ond)
  ofd = ond/size
  coeff = ((upbound - lwbound + 1)/size)^0.5
  if (mod == "ben" & dig == 1) {
   
    Distribution = c(label, "Benford", label, 
                     "Benford", label, "Benford", 
                     label, "Benford", label, 
                     "Benford", label, "Benford", 
                     label, "Benford", label, 
                     "Benford", label, "Benford")
    Digit = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 
              8, 9, 9)
    Freq.Prob = c(ofd[1], Benf.val(1), ofd[2], Benf.val(2), 
                  ofd[3], Benf.val(3), ofd[4], Benf.val(4), ofd[5], 
                  Benf.val(5), ofd[6], Benf.val(6), ofd[7], Benf.val(7), 
                  ofd[8], Benf.val(8), ofd[9], Benf.val(9))
    Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                 0, Sd.pr * (ofd[2] * (1 - ofd[2])/size)^0.5, 0, Sd.pr * 
                   (ofd[3] * (1 - ofd[3])/size)^0.5, 0, Sd.pr * 
                   (ofd[4] * (1 - ofd[4])/size)^0.5, 0, Sd.pr * 
                   (ofd[5] * (1 - ofd[5])/size)^0.5, 0, Sd.pr * 
                   (ofd[6] * (1 - ofd[6])/size)^0.5, 0, Sd.pr * 
                   (ofd[7] * (1 - ofd[7])/size)^0.5, 0, Sd.pr * 
                   (ofd[8] * (1 - ofd[8])/size)^0.5, 0, Sd.pr * 
                   (ofd[9] * (1 - ofd[9])/size)^0.5, 0)
    tab = data.frame(Distribution, Digit, Freq.Prob, Stan.Dev, Event = label, stringsAsFactors = F)
    tab$Digit = as.factor(tab$Digit)
    otab <-subset(tab, Distribution == label)
    btab <-subset(tab, Distribution == 'Benford') 
    p = ggplot(otab, aes(x = Digit, y = Freq.Prob, fill = Distribution) ) + 
      geom_bar(stat = "identity", color = "black", position = position_dodge()) + annotate('point', x = btab$Digit, y = btab$Freq.Prob, colour = 'red', size = 2) +
      geom_errorbar(aes(ymin = Freq.Prob - No.sd * Stan.Dev, ymax = Freq.Prob + No.sd * Stan.Dev), width = 0.3, position = position_dodge(0.9)) + 
      labs(title = main, x = "Digit", y = "Probabilities") + theme_minimal() + theme(legend.title = element_blank()) +
      scale_fill_manual(values = 'grey80')
    
    return(list(graph = p, data = tab))
  }
  else if (mod == "ben") {
    dev.new()
    Distribution = c(label, "Benford", label, 
                     "Benford", label, "Benford", 
                     label, "Benford", label, 
                     "Benford", label, "Benford", 
                     label, "Benford", label, 
                     "Benford", label, "Benford", 
                     label, "Benford")
    Digit = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 
              7, 8, 8, 9, 9)
    Freq.Prob = c(ofd[1], Benf.val(0, dig), ofd[2], Benf.val(1, 
                                                             dig), ofd[3], Benf.val(2, dig), ofd[4], Benf.val(3, 
                                                                                                              dig), ofd[5], Benf.val(4, dig), ofd[6], Benf.val(5, 
                                                                                                                                                               dig), ofd[7], Benf.val(6, dig), ofd[8], Benf.val(7, 
                                                                                                                                                                                                                dig), ofd[9], Benf.val(8, dig), ofd[10], Benf.val(9, 
                                                                                                                                                                                                                                                                  dig))
    Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                 0, Sd.pr * (ofd[2] * (1 - ofd[2])/size)^0.5, 0, Sd.pr * 
                   (ofd[3] * (1 - ofd[3])/size)^0.5, 0, Sd.pr * 
                   (ofd[4] * (1 - ofd[4])/size)^0.5, 0, Sd.pr * 
                   (ofd[5] * (1 - ofd[5])/size)^0.5, 0, Sd.pr * 
                   (ofd[6] * (1 - ofd[6])/size)^0.5, 0, Sd.pr * 
                   (ofd[7] * (1 - ofd[7])/size)^0.5, 0, Sd.pr * 
                   (ofd[8] * (1 - ofd[8])/size)^0.5, 0, Sd.pr * 
                   (ofd[9] * (1 - ofd[9])/size)^0.5, 0, Sd.pr * 
                   (ofd[10] * (1 - ofd[10])/size)^0.5, 0)
    tab = data.frame(Distribution, Digit, Freq.Prob, Stan.Dev)
    tab$Digit = as.factor(tab$Digit)
    p = ggplot(subset(tab, Distribution == label), aes(x = Digit, y = Freq.Prob, fill = Distribution)) + 
      geom_bar(stat = "identity", color = "black", 
               position = position_dodge()) + geom_errorbar(aes(ymin = Freq.Prob - 
                                                                  No.sd * Stan.Dev, ymax = Freq.Prob + No.sd * Stan.Dev), 
                                                            width = 0.3, position = position_dodge(0.9)) + labs(title = main, 
                                                                                                                x = "Digit", y = "Probabilities") +  
      scale_fill_manual(values = col) + theme_minimal() + geom_point(data = subset(tab, Distribution == 'Benford'), aes(colour = Distribution)) +
    print(p)
    return(p)
  }
  else if (mod == "ben&blo") {
    if (dig == 1) {
      dev.new()
      Distribution = c(label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau")
      Digit = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 
                5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9)
      Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                   0, coeff * Blon.val.sd(lwbound, upbound, 1), 
                   Sd.pr * (ofd[2] * (1 - ofd[2])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 2), Sd.pr * 
                     (ofd[3] * (1 - ofd[3])/size)^0.5, 0, coeff * 
                     Blon.val.sd(lwbound, upbound, 3), Sd.pr * (ofd[4] * 
                                                                  (1 - ofd[4])/size)^0.5, 0, coeff * Blon.val.sd(lwbound, 
                                                                                                                 upbound, 4), Sd.pr * (ofd[5] * (1 - ofd[5])/size)^0.5, 
                   0, coeff * Blon.val.sd(lwbound, upbound, 5), 
                   Sd.pr * (ofd[6] * (1 - ofd[6])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 6), Sd.pr * 
                     (ofd[7] * (1 - ofd[7])/size)^0.5, 0, coeff * 
                     Blon.val.sd(lwbound, upbound, 7), Sd.pr * (ofd[8] * 
                                                                  (1 - ofd[8])/size)^0.5, 0, coeff * Blon.val.sd(lwbound, 
                                                                                                                 upbound, 8), Sd.pr * (ofd[9] * (1 - ofd[9])/size)^0.5, 
                   0, coeff * Blon.val.sd(lwbound, upbound, 9))
      Freq.Prob = c(ofd[1], Benf.val(1), Blon.val(lwbound, 
                                                  upbound, 1), ofd[2], Benf.val(2), Blon.val(lwbound, 
                                                                                             upbound, 2), ofd[3], Benf.val(3), Blon.val(lwbound, 
                                                                                                                                        upbound, 3), ofd[4], Benf.val(4), Blon.val(lwbound, 
                                                                                                                                                                                   upbound, 4), ofd[5], Benf.val(5), Blon.val(lwbound, 
                                                                                                                                                                                                                              upbound, 5), ofd[6], Benf.val(6), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                         upbound, 6), ofd[7], Benf.val(7), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                    upbound, 7), ofd[8], Benf.val(8), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                                               upbound, 8), ofd[9], Benf.val(9), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                                                                                          upbound, 9))
      tab = data.frame(Distribution, Digit, Freq.Prob, 
                       Stan.Dev)
      tab$Digit = as.factor(tab$Digit)
      p = ggplot(subset(tab, Distribution == label), aes(x = Digit, y = Freq.Prob, fill = Distribution)) + 
        geom_bar(stat = "identity", color = "black", 
                 position = position_dodge()) + geom_errorbar(aes(ymin = Freq.Prob - 
                                                                    No.sd * Stan.Dev, ymax = Freq.Prob + No.sd * 
                                                                    Stan.Dev), width = 0.3, position = position_dodge(0.9)) + 
        geom_point(data = subset(tab, Distribution == Benford), aes(colour = Distribution)) +
        labs(title = main, x = "Digit", y = "Probabilities") + 
        theme_minimal() + scale_fill_manual(values = colbebl)
      print(p)
      return(p)
    }
    else {
      Distribution = c(label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau", label, "Benford", 
                       "Blondeau")
      Digit = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
                4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 
                9, 9)
      Freq.Prob = c(ofd[1], Benf.val(0, dig), Blon.val(lwbound, 
                                                       upbound, 0, dig), ofd[2], Benf.val(1, dig), Blon.val(lwbound, 
                                                                                                            upbound, 1, dig), ofd[3], Benf.val(2, dig), Blon.val(lwbound, 
                                                                                                                                                                 upbound, 2, dig), ofd[4], Benf.val(3, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                      upbound, 3, dig), ofd[5], Benf.val(4, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                           upbound, 4, dig), ofd[6], Benf.val(5, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                upbound, 5, dig), ofd[7], Benf.val(6, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                                                                     upbound, 6, dig), ofd[8], Benf.val(7, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                                                                                                                          upbound, 7, dig), ofd[9], Benf.val(8, dig), Blon.val(lwbound, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               upbound, 8, dig), ofd[10], Benf.val(9, dig), 
                    Blon.val(lwbound, upbound, 9, dig))
      Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                   0, coeff * Blon.val.sd(lwbound, upbound, 0, dig), 
                   Sd.pr * (ofd[2] * (1 - ofd[2])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 1, dig), 
                   Sd.pr * (ofd[3] * (1 - ofd[3])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 2, dig), 
                   Sd.pr * (ofd[4] * (1 - ofd[4])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 3, dig), 
                   Sd.pr * (ofd[5] * (1 - ofd[5])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 4, dig), 
                   Sd.pr * (ofd[6] * (1 - ofd[6])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 5, dig), 
                   Sd.pr * (ofd[7] * (1 - ofd[7])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 6, dig), 
                   Sd.pr * (ofd[8] * (1 - ofd[8])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 7, dig), 
                   Sd.pr * (ofd[9] * (1 - ofd[9])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 8, dig), 
                   Sd.pr * (ofd[10] * (1 - ofd[10])/size)^0.5, 0, 
                   coeff * Blon.val.sd(lwbound, upbound, 9, dig))
      tab = data.frame(Distribution, Digit, Freq.Prob, 
                       Stan.Dev)
      tab$Digit = as.factor(tab$Digit)
      p = ggplot(subset(tab, Distribution == label), aes(x = Digit, y = Freq.Prob, fill = Distribution)) + 
        geom_bar(stat = "identity", color = "black", 
                 position = position_dodge()) + geom_errorbar(aes(ymin = Freq.Prob - 
                                                                    No.sd * Stan.Dev, ymax = Freq.Prob + No.sd * 
                                                                    Stan.Dev), width = 0.3, position = position_dodge(0.9)) + 
        labs(title = main, x = "Digit", y = "Probabilities") + 
        theme_minimal() + scale_fill_manual(values = colbebl)  + geom_point(data = subset(tab, Distribution == Benford), aes(colour = Distribution)) +
      print(p)
      return(p)
    }
  }
  else if ((mod != "ben") & (mod != "ben&blo")) {
    {
      if (dig == 1) {
        dev.new()
        Distribution = c(label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau", label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau", label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau")
        Digit = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 
                  7, 7, 8, 8, 9, 9)
        Freq.Prob = c(ofd[1], Blon.val(lwbound, upbound, 
                                       1), ofd[2], Blon.val(lwbound, upbound, 2), 
                      ofd[3], Blon.val(lwbound, upbound, 3), ofd[4], 
                      Blon.val(lwbound, upbound, 4), ofd[5], Blon.val(lwbound, 
                                                                      upbound, 5), ofd[6], Blon.val(lwbound, upbound, 
                                                                                                    6), ofd[7], Blon.val(lwbound, upbound, 7), 
                      ofd[8], Blon.val(lwbound, upbound, 8), ofd[9], 
                      Blon.val(lwbound, upbound, 9))
        Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                     coeff * Blon.val.sd(lwbound, upbound, 1), Sd.pr * 
                       (ofd[2] * (1 - ofd[2])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 2), Sd.pr * 
                       (ofd[3] * (1 - ofd[3])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 3), Sd.pr * 
                       (ofd[4] * (1 - ofd[4])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 4), Sd.pr * 
                       (ofd[5] * (1 - ofd[5])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 5), Sd.pr * 
                       (ofd[6] * (1 - ofd[6])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 6), Sd.pr * 
                       (ofd[7] * (1 - ofd[7])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 7), Sd.pr * 
                       (ofd[8] * (1 - ofd[8])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 8), Sd.pr * 
                       (ofd[9] * (1 - ofd[9])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 9))
        tab = data.frame(Distribution, Digit, Freq.Prob, 
                         Stan.Dev)
        tab$Digit = as.factor(tab$Digit)
        p = ggplot(subset(tab, Distribution == label), aes(x = Digit, y = Freq.Prob, 
                            fill = Distribution)) + geom_bar(stat = "identity", 
                                                             color = "black", position = position_dodge()) + 
          geom_errorbar(aes(ymin = Freq.Prob - No.sd * 
                              Stan.Dev, ymax = Freq.Prob + No.sd * Stan.Dev), 
                        width = 0.3, position = position_dodge(0.9)) + 
          labs(title = main, x = "Digit", y = "Probabilities") + 
          theme_classic() + scale_fill_manual(values = colbl) + geom_point(data = subset(tab, Distribution == Benford), aes(colour = Distribution)) +
        print(p)
        return(p)
      }
      else {
        dev.new()
        Distribution = c(label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau", label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau", label, "Blondeau", 
                         label, "Blondeau", label, 
                         "Blondeau", label, "Blondeau")
        Digit = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 
                  6, 6, 7, 7, 8, 8, 9, 9)
        Freq.Prob = c(ofd[1], Blon.val(lwbound, upbound, 
                                       0, dig), ofd[2], Blon.val(lwbound, upbound, 
                                                                 1, dig), ofd[3], Blon.val(lwbound, upbound, 
                                                                                           2, dig), ofd[4], Blon.val(lwbound, upbound, 
                                                                                                                     3, dig), ofd[5], Blon.val(lwbound, upbound, 
                                                                                                                                               4, dig), ofd[6], Blon.val(lwbound, upbound, 
                                                                                                                                                                         5, dig), ofd[7], Blon.val(lwbound, upbound, 
                                                                                                                                                                                                   6, dig), ofd[8], Blon.val(lwbound, upbound, 
                                                                                                                                                                                                                             7, dig), ofd[9], Blon.val(lwbound, upbound, 
                                                                                                                                                                                                                                                       8, dig), ofd[10], Blon.val(lwbound, upbound, 
                                                                                                                                                                                                                                                                                  9, dig))
        Stan.Dev = c(Sd.pr * (ofd[1] * (1 - ofd[1])/size)^0.5, 
                     coeff * Blon.val.sd(lwbound, upbound, 0, dig), 
                     Sd.pr * (ofd[2] * (1 - ofd[2])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 1, dig), Sd.pr * 
                       (ofd[3] * (1 - ofd[3])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 2, dig), Sd.pr * 
                       (ofd[4] * (1 - ofd[4])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 3, dig), Sd.pr * 
                       (ofd[5] * (1 - ofd[5])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 4, dig), Sd.pr * 
                       (ofd[6] * (1 - ofd[6])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 5, dig), Sd.pr * 
                       (ofd[7] * (1 - ofd[7])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 6, dig), Sd.pr * 
                       (ofd[8] * (1 - ofd[8])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 7, dig), Sd.pr * 
                       (ofd[9] * (1 - ofd[9])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 8, dig), Sd.pr * 
                       (ofd[10] * (1 - ofd[10])/size)^0.5, coeff * 
                       Blon.val.sd(lwbound, upbound, 9, dig))
        tab = data.frame(Distribution, Digit, Freq.Prob, 
                         Stan.Dev)
        tab$Digit = as.factor(tab$Digit)
        p = ggplot(subset(tab, Distribution == label), aes(x = Digit, y = Freq.Prob, 
                            fill = Distribution)) + geom_bar(stat = "identity", 
                                                             color = "black", position = position_dodge()) + 
          geom_errorbar(aes(ymin = Freq.Prob - No.sd * 
                              Stan.Dev, ymax = Freq.Prob + No.sd * Stan.Dev), 
                        width = 0.3, position = position_dodge(0.9)) + 
          labs(title = main, x = "Digit", y = "Probabilities") + 
          theme_classic() + scale_fill_manual(values = colbl) + geom_point(data = subset(tab, Distribution == Benford), aes(colour = Distribution)) +
        print(p)
        return(p)
      }
    }
  }
}