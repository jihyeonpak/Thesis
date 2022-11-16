# results tables for serotypes
library(data.table)
library(dplyr)
library(tidyr)

# run analysis3.R then analysis_serotypes.R code first!

# input: name of aggregated results from each analysis method in analysis3 code
# output: a row for combined table for each analysis method
tableRows <- function(results, tmlerd = FALSE, tmlerr = FALSE){
  
  results <- data.frame(results)
  
  if (tmlerd == TRUE){
    resultsDT <- data.table::setDT(results, keep.rownames = TRUE)[]
    df <- data.frame(resultsDT) %>% 
      dplyr::select(1,2,4,5,6) 
  } else if (tmlerr == TRUE){
    resultsDT <- data.table::setDT(results, keep.rownames = TRUE)[]
    df <- data.frame(resultsDT) %>% 
      dplyr::select(1:5) 
  } else{
    resultsDT <- data.table::setDT(results, keep.rownames = TRUE)[]
    df <- data.frame(resultsDT) %>% 
      dplyr::select(1, 2, 3, 4, ncol(resultsDT)) 
  }
  
  colnames(df) <- c("rn", "estimate", "ci_ll", "ci_ul", "pvalue")
  
  df <- df %>% 
    dplyr::mutate(CI = paste0("(", ci_ll, ", ", ci_ul, ")"),
                  value = paste0(estimate, " ", CI, "; p=", pvalue)) %>% 
    dplyr::select(rn, value) %>% 
    tidyr::spread(rn, value)
  
  return(df)
}

# SEROTYPE1 

# risk difference table 
unadj.rdtable <- tableRows(type1.unadj.rd)
unadj.tmlerdtable <- tableRows(type1.unadj.tmle.rd, tmlerd = TRUE)
adj.rdtable <- tableRows(type1.adj.rd)
adj.tmlerdtable <- tableRows(type1.adj.tmle.rd, tmlerd = TRUE)
adj.ipcw.rdtable <- tableRows(type1.adj.ipcw.rd, tmlerd = TRUE)

type1.rd.table <- rbind(unadj.rdtable, unadj.tmlerdtable, adj.rdtable, adj.tmlerdtable, adj.ipcw.rdtable) 
type1.rd.table$model <- c("Unadjusted GLM (Linear Probability)", "Unadjusted TMLE Model", "Adjusted GLM (Linear Probability)", "Adjusted TMLE", "Adjusted TMLE with IPCW")
type1.rd.table <- type1.rd.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)
# risk ratio table
unadj.rrtable <- tableRows(type1.unadj.rr)
unadj.tmlerrtable <- tableRows(type1.unadj.tmle.rr, tmlerr = TRUE)
adj.rrtable <- tableRows(type1.adj.rr)
adj.tmlerrtable <- tableRows(type1.adj.tmle.rr, tmlerr = TRUE)

type1.rr.table <- rbind(unadj.rrtable, unadj.tmlerrtable, adj.rrtable, adj.tmlerrtable) 
type1.rr.table$model <- c("Unadjusted GLM (Log-Binomial)", "Unadjusted TMLE Model", "Adjusted GLM (Poisson)", "Adjusted TMLE")
type1.rr.table <- type1.rr.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)


## table ##
library(knitr)
library(kableExtra)

kable(type1.rd.table, caption = "Poliovirus serotype 1 risk differences across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3:5), background = "aliceblue")
kable(type1.rr.table, caption = "Poliovirus serotype 1 risk ratios across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3,4), background = "aliceblue")



# SEROTYPE2

# risk difference table 
unadj.rdtable <- tableRows(type2.unadj.rd)
unadj.tmlerdtable <- tableRows(type2.unadj.tmle.rd, tmlerd = TRUE)
adj.rdtable <- tableRows(type2.adj.rd)
adj.tmlerdtable <- tableRows(type2.adj.tmle.rd, tmlerd = TRUE)
adj.ipcw.rdtable <- tableRows(type2.adj.ipcw.rd, tmlerd = TRUE)

type2.rd.table <- rbind(unadj.rdtable, unadj.tmlerdtable, adj.rdtable, adj.tmlerdtable, adj.ipcw.rdtable) 
type2.rd.table$model <- c("Unadjusted GLM (Linear Probability)", "Unadjusted TMLE Model", "Adjusted GLM (Linear Probability)", "Adjusted TMLE", "Adjusted TMLE with IPCW")
type2.rd.table <- type2.rd.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)
# risk ratio table
unadj.rrtable <- tableRows(type2.unadj.rr)
unadj.tmlerrtable <- tableRows(type2.unadj.tmle.rr, tmlerr = TRUE)
adj.rrtable <- tableRows(type2.adj.rr)
adj.tmlerrtable <- tableRows(type2.adj.tmle.rr, tmlerr = TRUE)

type2.rr.table <- rbind(unadj.rrtable, unadj.tmlerrtable, adj.rrtable, adj.tmlerrtable) 
type2.rr.table$model <- c("Unadjusted GLM (Log-Binomial)", "Unadjusted TMLE Model", "Adjusted GLM (Poisson)", "Adjusted TMLE")
type2.rr.table <- type2.rr.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)


## table ##
kable(type2.rd.table, caption = "Poliovirus serotype 2 risk differences across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3:5), background = "aliceblue")
kable(type2.rr.table, caption = "Poliovirus serotype 2 risk ratios across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3,4), background = "aliceblue")


# SEROTYPE3

# risk difference table 
unadj.rdtable <- tableRows(type3.unadj.rd)
unadj.tmlerdtable <- tableRows(type3.unadj.tmle.rd, tmlerd = TRUE)
adj.rdtable <- tableRows(type3.adj.rd)
adj.tmlerdtable <- tableRows(type3.adj.tmle.rd, tmlerd = TRUE)
adj.ipcw.rdtable <- tableRows(type3.adj.ipcw.rd, tmlerd = TRUE)

type3.rd.table <- rbind(unadj.rdtable, unadj.tmlerdtable, adj.rdtable, adj.tmlerdtable, adj.ipcw.rdtable) 
type3.rd.table$model <- c("Unadjusted GLM (Linear Probability)", "Unadjusted TMLE Model", "Adjusted GLM (Linear Probability)", "Adjusted TMLE", "Adjusted TMLE with IPCW")
type3.rd.table <- type3.rd.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)
# risk ratio table
unadj.rrtable <- tableRows(type3.unadj.rr)
unadj.tmlerrtable <- tableRows(type3.unadj.tmle.rr, tmlerr = TRUE)
adj.rrtable <- tableRows(type3.adj.rr)
adj.tmlerrtable <- tableRows(type3.adj.tmle.rr, tmlerr = TRUE)

type3.rr.table <- rbind(unadj.rrtable, unadj.tmlerrtable, adj.rrtable, adj.tmlerrtable) 
type3.rr.table$model <- c("Unadjusted GLM (Log-Binomial)", "Unadjusted TMLE Model", "Adjusted GLM (Poisson)", "Adjusted TMLE")
type3.rr.table <- type3.rr.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)


## table ##
kable(type3.rd.table, caption = "Poliovirus serotype 3 risk differences across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3:5), background = "aliceblue")
kable(type3.rr.table, caption = "Poliovirus serotype 3 risk ratios across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3,4), background = "aliceblue")


