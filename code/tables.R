# results tables
library(data.table)
library(dplyr)
library(tidyr)

# run analysis3 code first!

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

# risk difference table
unadj.rdtable <- tableRows(unadj.rd)
unadj.tmlerdtable <- tableRows(unadj.tmle.rd, tmlerd = TRUE)
adj.rdtable <- tableRows(adj.rd)
adj.tmlerdtable <- tableRows(adj.tmle.rd, tmlerd = TRUE)
adj.ipcw.rdtable <- tableRows(adj.ipcw.rd, tmlerd = TRUE)

rd.table <- rbind(unadj.rdtable, unadj.tmlerdtable, adj.rdtable, adj.tmlerdtable, adj.ipcw.rdtable) 
rd.table$model <- c("Unadjusted GLM (Linear Probability)", "Unadjusted TMLE Model", "Adjusted GLM (Linear Probability)", "Adjusted TMLE", "Adjusted TMLE with IPCW")
rd.table <- rd.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)


# risk ratio table
unadj.rrtable <- tableRows(unadj.rr)
unadj.tmlerrtable <- tableRows(unadj.tmle.rr, tmlerr = TRUE)
adj.rrtable <- tableRows(adj.rr)
adj.tmlerrtable <- tableRows(adj.tmle.rr, tmlerr = TRUE)

rr.table <- rbind(unadj.rrtable, unadj.tmlerrtable, adj.rrtable, adj.tmlerrtable) 
rr.table$model <- c("Unadjusted GLM (Log-Binomial)", "Unadjusted TMLE Model", "Adjusted GLM (Poisson)", "Adjusted TMLE")
rr.table <- rr.table %>% 
  select(model, `A v D`, `B v D`, `C v D`, `A v B`, `A v C`, `B v C`)


## table ##
library(knitr)
library(kableExtra)

kable(rd.table, caption = "Risk differences across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3:5), background = "aliceblue")
kable(rr.table, caption = "Risk ratios across group contrasts") %>%  kable_styling(latex_options = "striped", font_size = 9) %>% row_spec(c(3,4), background = "aliceblue")
