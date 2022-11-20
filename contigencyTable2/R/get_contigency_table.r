#' Function to create a complete table results for contigency table
#' 
#' @usage \code{get_contigency_result(n11, n12, n21, n22,
#'     varname1 = "Expose", varname2 = "Disease",
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"), show_table_results = TRUE)}
#' @param n11 The number that shows this is that the first 
#' variable of the table is at its first level and the second 
#' variable of the table is also at its first level
#' 
#' @param n12 The numbers that indicate this, the first 
#'     variable of the table is on its 
#'     first level and the second variable of 
#'     the table is on its second level
#' @param n21 The numbers that indicate this, 
#'     the first variable of the table is on its second 
#'     level and the second variable of the table is on its first level
#' @param n22 The numbers that indicate this, the first variable of 
#'     the table is on its second level and the second variable of 
#'     the table is also on its second level
#' @param varname1 name of first variable
#' @param varname2 name of second variable
#' @param levels_var1 levels of first variable
#' @param levels_var2 levels of second variable
#' @param show_table_results A logical variable that takes two values, 
#'     FALSE and TRUE, when in the TRUE state, is displayed in the output of a 
#'     complete table as an HTML page.
#' @return Table_results A list containing 8 output tables in 
#'     \href{https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html}{kableExtra} format, 
#'     showing the outputs for each table.
#' @return stat_R_results list of 8 table as dataframe format for show
#'     result of table that generate from contigency table.
#' @export
#' @examples
#'\dontrun{get_contigency_result(475, 461, 7, 61, "Expose", "Disease",levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", "UnDisease"), show_table_results = TRUE)}
get_contigency_result <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Disease",
levels_var1 = c("Exposed", "UnExposed"), 
levels_var2 = c("Disease", "UnDisease"), show_table_results = TRUE){
## define data 

dat <- matrix(c(n11, n21, n12, n22), 2, 2) 
dimnames(dat) <- list(rows = levels_var1,
columns = levels_var2)
dat_tab <- as.table(dat)

## create expected tab

ex_tab <- epitools :: expected(dat_tab) 
ex_tab <- round(ex_tab, 3)

## create 3 first rows 
c1 <- c("", "", "", varname2, "")
c2 <- c("", "", levels_var2[1], levels_var2[2], "Total")
c3 <- c(varname1, "", "", "", "")

r1 <- c(dat_tab[1, ], sum(dat_tab[1, ]))
r2 <- ex_tab[1, ]
r3 <- c(r1/sum(dat_tab) * 100) 
r3 <- round(r3, 3)
r4 <- (r1/sum(r1) * 100)
r4 <- round(r4, 3)[1:2]
r5 <- (dat_tab[1, ]/c(sum(dat_tab[, 1]), sum(dat_tab[, 2]))*100) 
r5 <- round(r5, 3)

## get informaion about 3 second rows
r6 <- c(dat_tab[2, ], sum(dat_tab[2, ]))
r7 <- ex_tab[2, ]
r8 <- c(r6/sum(dat_tab) * 100, 
sum(r6/sum(dat_tab)) * 100) 
r8 <- round(r8, 3)[-4]
r9 <- (r6/r6[3] * 100) 
r9 <- round(r9, 3)[-3]

r10 <- (dat_tab[2, ]/c(sum(dat_tab[, 1]), sum(dat_tab[, 2]))*100) 
r10 <- round(r10, 3)
r11 <- c(sum(dat_tab[, 1]), sum(dat_tab[, 2]),
sum(dat_tab))
r12 <- c(r11/sum(dat_tab) * 100) 
r12 <- round(r12, 3)

## Define details of table n.1

r1 <- c(levels_var1[1], "Frequency", r1)
r2 <- c(levels_var1[1], "Expected", r2, "")
r3 <- c(levels_var1[1], "Percent", r3)
r4 <- c(levels_var1[1], "Row Pct", r4, "")
r5 <- c(levels_var1[1], "Col Pct", r5, "")

r6 <- c(levels_var1[2], "Frequency", r6)
r7 <- c(levels_var1[2], "Expected", r7, "")
r8 <- c(levels_var1[2], "Percent", r8)
r9 <- c(levels_var1[2], "Row Pct", r9, "")
r10 <- c(levels_var1[2], "Col Pct", r10, "")
d1 <- c("", "", "", "", "")
r11 <- c("Total", "Frequency", r11)
r12 <- c("", "Percent", r12)
freq_table <- rbind(c1, c2, 
c3, r1, r2, r3, 
r4, r5, r6, r7, r8, r9, r10, 
d1, r11, r12) 

freq_table <- as.data.frame(freq_table)
freq_table <- setNames(freq_table, NULL)

rownames(freq_table) <- NULL
lab1 <- paste("Table of", varname1, "by", 
varname2, sep = " ")


a1 <- kableExtra :: kbl(freq_table[, -1], 
caption = lab1, align = "r")

a2 <- kableExtra :: kable_paper(a1, "hover", full_width = F)

a3 <- kableExtra :: column_spec(a2, 1, border_left = T, border_right = T)

a4 <- kableExtra :: column_spec(a3, 1:2, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

a5 <- kableExtra :: row_spec(a4, 1:3, 
color = "#1c0345", 
background = "#d4ecf65e", 
hline_after = T, 
bold = T, 
italic = T)
a6 <- kableExtra :: pack_rows(a5, levels_var1[1], 4, 8)

a7 <- kableExtra :: pack_rows(a6, levels_var1[2], 9, 13) -> table1

## statistics table





## chi-Square 

res_chisq_F <- chisq.test(dat_tab, correct = F) 
chi_F <- res_chisq_F$statistic
chi_F <- round(chi_F, 3)


p_chi_F <- res_chisq_F$p.value 
p_chi_F <- round(p_chi_F, 3)

res_chisq_T <- chisq.test(dat_tab, correct = T)
chi_T <- res_chisq_T$statistic
chi_T <- round(chi_T, 4)
p_chi_T <- res_chisq_T$p.value
p_chi_T <- round(p_chi_T, 4)


## Mantel-Haenszel Chi-square
Mantel <- DescTools :: MHChisqTest(dat_tab)
mstat <- Mantel$statistic
mstat <- round(mstat, 4)
p_mantel <- Mantel$p.value
p_mantel <- round(p_mantel, 4)


## likelihood Ratio chi-square

like <- vcd :: assocstats(dat_tab)

likel_stat <- like$chisq_tests[1, 1]
likel_stat <- round(likel_stat, 4)
p_like <- like$chisq_tests[1, 3]
p_like <- round(p_like, 4)
Phi <- like$phi 
Phi <- round(Phi, 4)
Contigency_Coefficient <- like$contingency 
Contigency_Coefficient <- round(Contigency_Coefficient, 4)
Cram <- like$cramer 
Cram <- round(Cram, 4)

pv1 <- c(p_chi_F, p_like, p_chi_T, p_mantel) 
ind <- which(pv1 <= 0.0001)
pv1[ind] <- "< .0001"

stat_table <- data.frame(
v1 = c("Statistics", 
"Chi-Square", 
"Likelihood Ratio Chi-Square", 
"Contigency Adj.Chi-Square", 
"Mantel Haenszel Chi-Square",
"Phi Coefficient", 
"Contigency Coefficient", 
"Cramer's V"), 
v2 = c("DF", rep(1, 4), "", "", ""), 
v3 = c("Value", chi_F, likel_stat, chi_T, mstat, Phi, Contigency_Coefficient, 
Cram),  
v4 = c("P-value", pv1, "", "", "")
)


names(stat_table) <- NULL

## create table stat_table

lab2 <- paste("Statistics for Table of", varname1, "by", varname2, 
sep = " ")

b1 <- kableExtra :: kbl(stat_table, caption = lab2, escape = F)

b2 <- kableExtra :: kable_paper(b1, "hover", full_width = F)

b3 <- kableExtra :: column_spec(b2, 1, 
 border_left = T, 
 border_right = T)

table2 <- kableExtra :: row_spec(b3, 1, 
color = "#1c0345", 
background = "#d4ecf65e", 
hline_after = T, 
bold = T, 
italic = T)



## fisher exact test

cell_11 <- dat_tab[1, 1]

fish_left <- fisher.test(dat_tab, alternative = "less")
fish_right <- fisher.test(dat_tab, alternative = "greater")
fish_two_side <- fisher.test(dat_tab, alternative = "two.sided")
p_fish_l <- fish_left$p.value 
p_fish_l <- round(p_fish_l, 4)
p_fish_r <- fish_right$p.value 
p_fish_r <- round(p_fish_r, 4)
p_fish_two <- fish_two_side$p.value 
p_fish_two <- round(p_fish_two, 4)
m <- sum(dat_tab[, 1]); n <- sum(dat_tab[, 2]); 

k <- sum(dat_tab[1, ])
tab_p <- round(dhyper(cell_11, m, n, k), 4) 
fish_res <- c(
cell_11, 
ifelse(p_fish_l <= 0.0001, "<.0001", p_fish_l), 
ifelse(p_fish_r <= 0.0001, "<.0001", p_fish_r),
"", 
ifelse(tab_p <= 0.0001, "<.0001", tab_p), 
ifelse(p_fish_two <= 0.0001, "<.0001", p_fish_two)
)
fisher_exact_test <- data.frame(
v1 = c("cell[1, 1] Frequancy (F)", 
"Left-sided p-value", 
"Right-sided p-value", 
"", "Table Probability (P)", "Two-sided p-value"), 
v2 = fish_res
) 
names(fisher_exact_test) <- NULL

c1 <- kableExtra :: kbl(fisher_exact_test, caption = "Fisher's Exact Test")

c2 <- kableExtra :: kable_paper(c1, "hover", full_width = F)

c3 <- kableExtra :: column_spec(c2, 1, 
 border_left = T, 
 border_right = T)

table3 <- kableExtra :: column_spec(c3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)



## other statistics

### define a function for create 
### original data from contigency table 2x2

create_dat_two <- function(tab, name1, 
name2){
n <- sum(tab)
n11 = tab[1, 1]; n12 = tab[1, 2]; 
n21 = tab[2, 1]; n22 = tab[2, 2]
x <- rep(c(1, 1, 0, 0),  c(n11, n12, n21, n22))
y <- rep(c(1, 0, 1, 0), c(n11, n12, n21, n22))


res <- as.data.frame(cbind(x, y))
names(res) <- c(name1, name2)
return(res)
}

Dat <- create_dat_two(dat_tab, varname1, varname2)

gama_cor <- Hmisc :: rcorr.cens(Dat[[varname1]], Dat[[varname2]], 
outx = TRUE)[2]
round(gama_cor, 4) -> gama_cor
## kendal and pearson and spearman correlation

pears <- round(cor(Dat[[varname1]], Dat[[varname2]], method = "pearson"), 4)
spear <- round(cor(Dat[[varname1]], Dat[[varname2]], method = "spearman"), 4)

kend_B <- round(DescTools :: KendallTauB(Dat[[varname1]], Dat[[varname2]]), 4)
kend_C <- round(DescTools :: StuartTauC(Dat[[varname1]], Dat[[varname2]]), 4)
kend_A <- round(DescTools :: KendallTauA(Dat[[varname1]], Dat[[varname2]]), 4)

somers_r_c <- round(Hmisc :: somers2(Dat[[varname1]], 
Dat[[varname2]])[2], 4)
somers_c_r <- round(Hmisc :: somers2(Dat[[varname2]], 
Dat[[varname1]])[2], 4)

## lambda assymetric C|R
tabd <- table(Dat)
v1 <- max(tabd[1, ])
v2 <- max(tabd[2, ])
v <- max(colSums(tabd))
w1 <- max(tabd[, 1])
w2 <- max(tabd[, 2])
w <- max(rowSums(tabd))
n <- sum(tabd)
lam_c_r <- (v1 + v2 - v)/(n-v)
lam_c_r <- round(lam_c_r, 4)

## get lambda C|R
Dat2 <- Dat[, 2:1]
tabd2 <- table(Dat2)
v1_2 <- max(tabd2[1, ])
v2_2 <- max(tabd2[2, ])
v_2 <- max(colSums(tabd2))
w1_2 <- max(tabd2[, 1])
w2_2 <- max(tabd2[, 2])
w_2 <- max(rowSums(tabd2))
lam_r_c <- (v1_2 + v2_2 - v_2)/(n-v_2) 
lam_r_c <- round(lam_r_c, 4)

## get lambda Symmetric

lam <- (v1 + v2 + w1 + w2
-v - w)/(2*n - v - w) 
lam <- round(lam, 4)

## Uncertainty C|R

hx <- -sum(rowSums(tabd)/n *log(rowSums(tabd)/n))
hy <- -sum(colSums(tabd)/n * log(colSums(tabd)/n))
hxy <- -sum(tabd/n * log(tabd/n))
hv <- hx + hy - hxy
unc_c_r <- hv/hy 
unc_c_r <- round(unc_c_r, 4)

## Uncertainty R|C

hx_2 <- -sum(rowSums(tabd2)/n *log(rowSums(tabd2)/n))
hy_2 <- -sum(colSums(tabd2)/n * log(colSums(tabd2)/n))
hxy_2 <- -sum(tabd2/n * log(tabd2/n))
hv_2 <- hx_2 + hy_2 - hxy_2
unc_r_c <- hv_2/hy_2 
unc_r_c <- round(unc_r_c, 4)

## Uncertainty sym

unc_sym <- 2 * (hx + hy - hxy)/(hx + hy)
unc_sym <- round(unc_sym, 4)

Coef_table <- data.frame(
v1 = c("Gamma", "Kendall's Tau-b",
"Stuart's Tau-c", 
"Kendall's Tau-A",
"Somers'D C|R", 
"Somers'D R|C", 
"Pearson Correlation", 
"Spearman Correlation",
"Lambda Asymmetric C|R", 
"Lambda Asymmetric R|C", 
"Lambda Symmetric", 
"Uncertainty Coefficient C|R", 
"Uncertainty Coefficient R|C", 
"Uncertainty Coefficient Symmetric"), 
v2 = c(gama_cor, kend_B, kend_C, kend_A, somers_c_r, somers_r_c, 
pears, spear, lam_c_r, lam_r_c, lam, unc_c_r, unc_r_c, unc_sym) 
)

names(Coef_table) <- NULL
labs <- "Coefficients Statistics (correlation and other coefficients)"
d1 <- kableExtra :: kbl(Coef_table, caption = labs)

d2 <- kableExtra :: kable_paper(d1, "hover", full_width = F)

d3 <- kableExtra ::  column_spec(d2, 1, 
 border_left = T, 
 border_right = T)

table4 <- kableExtra :: column_spec(d3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T) 
############## Odds Ratio

#### ref = column 1

## or wald 
or_wald <- epitools :: oddsratio(tabd, method = "wald")
or_wald_val <- round(or_wald$measure[2, 1], 4)

## or midp 

or_midp <- epitools :: oddsratio(tabd, method = "midp")
or_midp_val <- round(or_midp$measure[2, 1], 4) 

## or exact
or_exact <- epitools :: oddsratio(tabd, method = "fisher")
or_exact_val <- round(or_exact$measure[2, 1], 4)

## confint or wald

ci_wald <- round(or_wald$measure[2, 2:3], 4)



## confint or midp

ci_midp <- round(or_midp$measure[2, 2:3], 4)



## confint or exact

ci_exact <- round(or_exact$measure[2, 2:3], 4)



## or  pval wald 

round(or_wald$p.value[2, 3], 4) -> pval_wald

## or pval midp 

round(or_midp$p.value[2, 1], 4) -> pval_midp

## or pval exact 

round(or_exact$p.value[2, 2], 4) -> pval_exact

#### ref = column 2

## or2 wald 
or_wald_2 <- epitools :: oddsratio(tabd[, 2:1], method = "wald")
or_wald_val_2 <- round(or_wald_2$measure[2, 1], 4)


## or2 midp 

or_midp_2 <- epitools :: oddsratio(tabd[, 2:1], method = "midp")
or_midp_val_2 <- round(or_midp_2$measure[2, 1], 4)

## or exact
or_exact_2 <- epitools :: oddsratio(tabd[, 2:1], method = "fisher")
or_exact_val_2 <- round(or_exact_2$measure[2, 1], 4)

## confint or wald2

ci_wald_2 <- round(or_wald_2$measure[2, 2:3], 4)



## confint or midp

ci_midp_2 <- round(or_midp_2$measure[2, 2:3], 4)



## confint or exact

ci_exact_2 <- round(or_exact_2$measure[2, 2:3], 4)



## or2  pval wald 

round(or_wald_2$p.value[2, 3], 4) -> pval_wald_2

## or2 pval midp 

round(or_midp_2$p.value[2, 1], 4) -> pval_midp_2

## or2 pval exact 

round(or_exact_2$p.value[2, 2], 4) -> pval_exact_2


######## Risk ratio ref = column 1


## rr wald
rr_wald <- epitools :: riskratio(tabd, method = "wald")
round(rr_wald$measure[2, 1], 4) -> rr_wald_val
 
## rr boot

rr_boot <- epitools :: riskratio.boot(tabd, replicates = 5000)
round(rr_boot$measure[2, 1], 4) -> rr_boot_val

## rr wald ci

rr_ci_wald <- round(rr_wald$measure[2, 2:3], 4)

## rr boot ci

rr_ci_boot <- round(rr_boot$measure[2, 2:3], 4)

## rr pval wald

round(rr_wald$p.value[2, 3], 4) -> rr_wald_pval


## rr pval exact 

round(rr_boot$p.value[2, 2], 4) -> rr_exact_pval




######## Risk ratio ref = column 2


## rr2 wald
rr_wald_2 <- epitools :: riskratio(tabd[, 2:1], method = "wald")
round(rr_wald_2$measure[2, 1], 4) -> rr_wald_val_2

## rr2 boot

rr_boot_2 <- epitools :: riskratio.boot(tabd[, 2:1], replicates = 5000)
round(rr_boot_2$measure[2, 1], 4) -> rr_boot_val_2

## rr2 wald ci

rr_ci_wald_2 <- round(rr_wald_2$measure[2, 2:3], 4)
## rr2 boot ci

rr_ci_boot_2 <- round(rr_boot_2$measure[2, 2:3], 4)
## rr2 pval wald

round(rr_wald_2$p.value[2, 3], 4) -> rr_wald_pval_2


## rr2 pval exact 

round(rr_boot_2$p.value[2, 2], 4) -> rr_exact_pval_2

## create result oddsratio

oddsRatio_results <- data.frame(
v1 = 
c("Reffrence", rep("Reffrence = Column 1", 3), 
rep("Reffrence = Column 2", 3)), 
v2 = c("Method", "Wald", "midp", "Fisher-Exact",
"Wald", "midp", "Fisher-Exact"), 
v3 = c(
"Estimate", 
or_wald_val, or_midp_val, or_exact_val, 
or_wald_val_2, or_midp_val_2, or_exact_val_2
),
v4 = c("Lower bond, CI = 95%", 
ci_wald[1], ci_midp[1], ci_exact[1], 
ci_wald_2[1], 
ci_midp_2[1], 
ci_exact_2[1]), 

v5 =  c("Upper bond level, CI = 95%", 
ci_wald[2], ci_midp[2], ci_exact[2], 
ci_wald_2[2], 
ci_midp_2[2], 
ci_exact_2[2])
)

names(oddsRatio_results) <- NULL

e1 <- kableExtra :: kbl(oddsRatio_results[, -1], 
caption = "Odds Ratio Results", align = "l") 

e2 <- kableExtra :: kable_paper(e1, "hover", full_width = F)


e3 <- kableExtra :: column_spec(e2, 1:4, 
 border_left = T, 
 border_right = T)


e4 <- kableExtra :: column_spec(e3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)
e5 <- kableExtra :: row_spec(e4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

e6 <- kableExtra :: pack_rows(e5, "Reffrence = Column:1", 2, 4) 
kableExtra :: pack_rows(e6, "Reffrence = Column:2", 5, 7) -> table5


## pval table for oddratio

pval_result <- c(pval_wald, pval_midp, pval_exact, 
pval_wald_2, pval_midp_2, pval_exact_2)

ind2 <- pval_result <= 0.0001
pval_result[ind2] <- "<.0001"
odd_pval_result <- data.frame(
v1 = c("Method", "Wald", "midp", "Exact", 
"Wald", "midp", "Exact"), 
v2 = c("P-value", 
pval_result)
)  
odd_pval_result <- setNames(odd_pval_result, NULL)
labs = "Odds Ratio P-value Results"
f1 <- kableExtra :: kbl(odd_pval_result,
align = "l", caption = labs)

f2 <- kableExtra :: kable_paper(f1, "hover", full_width = F)

f3 <- kableExtra :: column_spec(f2, 1:2, 
 border_left = T, 
 border_right = T)

f4 <- kableExtra :: column_spec(f3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

f5 <- kableExtra :: row_spec(f4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

f6 <- kableExtra :: pack_rows(f5, "Reffrence = Column:1", 2, 4)

kableExtra :: pack_rows(f6, "Reffrence = Column:2", 5, 7) -> table6


## get relative risk Result

relativeRisk_Result <- data.frame(
v1 = 
c("Reffrence", rep("Reffrence = Column 1", 2), 
rep("Reffrence = Column 2", 2)), 

v2 = c("Method", "Wald", "boot",
"Wald", "boot"), 

v3 = c(
"Estimate", 
rr_wald_val, rr_boot_val, 
rr_wald_val_2, rr_boot_val_2
),

v4 = c("Lower bond, CI = 95%", 
rr_ci_wald[1], rr_ci_boot[1],  
rr_ci_wald_2[1], 
rr_ci_boot_2[1]), 

v5 =  c("Upper bond level, CI = 95%", 
rr_ci_wald[2], rr_ci_boot[2],  
rr_ci_wald_2[2], 
rr_ci_boot_2[2])
) 

relativeRisk_Result <- setNames(relativeRisk_Result, NULL)

g1 <- kableExtra :: kbl(relativeRisk_Result[, -1], 
caption = "Relative Risk Result", 
align = "l")

g2 <- kableExtra :: kable_paper(g1, "hover", full_width = F)


g3 <- kableExtra ::  column_spec(g2, 1:4, 
 border_left = T, 
 border_right = T)
 
 g4 <- kableExtra :: column_spec(g3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)
 
g5 <- kableExtra :: row_spec(g4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

g6 <- kableExtra :: pack_rows(g5, "Reffrence = Column:1", 2, 3) 

kableExtra :: pack_rows(g6, "Reffrence = Column:2", 4, 5) -> table7


## Relative Risk pvalue


rr_pval <- c(rr_wald_pval, rr_exact_pval, 
rr_wald_pval_2, rr_exact_pval_2)

ind3 <- rr_pval <= 0.0001
rr_pval[ind3] <- "<.0001"

rr_pvalue_result <- data.frame(
v1 = c("Method", "Wald", "Exact", 
"Wald", "Exact"), 
v2 = c("P-value", 
rr_pval)
)  


rr_pvalue_result <- setNames(rr_pvalue_result, NULL)



labs <- "Relative Risk P-value Results"
h1 <- kableExtra :: kbl(rr_pvalue_result, caption = labs, 
align = "l") 

h2 <- kableExtra :: kable_paper(h1, "hover", full_width = F)
h3 <- kableExtra ::  column_spec(h2, 1:2, 
 border_left = T, 
 border_right = T)
 h4 <- kableExtra :: column_spec(h3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

h5 <- kableExtra :: row_spec(h4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T) 

h6 <- kableExtra :: pack_rows(h5, "Reffrence = Column:1", 2, 3)
kableExtra :: pack_rows(h6, "Reffrence = Column:2", 4, 5) -> table8


## merge table

if(show_table_results){
print(htmltools :: browsable(
htmltools :: tagList(htmltools :: HTML(table1), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(), 

htmltools :: HTML(table2), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table3), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table4), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table5), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table6), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table7), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),

htmltools :: HTML(table8), 
htmltools :: br(), htmltools :: br(),  htmltools :: br(),
)))
}

Table_results <- list(Contigency_table = table1, 
chi_table = table2, 
fisher_table = table3, Corr_table = table4, 
Odds_ratio_table = table5, 
Pvalue_Oddratio_table = table6, 
Relative_Risk_table = table7, 
relativeRisk_Pvalue_table = table8
)

stat_R_results = list(Contigency_results = freq_table, 
chi_square_result = stat_table, 
fisher_results = fisher_exact_test, 
corr_coef_results = Coef_table, 
oddsRatio_results = oddsRatio_results, 
odd_pvalue_result = odd_pval_result, 
relativeRisk_Result = relativeRisk_Result, 
Relative_risk_pvalue_result = rr_pvalue_result
)

Total_result = list(Tables = Table_results, 
stat_output = stat_R_results)

return(Total_result)
}



#' create a function to create original data from a table 2x2 
#' 
#' @usage \code{create_dat_two(tab, name1, name2)}
#' @param tab contigency table 2x2
#' @param name1 A string that name of first variable into table
#' @param name2 A string that name of second variable into table
#' @return res a dataframe that has two column
#'     which column 1 is first variable and column 2 is second variable
#' @export
#' @examples
#' \dontrun{
#' create_dat_two(mytable, "Expose", "Disease")
#'}
create_dat_two <- function(tab, name1, 
name2){
n <- sum(tab)

n11 <- tab[1, 1]; n12 = tab[1, 2]; 
n21 = tab[2, 1]; n22 = tab[2, 2]
x <- rep(c(1, 1, 0, 0),  c(n11, n12, n21, n22))
y <- rep(c(1, 0, 1, 0), c(n11, n12, n21, n22))

as.data.frame(cbind(x, y)) -> res
names(res) <- c(name1, name2)
return(res)
}

#' for get oddsRatio based on Column 1, Column 2 from a 
#' contigency table
#' 
#' @usage \code{odr(n11, n12, n21, varname1 = "Expose", varname2 = "Disease", 
#'     levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", 
#'     "UnDisease"), method = "wald", conf_level = 0.95, 
#'     show_table_result = TRUE))}
#' @param n11 seealso \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @param method The odds ratio estimation method has three state \code{`"midp"`, 
#'     `"wald"`, `"exact"`} 
#' @param conf_level level of confidence Interval
#' @param show_table_result see also \code{\link{get_contigency_result}}
#' @return two table of oddsratio results, a html table and a r table
#' @export 
#' @examples 
#'\dontrun{
#' odr(475, 461, 7, 61, "Expose", "Disease", c("Exposed", "UnExposed"), c("Disease", "UnDisease"), method = "wald", conf_level = 0.95, show_table_result = TRUE)
#'}
odr <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Disease", 
levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
method = "wald", conf_level = 0.95, show_table_result = TRUE){


## introduction for preparation data

dat <- matrix(c(n11, n21, n12, n22), 2, 2) 
dimnames(dat) <- list(rows = levels_var1,
columns = levels_var2)
dat_tab <- as.table(dat)
Dat <- create_dat_two(dat_tab, varname1, varname2)

tabd <- table(Dat)



#### ref = column 1

## get result 
ord <- epitools :: oddsratio(tabd, 
method = ifelse(method == "exact", "fisher", method), 
conf.level = conf_level)
ord_val <- round(ord$measure[2, 1], 4)

## confint 

ord_ci <- round(ord$measure[2, 2:3], 4)

## ord pval 

ind_pval <- switch(method, 
"exact" = 2, 
"midp" = 1, 
3)

round(ord$p.value[2, ind_pval], 4) -> ord_pval

#### ref = column 2

## ord2 
ord_2 <- epitools :: oddsratio(tabd[, 2:1], 
method = ifelse(method == "exact", 
"fisher", method), conf.level = conf_level)

ord_val_2 <- round(ord_2$measure[2, 1], 4)



## confint ord2

ord_ci_2 <- round(ord_2$measure[2, 2:3], 4)


## ord2  pval 

round(ord_2$p.value[2, ind_pval], 4) -> ord_pval_2

## create result 

odd_result <- data.frame(
Reffrence = c("Column:1", "Column:2"), 
Method = c(method, method), 
estimate = c(ord_val, ord_val_2), 
"p-value" = c(ord_pval, ord_pval_2), 
`CI-Lower` = c(ord_ci[1], ord_ci_2[1]), 
`CI-Upper` = c(ord_ci[2], ord_ci_2[2])) 


odd_result2 <- odd_result
odd_result$p.value <- kableExtra :: cell_spec(odd_result$p.value,
background = ifelse(odd_result$p.value > 0.05, 
"green", "red"), color = "white", 
bold = T)


foot_note1 <- paste("CI = ", 100 * conf_level, 
"%", sep = "")

a1 <- kableExtra :: kbl(odd_result, 
caption = "Table of Oddratio Results", escape = F) 
a2 <- kableExtra :: kable_paper(a1, "hover")
a3 <- kableExtra :: column_spec(a2, 1, background = "green", 
color = "white", 
italic = T, bold = T)
a4 <- kableExtra :: row_spec(a3, 0, background = "green", 
color = "white", italic = T, bold = T)

kableExtra :: footnote(a4, general = foot_note1,
           number = c("if pvalue cell is red, means that p-value < 0.05"),
           ) -> Table_result 


Return_result <- list(odd_ratio_result = odd_result2, 
Table = Table_result)
return(Return_result)
}

#'  define function for get relative risk results 
#' 
#' @usage \code{rr(n11, n12, n21, n22, varname1 = "Expose", 
#'     varname2 = "Disease", levels_var1 = c("Exposed", 
#'     "UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
#'     method = "wald", conf_level = 0.95)}
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @param method It has two modes, ‍‍‍`"wald"` and `"boot"`, which is the 
#'     `"boot"` mode based on resampling.
#' @param nboot when \code{method = "boot"} therefore nboot is number of
#'     replicates that make resampling. \href{https://uc-r.github.io/resampling_methods}{resamplingMethods}.
#' @return two table for RiskRatio results.
#' @export
#' @examples 
#' \dontrun{rr(475, 461, 7, 61, "Expose", "Disease", c("Exposed", "UnExposed"), c("Disease", "UnDisease"), method = "boot", conf_level = 0.95, nboot = 1000)}
rr <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
"UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
method = "wald", conf_level = 0.95, 
nboot = 1000){

## introduction for preparation data

dat <- matrix(c(n11, n21, n12, n22), 2, 2) 
dimnames(dat) <- list(rows = levels_var1,
columns = levels_var2)
dat_tab <- as.table(dat)
Dat <- create_dat_two(dat_tab, varname1, varname2)

tabd <- table(Dat)


## get rr column 1
rr1 <- epitools :: riskratio(tabd, method = method, 
conf.level = conf_level, 
replicates = ifelse(method == "boot", 
nboot, NULL))


round(rr1$measure[2, 1], 4) -> rr_1_val

## rr2 ci

rr_ci <- round(rr1$measure[2, 2:3], 4)

## rr1 pval
round(rr1$p.value[2, 3], 4) -> rr1_wald_pval


## rr1 pval exact 

round(rr1$p.value[2, 2], 4) -> rr_exact_pval


# get rr for column 2

rr2 <- epitools :: riskratio(tabd[, 2:1], method = method, 
conf.level = conf_level, 
replicates = ifelse(method == "boot", 
nboot, NULL))


round(rr2$measure[2, 1], 4) -> rr_2_val

## rr2  ci

rr_ci_2 <- round(rr2$measure[2, 2:3], 4)

## rr2 wald pval
round(rr2$p.value[2, 3], 4) -> rr2_wald_pval


## rr2 pval exact 

round(rr2$p.value[2, 2], 4) -> rr2_exact_pval

## create result 

riskratio_result <- data.frame(
Reffrence = c("Column:1", "Column:2"), 
Method = c(method, method), 
estimate = c(rr_1_val, rr_2_val), 
pvalue_exatMethod = c(rr_exact_pval, rr2_exact_pval), 
pvalue_waldMethod = c(rr1_wald_pval, rr2_wald_pval), 
`CI-Lower` = c(rr_ci[1], rr_ci_2[1]), 
`CI-Upper` = c(rr_ci[2], rr_ci_2[2])) 

riskratio_result2 <- riskratio_result
riskratio_result$pvalue_exatMethod <- kableExtra :: cell_spec(
riskratio_result$pvalue_exatMethod,
background = ifelse(riskratio_result$pvalue_exatMethod > 0.05, 
"green", "red"), color = "white", 
bold = T)

riskratio_result$pvalue_waldMethod <- kableExtra :: cell_spec(
riskratio_result$pvalue_waldMethod,
background = ifelse(riskratio_result$pvalue_waldMethod > 0.05, 
"green", "red"), color = "white", 
bold = T)

foot_note1 <- paste("CI = ", 100 * conf_level, 
"%", sep = "")

labs <- "Table of RiskRatio Results"
a1 <- kableExtra :: kbl(riskratio_result, caption = labs, escape = F)

a2 <- kableExtra :: kable_paper(a1, "hover") 

a3 <- kableExtra :: column_spec(a2, 1, background = "green", 
color = "white", 
italic = T, bold = T)

a4 <- kableExtra :: row_spec(a3, 0, background = "green", 
color = "white", italic = T, bold = T)

kableExtra :: footnote(a4, general = foot_note1,
           number = c("if pvalue cell is red, means that p-value < 0.05"),
           ) -> Table_result 


Return_result <- list(RiskRatio_results = riskratio_result2, 
Table = Table_result)
return(Return_result)
}

#' define function for get Lambda coefficients 
#' 
#' @usage \code{lambda_coef_contigency(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Disease", levels_var1 = c("Exposed", 
#'     "UnExposed"), levels_var2 = c("Dieseae", "UnDisease"))}
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @return table of lambda result, for more detail of what is lambda 
#'     \href{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details115.htm}{SAS_help}
#' @export
#' @examples 
#' \dontrun{lambda_coef_contigency(475, 461, 7, 61, "Expose", "Disease", levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", "UnDisease"))}
lambda_coef_contigency <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
"UnExposed"), levels_var2 = c("Disease", "UnDisease")){

# introduction prepare data

dat <- matrix(c(n11, n21, n12, n22), 2, 2) 
dimnames(dat) <- list(rows = levels_var1,
columns = levels_var2)
dat_tab <- as.table(dat)
Dat <- create_dat_two(dat_tab, varname1, varname2)

tabd <- table(Dat)


## lambda assymetric C|R
tabd <- table(Dat)

v1 <- max(tabd[1, ])
v2 <- max(tabd[2, ])
v <- max(colSums(tabd))
w1 <- max(tabd[, 1])
w2 <- max(tabd[, 2])
w <- max(rowSums(tabd))
n <- sum(tabd)
lam_c_r <- (v1 + v2 - v)/(n-v)

lam_c_r <- round(lam_c_r, 4)
## get lambda C|R
Dat2 <- Dat[, 2:1]
tabd2 <- table(Dat2)
v1_2 <- max(tabd2[1, ])
v2_2 <- max(tabd2[2, ])
v_2 <- max(colSums(tabd2))
w1_2 <- max(tabd2[, 1])
w2_2 <- max(tabd2[, 2])
w_2 <- max(rowSums(tabd2))
lam_r_c <- (v1_2 + v2_2 - v_2)/(n-v_2) 
lam_r_c <- round(lam_r_c, 4)

## get lambda Symmetric

lam <- (v1 + v2 + w1 + w2
-v - w)/(2*n - v - w) 
lam <- round(lam, 4)

result <- data.frame(
`Lambda Asym C|R` = lam_c_r, 
`Lambda Asym R|C` = lam_r_c, 
`Lambda Symmetric` = lam
)
labs = paste("Lambda Coefficient for", varname1, "by", varname2, sep = " ")

a1 <- kableExtra :: kbl(result, caption = labs) 

a2 <- kableExtra :: kable_paper(a1, "hover") 

kableExtra :: row_spec(a2, 0, 
background = "green", color = "white") -> table_lam

return(list(Result = result, Table = table_lam))
}


#' Uncertainty coefficient function 
#' 
#' @usage \code{uncerainty_get(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Disease", levels_var1 = c("Exposed", 
#'     "UnExposed"), levels_var2 = c("Dieseae", "UnDisease"))}
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @return table of uncertainty coefficienty results, 
#'     for more detail of what is uncertainty coefficienty see 
#'     \href{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details115.htm}{SAS_help}
#' @export
#' @examples 
#' \dontrun{uncertainty_get(475, 461, 7, 61, "Expose", "Disease", levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", "UnDisease"))}
#' @export
uncertainty_get <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
"UnExposed"), levels_var2 = c("Disease", "UnDisease")){

# introduction prepare data

dat <- matrix(c(n11, n21, n12, n22), 2, 2) 
dimnames(dat) <- list(rows = levels_var1,
columns = levels_var2)
dat_tab <- as.table(dat)
Dat <- create_dat_two(dat_tab, varname1, varname2)

tabd <- table(Dat)

## Uncertainty C|R
n <- sum(tabd)
hx <- -sum(rowSums(tabd)/n *log(rowSums(tabd)/n))
hy <- -sum(colSums(tabd)/n * log(colSums(tabd)/n))
hxy <- -sum(tabd/n * log(tabd/n))
hv <- hx + hy - hxy
unc_c_r <- hv/hy 
unc_c_r <- round(unc_c_r, 4)

## Uncertainty R|C
Dat2 <- Dat[, 2:1]
tabd2 <- table(Dat2)
hx_2 <- -sum(rowSums(tabd2)/n *log(rowSums(tabd2)/n))
hy_2 <- -sum(colSums(tabd2)/n * log(colSums(tabd2)/n))
hxy_2 <- -sum(tabd2/n * log(tabd2/n))
hv_2 <- hx_2 + hy_2 - hxy_2
unc_r_c <- hv_2/hy_2 
unc_r_c <- round(unc_r_c, 4)

## Uncertainty sym

unc_sym <- 2 * (hx + hy - hxy)/(hx + hy)
unc_sym <- round(unc_sym, 4)


result <- data.frame(
v1 = c("Uncertainty Coefficient C|R", 
"Uncertainty Coefficient R|C", 
"Uncertainty Coefficient Symm"),
v2 = c(unc_c_r, unc_r_c, unc_sym)
)

result <- as.data.frame(t(result))
result <- setNames(result, NULL) 
labs = paste("Uncertainty Coefficient for", varname1, "by", varname2, 
sep = " ")
rownames(result) = NULL
a1 <- kableExtra :: kbl(result, caption = labs)
a2 <- kableExtra :: kable_paper(a1, "hover") 
kableExtra :: row_spec(a2, 1, 
background = "green", color = "white") -> table_unc

return(list(Result = result, Table = table_unc))
}


#' this function created for get mantel-haenszel and test homogenty of OR 
#' @import import
#' @importFrom magrittr %>% 
#' @usage \code{homogenity_test_or(x, partial_oddsratio_method = "wald",
#'     confront_var = "age")}
#' @param x is array with atleast 3 dimension
#' @param partial_oddsratio_method  method The odds ratio estimation method has three state \code{`"midp"`, 
#' `"wald"`, `"exact"`} 
#' @param confront_var confounding variable is factor variable
#' @return odd_ratio_result result of oddratio into table
#' @return test_result resut results
#' @return  tabe_test t table 
#' @examples 
#' \dontrun{homogenity_test_or <- function(x, partial_oddsratio_method = "wald", confront_var = "age")}
#' @export
homogenity_test_or <- function(x, partial_oddsratio_method = "wald", 
confront_var = "age"){
    dim_res <- dim(x)
    n <-dim_res[3]
    Type <- partial_oddsratio_method
    Partial_tabs <- list()
    for(i in 1:n){
        Partial_tabs[[i]] <- x[, , i]
    }
    odd_result <- matrix(NA, n, 3)
    for(j in 1:n){
        dati <- Partial_tabs[[j]]
        odd_result[j, ] <- epitools :: oddsratio(dati, method = Type)$measure[2, ]
    }
    ## odd_result

    Marginal_tab <- margin.table(x, c(1, 2))
    odd_crude <- epitools :: oddsratio(Marginal_tab, method = Type)$measure[2, ]
    test_mantel <- mantelhaen.test(x)
    MH_odd_combined <- c(test_mantel$estimate, test_mantel$conf.int)
    odd_result <- rbind(odd_result, odd_crude, MH_odd_combined) %>%
    as.data.frame %>% setNames(c("statistic", "lower-band", "upper-band"))
    rownames(odd_result) <- NULL
    Nam <- c(paste0(paste0("level ", 1:n), paste0(" of ", confront_var)),
     "Marginal OR", 
    "MH OR")
    odd_result <- odd_result %>%
    dplyr :: mutate(varname = Nam) %>%
    dplyr :: relocate("varname", .before = "statistic")
    rownames(odd_result) <- NULL


    ######## get test_result
    test_mantel <- mantelhaen.test(dat)
    test_bres <- DescTools :: BreslowDayTest(dat)
    vcd :: woolf_test(dat) -> test_woolf
    mat_test <- matrix(NA, 3, 2)

    MH_res <- c(test_mantel$statistic, test_mantel$p.value) %>% round(4)
    c(test_bres$statistic, test_bres$p.value) %>% round(4) -> Breslow_res
    c(test_woolf$statistic, test_woolf$p.value) %>% round(4) -> woolf_res
    h_test <- rbind(MH_res, Breslow_res, woolf_res) %>% 
    as.data.frame %>% setNames(c("statistic", "p-value")) %>%
    tibble :: rownames_to_column(var = "Method")
    list_result <- list(oddratio_rsult = odd_result, test_result = h_test)

    OR_table <- odd_result %>%
    kableExtra :: kbl(caption = "OddsRatio Results", align = "c") %>%
    kableExtra :: kable_paper("hover", full_width = F) %>%
    kableExtra :: pack_rows(paste0("levels of ", confront_var), 1, n) %>%
    kableExtra :: pack_rows("Marginal OR, Mantel-Haenszel OR", n+1, n+2) %>%
        kableExtra :: column_spec(1, background = "green", 
    color = "white", 
    italic = T, bold = T) %>%
    kableExtra :: row_spec(0, background = "green", 
    color = "white", italic = T, bold = T)

    test_table <- h_test %>%
    kableExtra :: kbl(caption = "test Results", align = "c") %>%
    kableExtra :: kable_paper("hover") %>%
    kableExtra :: column_spec(1, background = "green", 
    color = "white", 
    italic = T, bold = T) %>%
    kableExtra :: row_spec(0, background = "green", 
    color = "white", italic = T, bold = T)
    tables <- list(OR_table, test_table)
    return(list(oddratio_rsult = odd_result, test_result = h_test, 
    table_Oddsratio = OR_table, table_test = test_table))
}



#' table_1 contigency table with 3 variables
#'
#' A dataset containing a contigency table with 3 variable 
#' The variables are as follows:
#'
#' \itemize{
#'   \item exposure. The variable that shows how many were exposed, which is a binary variable with two levels of exposure (1) or no exposure (0).
#'   \item Group. A binary variable that is leveled at the level of the treated group (1) and the control group (0).
#' \item age. A categorical variable, which is divided into three levels: 1, 2, and 3.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name table_1
#' @usage data(table_1)
#' @format contigency table with 3 variables
NULL
