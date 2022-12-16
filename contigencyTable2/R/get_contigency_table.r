#' @importFrom stats dhyper cor fisher.test chisq.test mantelhaen.test pchisq setNames pnorm qnorm qf var.test
#' @importFrom utils installed.packages data
#' @importFrom magrittr %>% 
#' @importFrom Hmisc somers2 rcorr.cens
#' @importFrom epitools riskratio expected oddsratio riskratio.boot
#' @importFrom htmltools br HTML tagList browsable
#' @importFrom kableExtra group_rows footnote cell_spec kbl pack_rows row_spec column_spec kable_paper
#' @importFrom vcd assocstats woolf_test
#' @importFrom DescTools BreslowDayTest KendallTauA StuartTauC KendallTauB MHChisqTest
#' @importFrom dplyr relocate mutate group_by
#' @importFrom tibble rownames_to_column
#' @importFrom ggforce geom_ellipse
#' @importFrom rstatix get_summary_stats t_test
#' @importFrom ggplot2 geom_abline coord_fixed coord_cartesian geom_segment 
#' @importFrom ggplot2 geom_point element_blank theme labs theme_bw annotate 
#' @importFrom ggplot2 geom_errorbar geom_vline aes ggplot 
#' @importFrom grid arrow unit
#' 
NULL
#> NULL


#' Function to create a complete table results for contigency table
#' 
#' 
#' @usage get_contigency_result(n11, n12, n21, n22,
#'     varname1 = "Expose", varname2 = "Disease",
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"), show_table_results = TRUE)
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
#' @return Table_results A list containing 8 output tables in html format, 
#'     showing the outputs for each table.
#' @return stat_R_results list of 8 table as dataframe format for show
#'     result of table that generate from contigency table.
#' 
#' @export
#' @examples
#'\dontrun{get_contigency_result(
#'     n11 = 475, n12 = 461, n21 = 7, n22 = 61, 
#'     varname1 = "Expose", varname2 = "Disease",
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"), 
#'     show_table_results = TRUE)}
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

ex_tab <- expected(dat_tab) 
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


a1 <- kbl(freq_table[, -1], 
caption = lab1, align = "r")

a2 <- kable_paper(a1, "hover", full_width = F)

a3 <- column_spec(a2, 1, border_left = T, border_right = T)

a4 <- column_spec(a3, 1:2, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

a5 <- row_spec(a4, 1:3, 
color = "#1c0345", 
background = "#d4ecf65e", 
hline_after = T, 
bold = T, 
italic = T)
a6 <- pack_rows(a5, levels_var1[1], 4, 8)

a7 <- pack_rows(a6, levels_var1[2], 9, 13) -> table1

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
Mantel <- MHChisqTest(dat_tab)
mstat <- Mantel$statistic
mstat <- round(mstat, 4)
p_mantel <- Mantel$p.value
p_mantel <- round(p_mantel, 4)


## likelihood Ratio chi-square

like <- assocstats(dat_tab)

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

b1 <- kbl(stat_table, caption = lab2, escape = F)

b2 <- kable_paper(b1, "hover", full_width = F)

b3 <- column_spec(b2, 1, 
 border_left = T, 
 border_right = T)

table2 <- row_spec(b3, 1, 
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

c1 <- kbl(fisher_exact_test, caption = "Fisher's Exact Test")

c2 <- kable_paper(c1, "hover", full_width = F)

c3 <- column_spec(c2, 1, 
 border_left = T, 
 border_right = T)

table3 <- column_spec(c3, 1, color = "#1c0345", 
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

gama_cor <- rcorr.cens(Dat[[varname1]], Dat[[varname2]], 
outx = TRUE)[2]
round(gama_cor, 4) -> gama_cor
## kendal and pearson and spearman correlation

pears <- round(cor(Dat[[varname1]], Dat[[varname2]], method = "pearson"), 4)
spear <- round(cor(Dat[[varname1]], Dat[[varname2]], method = "spearman"), 4)

kend_B <- round(KendallTauB(Dat[[varname1]], Dat[[varname2]]), 4)
kend_C <- round(StuartTauC(Dat[[varname1]], Dat[[varname2]]), 4)
kend_A <- round(KendallTauA(Dat[[varname1]], Dat[[varname2]]), 4)

somers_r_c <- round(somers2(Dat[[varname1]], 
Dat[[varname2]])[2], 4)
somers_c_r <- round(somers2(Dat[[varname2]], 
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
d1 <- kbl(Coef_table, caption = labs)

d2 <- kable_paper(d1, "hover", full_width = F)

d3 <- column_spec(d2, 1, 
 border_left = T, 
 border_right = T)

table4 <- column_spec(d3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T) 
############## Odds Ratio

#### ref = column 1

## or wald 
or_wald <- oddsratio(tabd, method = "wald")
or_wald_val <- round(or_wald$measure[2, 1], 4)

## or midp 

or_midp <- oddsratio(tabd, method = "midp")
or_midp_val <- round(or_midp$measure[2, 1], 4) 

## or exact
or_exact <- oddsratio(tabd, method = "fisher")
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
or_wald_2 <- oddsratio(tabd[, 2:1], method = "wald")
or_wald_val_2 <- round(or_wald_2$measure[2, 1], 4)


## or2 midp 

or_midp_2 <- oddsratio(tabd[, 2:1], method = "midp")
or_midp_val_2 <- round(or_midp_2$measure[2, 1], 4)

## or exact
or_exact_2 <- oddsratio(tabd[, 2:1], method = "fisher")
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
rr_wald <- riskratio(tabd, method = "wald")
round(rr_wald$measure[2, 1], 4) -> rr_wald_val
 
## rr boot

rr_boot <- riskratio.boot(tabd, replicates = 5000)
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
rr_wald_2 <- riskratio(tabd[, 2:1], method = "wald")
round(rr_wald_2$measure[2, 1], 4) -> rr_wald_val_2

## rr2 boot

rr_boot_2 <- riskratio.boot(tabd[, 2:1], replicates = 5000)
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

e1 <- kbl(oddsRatio_results[, -1], 
caption = "Odds Ratio Results", align = "l") 

e2 <- kable_paper(e1, "hover", full_width = F)


e3 <- column_spec(e2, 1:4, 
 border_left = T, 
 border_right = T)


e4 <- column_spec(e3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)
e5 <- row_spec(e4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

e6 <- pack_rows(e5, "Reffrence = Column:1", 2, 4) 
pack_rows(e6, "Reffrence = Column:2", 5, 7) -> table5


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
f1 <- kbl(odd_pval_result,
align = "l", caption = labs)

f2 <- kable_paper(f1, "hover", full_width = F)

f3 <- column_spec(f2, 1:2, 
 border_left = T, 
 border_right = T)

f4 <- column_spec(f3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

f5 <- row_spec(f4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

f6 <- pack_rows(f5, "Reffrence = Column:1", 2, 4)

pack_rows(f6, "Reffrence = Column:2", 5, 7) -> table6


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

g1 <- kbl(relativeRisk_Result[, -1], 
caption = "Relative Risk Result", 
align = "l")

g2 <- kable_paper(g1, "hover", full_width = F)


g3 <- column_spec(g2, 1:4, 
 border_left = T, 
 border_right = T)
 
 g4 <- column_spec(g3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)
 
g5 <- row_spec(g4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

g6 <- pack_rows(g5, "Reffrence = Column:1", 2, 3) 

pack_rows(g6, "Reffrence = Column:2", 4, 5) -> table7


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
h1 <- kbl(rr_pvalue_result, caption = labs, 
align = "l") 

h2 <- kable_paper(h1, "hover", full_width = F)
h3 <- column_spec(h2, 1:2, 
 border_left = T, 
 border_right = T)
 h4 <- column_spec(h3, 1, color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T)

h5 <- row_spec(h4, 1,  color = "#1c0345", 
background = "#d4ecf65e", 
italic = T, bold = T) 

h6 <- pack_rows(h5, "Reffrence = Column:1", 2, 3)
pack_rows(h6, "Reffrence = Column:2", 4, 5) -> table8


## merge table

if(show_table_results){
print(browsable(
tagList(HTML(table1), 
br(), br(), br(), 
HTML(table2), 
br(), br(), br(),
HTML(table3), 
br(), br(), br(),
HTML(table4), 
br(), br(), br(),
HTML(table5), 
br(), br(), br(),
HTML(table6), 
br(), br(), br(),
HTML(table7), 
br(), br(), br(),
HTML(table8), 
br(), br(), br(),
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
#' @usage create_dat_two(tab, name1, name2)
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
#' @usage odr(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Disease", 
#'     levels_var1 = c("Exposed", "UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
#'     method = "wald", conf_level = 0.95, show_table_result = TRUE)
#' @param n11 see \code{\link{get_contigency_result}}
#' @param n12 see \code{\link{get_contigency_result}}
#' @param n21 see \code{\link{get_contigency_result}}
#' @param n22 see \code{\link{get_contigency_result}}
#' @param varname1 see \code{\link{get_contigency_result}}
#' @param varname2 see \code{\link{get_contigency_result}}
#' @param levels_var1 see \code{\link{get_contigency_result}}
#' @param levels_var2 see \code{\link{get_contigency_result}}
#' @param method The odds ratio estimation method has three state \code{"midp", "wald", "exact"} 
#' @param conf_level level of confidence Interval
#' @param show_table_result see also \code{\link{get_contigency_result}}
#' @return two table of oddsratio results, a html table and a r table
#' @export 
#' @examples 
#'\dontrun{
#'     odr(n11 = 475, n12 = 461, n21 = 7, n22 = 61, varname1 = "Expose", 
#'     varname2 = "Disease", levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"), 
#'     method = "wald", conf_level = 0.95, show_table_result = TRUE)
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
ord <- oddsratio(tabd, 
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
ord_2 <- oddsratio(tabd[, 2:1], 
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
odd_result$p.value <- cell_spec(odd_result$p.value,
background = ifelse(odd_result$p.value > 0.05, 
"green", "red"), color = "white", 
bold = T)


foot_note1 <- paste("CI = ", 100 * conf_level, 
"%", sep = "")

a1 <- kbl(odd_result, 
caption = "Table of Oddratio Results", escape = F) 
a2 <- kable_paper(a1, "hover")
a3 <- column_spec(a2, 1, background = "green", 
color = "white", 
italic = T, bold = T)
a4 <- row_spec(a3, 0, background = "green", 
color = "white", italic = T, bold = T)

footnote(a4, general = foot_note1,
           number = c("if pvalue cell is red, means that p-value < 0.05"),
           ) -> Table_result 


Return_result <- list(odd_ratio_result = odd_result2, 
Table = Table_result)
return(Return_result)
}

#'  define function for get relative risk results 
#' 
#' @usage rr(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
#'     "UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
#'     method = "wald", conf_level = 0.95, nboot = 1000)
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @param method It has two modes: \code{"wald", "boot"}
#'     which is the \code{"boot"} mode based on resampling Method.
#' @param conf_level see \code{\link{odr}}
#' @param nboot when \code{method = "boot"} therefore nboot is number of
#'     replicates that make resampling. \href{https://uc-r.github.io/resampling_methods}{resamplingMethods}.
#' @return two table for RiskRatio results.
#' @export
#' @examples 
#' \dontrun{rr(475, 461, 7, 61, "Expose", "Disease", c("Exposed", "UnExposed"), 
#'     c("Disease", "UnDisease"), method = "boot", conf_level = 0.95, nboot = 1000)}
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
rr1 <- riskratio(tabd, method = method, 
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

rr2 <- riskratio(tabd[, 2:1], method = method, 
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
riskratio_result$pvalue_exatMethod <- cell_spec(
riskratio_result$pvalue_exatMethod,
background = ifelse(riskratio_result$pvalue_exatMethod > 0.05, 
"green", "red"), color = "white", 
bold = T)

riskratio_result$pvalue_waldMethod <- cell_spec(
riskratio_result$pvalue_waldMethod,
background = ifelse(riskratio_result$pvalue_waldMethod > 0.05, 
"green", "red"), color = "white", 
bold = T)

foot_note1 <- paste("CI = ", 100 * conf_level, 
"%", sep = "")

labs <- "Table of RiskRatio Results"
a1 <- kbl(riskratio_result, caption = labs, escape = F)

a2 <- kable_paper(a1, "hover") 

a3 <- column_spec(a2, 1, background = "green", 
color = "white", 
italic = T, bold = T)

a4 <- row_spec(a3, 0, background = "green", 
color = "white", italic = T, bold = T)

footnote(a4, general = foot_note1,
           number = c("if pvalue cell is red, means that p-value < 0.05"),
           ) -> Table_result 


Return_result <- list(RiskRatio_results = riskratio_result2, 
Table = Table_result)
return(Return_result)
}

#' define function for get Lambda coefficients 
#' 
#' @usage lambda_coef_contigency(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
#'     "UnExposed"), levels_var2 = c("Disease", "UnDisease"))
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @return table of lambda result, for more detail of what is lambda
#' @export
#' @examples 
#' \dontrun{lambda_coef_contigency(475, 461, 7, 61, "Expose", "Disease", 
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"))}
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

a1 <- kbl(result, caption = labs) 

a2 <- kable_paper(a1, "hover") 

row_spec(a2, 0, 
background = "green", color = "white") -> table_lam

return(list(Result = result, Table = table_lam))
}


#' Uncertainty coefficient function 
#' 
#' @usage uncertainty_get(n11, n12, n21, n22, 
#'     varname1 = "Expose", varname2 = "Disease", 
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"))
#' @param n11 see also \code{\link{get_contigency_result}}
#' @param n12 see also \code{\link{get_contigency_result}}
#' @param n21 see also \code{\link{get_contigency_result}}
#' @param n22 see also \code{\link{get_contigency_result}}
#' @param varname1 see also \code{\link{get_contigency_result}}
#' @param varname2 see also \code{\link{get_contigency_result}}
#' @param levels_var1 see also \code{\link{get_contigency_result}}
#' @param levels_var2 see also \code{\link{get_contigency_result}}
#' @return table of uncertainty coefficienty results
#' @export
#' @examples 
#' \dontrun{uncertainty_get(n11 = 475, n12 = 461, 
#'     n21 = 7, n22 = 61, varname1 = "Expose", 
#'     varname2 = "Disease", 
#'     levels_var1 = c("Exposed", "UnExposed"), 
#'     levels_var2 = c("Disease", "UnDisease"))}
#' @export
uncertainty_get <- function(n11, n12, n21, n22, 
varname1 = "Expose", varname2 = "Disease", levels_var1 = c("Exposed", 
"UnExposed"), levels_var2 = c("Disease", "UnDisease")){

# prepare data

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
a1 <- kbl(result, caption = labs)
a2 <- kable_paper(a1, "hover") 
row_spec(a2, 1, 
background = "green", color = "white") -> table_unc

return(list(Result = result, Table = table_unc))
}


#' this function created for get mantel-haenszel and test homogenty of OR 
#' 
#' 
#'
#' @usage homogenity_test_or(x, partial_oddsratio_method = "wald",
#'     confront_var = "age")
#' @param x is array with Atleast 3 dimension
#' @param partial_oddsratio_method  method The odds ratio estimation method has three state \code{"midp", 
#' "wald", "exact"} 
#' @param confront_var confounding variable is A factor variable
#' @return odd_ratio_result result 
#' @return test_result resut results
#' @return tabe_test t table 
#' @examples 
#' \dontrun{homogenity_test_or(x, partial_oddsratio_method = "wald", confront_var = "age")}
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
        odd_result[j, ] <- oddsratio(dati, method = Type)$measure[2, ]
    }
    ## odd_result

    Marginal_tab <- margin.table(x, c(1, 2))
    odd_crude <- oddsratio(Marginal_tab, method = Type)$measure[2, ]
    test_mantel <- mantelhaen.test(x)
    MH_odd_combined <- c(test_mantel$estimate, test_mantel$conf.int)
    odd_result <- rbind(odd_result, odd_crude, MH_odd_combined) %>%
    as.data.frame %>% setNames(c("statistic", "lower-band", "upper-band"))
    rownames(odd_result) <- NULL
    Nam <- c(paste0(paste0("level ", 1:n), paste0(" of ", confront_var)),
     "Marginal OR", 
    "MH OR")
    odd_result <- odd_result %>%
    mutate(varname = Nam) %>%
    relocate("varname", .before = "statistic")
    rownames(odd_result) <- NULL


    ######## get test_result
    test_mantel <- mantelhaen.test(x)
    test_bres <- BreslowDayTest(x)
    woolf_test(x) -> test_woolf
    mat_test <- matrix(NA, 3, 2)

    MH_res <- c(test_mantel$statistic, test_mantel$p.value) %>% round(4)
    c(test_bres$statistic, test_bres$p.value) %>% round(4) -> Breslow_res
    c(test_woolf$statistic, test_woolf$p.value) %>% round(4) -> woolf_res
    h_test <- rbind(MH_res, Breslow_res, woolf_res) %>% 
    as.data.frame %>% setNames(c("statistic", "p-value")) %>%
    rownames_to_column(var = "Method")
    list_result <- list(oddratio_rsult = odd_result, test_result = h_test)

    OR_table <- odd_result %>%
    kbl(caption = "OddsRatio Results", align = "c") %>%
    kable_paper("hover", full_width = F) %>%
    pack_rows(paste0("levels of ", confront_var), 1, n) %>%
    pack_rows("Marginal OR, Mantel-Haenszel OR", n+1, n+2) %>%
    column_spec(1, background = "green", 
    color = "white", 
    italic = T, bold = T) %>%
    row_spec(0, background = "green", 
    color = "white", italic = T, bold = T)

    test_table <- h_test %>%
    kbl(caption = "test Results", align = "c") %>%
    kable_paper("hover") %>%
    column_spec(1, background = "green", 
    color = "white", 
    italic = T, bold = T) %>%
    row_spec(0, background = "green", 
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
#'     \item exposure: The variable that shows how many were exposed, 
#'         which is a binary variable with two levels of exposure (1) or no exposure (0).
#'     \item Group: A binary variable that is leveled at the level of 
#'         the treated group (1) and the control group (0).
#'     \item age: A categorical variable, which is divided into three levels: 1, 2, and 3.
#'}
#'
#' @docType data
#' @keywords datasets
#' @name table_1
#' @usage data(table_1)
#' @format contigency table with 3 variables
"table_1"


#' this function created for convert a list to dataframe, 
#'     List members must be vectors with equal number of members.
#' 
#' 
#' @usage list_to_dataframe(List)
#' @param List a list; List members must be vectors with equal number of members
#' @return a dataframe, A dataframe whose columns are members of the input list.
#' @examples 
#' \dontrun{
#'     List = list(a = c(1, 2, 3, 4), b = c("a", "b", "c", "d"))
#'     list_to_dataframe(List)}
#' @export
list_to_dataframe <- function(List){
Nam <- names(List)
n <- length(List)
N <- lengths(List)
unique(N) -> nn
if(length(nn) != 1) stop("every member of list most be same size")
Array <- matrix(NA, nn, n)
for(i in 1:n){
    Array[, i] <- List[[i]]
}
Nam2 <- names(List[[1]])
dimnames(Array) <- list(Nam2, Nam)
as.data.frame(Array) -> res
return(res)
}



#' This function is prepared to check whether the package is installed by 
#'     entering the name of a package as a string.
#' 
#' 
#' @usage check_package(pak)
#' @param pak name of package as string format
#' @return return a string ("this package is not installed") or a vector with two element, name and version of vector 
#' @examples 
#' \dontrun{
#'     pak <- "ggplot2"
#'     check_package(pak)}
#' @export
check_package <- function(pak){
my_paks <- installed.packages()
paks <- my_paks[, 1]
if(pak %in% paks){
version_paks <- my_paks[, 3]
ind <- which(paks == pak)
res = c(paks[ind], version_paks[ind])
names(res) = NULL
names(res) = c("package_name", "Version")
return(res)
}else return("this package is not installed")
}




#' This function is designed to implement Fisher's algorithm for exact testing in a 2x2 contigency table. 
#' Although the \code{\link[stats:fisher.test]{stats::fisher.test()}} function is a 
#' very fast and good function, this function is also suitable.
#' 
#' 
#' @usage h_fisher(tab, alternative = "two-sided")
#' @param tab contigency table \deqn{2 \times 2}
#' @param alternative argumment that can take 3 value ("two-sided", "less", "greater")
#' @return a vector with two element \code{"p-value", "p-table"} that, \code{"p-value" is} \deqn{p_{value}} of test and
#'     \code{p-table} is probablity of original table.
#'  
#' @examples 
#' \dontrun{tab2 <- matrix(c(1, 9, 11, 3), 2, 2,
#'     byrow = T)
#'     h_fisher(tab2, alternative = "two-sided")}
#' @export
h_fisher <- function(tab, alternative = "two-sided"){
    n11 <- tab[1, 1]
    n12 <- tab[1, 2]
    n21 <- tab[2, 1]
    n22 <- tab[2, 2]
    prob_tab <- dhyper(n11, n11 + n12, n21 + n22, n11 + n21)
    Mat_list1 <- list()
    for(k in 0:(n11 + n21)){
        Mat_list1[[k + 1]] <- matrix(c(k, n21 + n11 - k, n12 + n11 - k, n22 - n11 + k), 
        2, 2)
    }


### two-sided
get_proper_mat_two_sided <- function(M){
    indicate <- FALSE
    res_val1 <- sum(M < 0)
    if(!res_val1) {
        prob <- dhyper(M[1, 1], M[1, 1] + M[1, 2], M[2, 1] + M[2, 2],
         M[1, 1] + M[2, 1])
         if(prob <= prob_tab) indicate <- TRUE
    }
    return(indicate)
}
ind_proper_two_sided <- unlist(lapply(Mat_list1, get_proper_mat_two_sided))
ind_proper_two_sided <- which(ind_proper_two_sided)
proper_mat_two_sided <- Mat_list1[ind_proper_two_sided]



### less

get_proper_mat_less <- function(M){
    indicate <- FALSE
    res_val1 <- sum(M < 0)
    if(!res_val1) {
        if(M[1, 1] <= n11) indicate <- TRUE
    }
    return(indicate)
}
ind_proper_less <- unlist(lapply(Mat_list1, get_proper_mat_less))
ind_proper_less <- which(ind_proper_less)
proper_mat_less <- Mat_list1[ind_proper_less]

### greater

get_proper_mat_greater <- function(M){
    indicate <- FALSE
    res_val1 <- sum(M < 0)
    if(!res_val1) {
        if(M[1, 1] >= n11) indicate <- TRUE
    }
    return(indicate)
}
ind_proper_greater <- unlist(lapply(Mat_list1, get_proper_mat_greater))
ind_proper_greater <- which(ind_proper_greater)
proper_mat_greater <- Mat_list1[ind_proper_greater]


Mat_final <- switch(alternative, 
"two-sided" = proper_mat_two_sided, 
"less" = proper_mat_less, 
proper_mat_greater)

get_pval <- function(M){
    mm <- n11 + n12
    nn <- n21 + n22
    a <- M[1, 1]
    b <- M[2, 1]
    k <- a + b
    return(dhyper(a, mm, nn, k))
}
pval_total <- unlist(lapply(Mat_final, get_pval))
data.frame("p-vlaue" = round(sum(pval_total), 4), "prob-table" = round(prob_tab, 4)) -> result_
result_2 <- result_
result_2$p.vlaue <- cell_spec(result_2$p.vlaue, 
background = ifelse(result_2$p.vlaue > 0.05, "green", "red"), color = "white", bold = T)
result_2 %>%
kbl(caption = "Table of Fisher Test for Contigency Table 2x2", escape = F) %>%
kable_paper("hover", full_width = F) %>%
row_spec(0, background = "green", color = "white", bold = T, italic = T) %>%
footnote(general = "if pvalue cell is red, means that p-value < 0.05") -> table_result
return(list(results = result_, table_result = table_result))
}







#' This function is designed so that, according to the user's request, 
#' from an Contigency table based on two variables, 
#' a data set with type; Create a matrix or dataframe or list.
#' 
#' 
#' @usage get_dat_from_tab(tab, Levels = NULL , idLevel = 0, data_type = "Matrix", 
#'     varnames = c("Var1", "Var2"))
#' @param tab contigency table based on Two Variables.
#' @param Levels A list with two members, the first member of the variable levels 
#'     that is distributed in the rows of the contigency 
#'     table and the second Member in its columns, 
#'     the default value is NULL. And level two and ... for two variables.
#' @param idLevel indicator variable, if the \code{Levels} argument is entered, 
#'     this argument must take the value 1, otherwise 0.
#' @param data_type According to the user's request, if you want the format of 
#'     the output data to be in the form of a matrix, 
#'     the value of the \code{"Matrix"} is entered, for 
#'     the dataframe, \code{"dataframe"} and for the list entered \code{"list"}.
#' @param varnames A vector with two members, which are the names of the 
#'     first variable (the variable whose levels are distributed in the 
#'     rows of the contigency table) and the second.
#' @return The output is a list with two members, input table (\code{original_table}) and dataset (\code{Data}).
#'  
#' @examples 
#' \dontrun{data(table_2)
#'     get_dat_from_tab(tab = table_2, data_type = "dataframe")}
#' @export
get_dat_from_tab <- function(tab, Levels = NULL, idLevel = 0, data_type = "Matrix", 
varnames = c("Var1", "Var2")){
    n1 <- nrow(tab)
    n2 <- ncol(tab)
    n <- sum(tab)
    N1 <- rowSums(tab)
    N2 <- colSums(tab)
    f1 <- function(x) {
        dimnames(x) = list(1:n, varnames)
        return(x)
    }
    f2 <- function(x) {
        names(x) <- varnames
        return(x)
    }

    if(idLevel){
        lev_1 <- Levels[[1]]
        lev_2 <- Levels[[2]]
        x1 <- rep(lev_1, N1)
        x2 <- rep(rep(lev_2, times = n1), times = as.vector(t(tab)))
        dat1 <- cbind(x1, x2)
        dat2 <- data.frame(x1, x2)
        dat3 <- list(x1, x2)
        Dat <- switch(data_type, 
        "Matrix" = dat1, 
        "dataframe" = dat2, 
        dat3) 
        Dat <- switch(data_type, 
        "Matrix" = f1(Dat), 
        f2(Dat))
        return(list(Data = Dat, original_table = tab))
    }else{
        x1 <- rep(1:n1, N1)
        x2 <- rep(rep(1:n2, times = n1), times = as.vector(t(tab)))
        dat1 <- cbind(x1, x2)
        dat2 <- data.frame(x1, x2)
        dat3 <- list(x1, x2)
        Dat <- switch(data_type, 
        "Matrix" = dat1, 
        "dataframe" = dat2, 
        dat3) 
        Dat <- switch(data_type, 
        "Matrix" = f1(Dat), 
        f2(Dat))
        return(list(Data = Dat, original_table = tab))
}
}



#' table_2 contigency table with 2 variables
#'
#' A contigency table based on the number of case-control study for ovarian cancer patients 
#' and its association with contraceptive use and duration of use.
#'
#' \itemize{
#'     \item Disease: The variable that shows how many were Disease (case) on Not Disease (control), 
#'        which is a binary variable with two levels of Disease (case) or Not Disease (control).
#'     \item OC Duration time: How long the person in question has been using contraceptives.
#'        which has 4 levels, no use (\code{None}), between 0 and 5 years of use (\code{0-5}), 
#'        between 5 and 10 years of use (\code{50-10} and more than 10 years of use (\code{>10}).
#' }
#'
#' @docType data
#' @keywords datasets
#' @name table_2
#' @usage data(table_2)
#' @format contigency table with 2 variables
"table_2"


#' This function has been prepared for the purpose of performing three valid tests to check the connection 
#' or non-connection of the columns and rows of a contigency table and to output the 
#' test statistics as well as the expected values of the table and to check whether 
#' the exact test should also be performed or not. bring.
#' 
#' 
#' @usage Table_Test_Result(tab, Levels, idLevel = 0)
#' @param tab contigency table with two variable, that any variable have I (I >= 2) levels.
#' @param Levels see \code{\link{get_dat_from_tab}}
#' @param idLevel see \code{\link{get_dat_from_tab}}
#' @return ExpEcted_Vals table of expected values of a contigency table (tab)
#' @return test_result table of test results
#' @return Total_results table of Total results (expected values, test resutls and input table)
#' @return table_results html table for total results
#'
#' @details for calculate test statistics values, we use this formulas:
#' \deqn{
#' \text{Contigency Table} = \left[\begin{array}{c|c|c|c}n_{(1, ~1)} & n_{(1,~2)} & \cdots & n_{(1,~J)}\\
#'     n_{(2, ~1)} & n_{(2, ~2)} & \cdots & n_{(2, ~J)} \\
#'     \vdots & \ddots & \ddots & \vdots \\
#'     n_{(I, ~1)} & n_{(2, ~2)} & \cdots & n_{(I, ~J)}\end{array}\right]}
#' \deqn{
#' \Lambda = \frac{\prod_i \prod_j(n_{i+}\times n_{+j})^{n_{ij}}}{n \prod_i\prod_j n_{ij}^{n_{ij}}}}
#' \deqn{
#'  G^2 = -2\log(\Lambda) = 2\sum_i\sum_j n_{ij}\log\left(\frac{n_{ij}}{\hat{\mu}_{ij}}\right)}
#' \deqn{
#'  \hat{\mu}_{ij} = \frac{n_{i+} \times n_{+j}}{n}}
#' \deqn{
#' \underset{\text{If}~ H_0 ~ \text{is TRUE}}{G^2} \approx \chi^2_{(I-1)\times (J-1)}}
#' \deqn{
#' \chi^2_{\text{pearson}} = \sum_{i = 1}^I\sum_{j = 1}^J\frac{(n_{(i, ~j)}-\hat{\lambda}_{(i, ~j))})^2}{\hat{\lambda}_{(i, ~j))}}}
#' \deqn{
#' \underset{\text{If}~ H_0 ~ \text{is TRUE}}{\chi^2_{(\text{pearson})}} \approx \chi^2_{(I-1)\times (J-1)}}
#' \deqn{
#' \text{Trend Test Statistics} = M^2 = r^2 \times (n-1)}
#' \deqn{\underset{\text{If} ~H_0 ~ \text{Is TRUE}}{M^2} \approx \chi^2_{(1)}}
#' \deqn{n = \sum_{i =1}^I\sum_{j=1}^J n_{(i, ~j)},}
#' \deqn{r = \text{Corr}(X_1, ~X_2), \quad X_1, ~ X_2 ~\text{Are two variables of contigency table}}
#' 
#' 
#' @examples 
#' \dontrun{data(table_2)
#'     Table_Test_Result(tab = table_2)}
#' @export
Table_Test_Result <- function(tab, Levels, idLevel = 0){
    n1 <- nrow(tab); n2 <- ncol(tab); n <- sum(tab)
    N1 <- rowSums(tab); N2 <- colSums(tab)
    Log_Lambda <- 0
    lambda_hat <- outer(1:n1, 1:n2, function(i, j) N1[i] * N2[j] / n)
    for(i in 1:n1){
        for(j in 1:n2){
            Log_Lambda <- Log_Lambda + 2 * tab[i, j] * log(ifelse(tab[i, j] == 0, .5, tab[i, j]) * n / (N1[i] * N2[j]))
        }
    }
    chi_value <- sum((tab - lambda_hat)^2/lambda_hat)
    Df <- (n1 - 1)  * (n2 - 1)
    p_value_pearson <- pchisq(chi_value, df = Df, lower.tail = F)
    p_value_likelihood_ratio <- pchisq(Log_Lambda, df = Df, lower.tail = F)


    dat_tab <- get_dat_from_tab(tab)$Data
    cor_r <- cor(dat_tab)[1, 2]
    M2 <- cor_r^2 * (n - 1)
    p_value_trend <- pchisq(M2, df = 1, lower.tail = F)
    tab_res1 <- cbind(tab, N1)
    tab_res2 <- rbind(tab_res1, c(N2, n))
    row_0 <- c(ifel(idLevel, Levels[[2]], paste("level", 1:n2, sep = "-")), "Total")
    tab_res3 <- cbind(lambda_hat %>% round(4), rowSums(lambda_hat))
    tab_res4 <- rbind(tab_res3, c(colSums(lambda_hat), n))
    tab_res_5 <- rbind(row_0, tab_res4)
    tab_final_1 <- rbind(row_0, tab_res2, tab_res_5)
    col_0 <- rep(c("Levels", ifel(idLevel, Levels[[1]], paste("level", 1:n1, sep = "-")), "Total"), 2)
    tab_final_2 <- cbind(col_0, tab_final_1)
    dimnames(tab_final_2) <- NULL
    pval_3 <- c(p_value_pearson, p_value_likelihood_ratio, p_value_trend)
    p_val_name <- c("Pearson-Chisquare", "Likelihood-Ratio", "Trend-Test(Linear-by-Linear)")
    degree_free <- c(Df, Df, 1)
    stat_value <- c(chi_value, Log_Lambda, M2)
    test_result <- cbind(stat_value, degree_free, pval_3)
    test_result2 <- cbind(p_val_name, test_result %>% round(4))
    test_result3 <- cbind(test_result2, matrix("", 3, (n2-2)))
    row0_test <- c("Method", "stat_value", "Df", "p-vlaue", rep("", n2 - 2))
    test_result_final <- rbind(row0_test, test_result3, c("Total Observation", n, rep("", n2)))

    Final_result <- rbind(tab_final_2, test_result_final)
    dimnames(Final_result) <- NULL
    Final_result <- data.frame(Final_result) %>% setNames(NULL)
    Final_result2 <- Final_result
    m1 <- nrow(Final_result)
    j <- 0
    for(i in (m1-3):(m1-1)){
        j <- j + 1
        Final_result2[i, 4] <- cell_spec(Final_result2[i, 4],
        background = ifel(pval_3[j] > 0.05, 
        "green", "red"), color = "white", 
        bold = T)
    }
    for(k in (n1 + 4):(2*n1 + 3)){
        for(h in 2:(n2 + 1)){
            Final_result2[k, h] <- cell_spec(Final_result2[k, h],
            background = ifel(as.numeric(Final_result2[k, h]) >= 5, 
            "green", "red"), color = "white", 
            bold = T)
        }
    }
    ss <- sum(lambda_hat < 5)
    percent_low <- ss/prod(dim(lambda_hat)) * 100
    Foot_note1 <- sprintf("%d cells (%.2f %s) have expected count less than five. The Minimum expected count is %.2f", ss, percent_low, "%", min(lambda_hat))
    Final_result2 %>%
    kbl(caption = "Table of Test Results for Contigency Table", escape = F) %>%
    kable_paper("hover") %>%
    row_spec(c(1, n1 + 3, 2*n1 + 5), background = "green", 
    color = "white", italic = T, bold = T) %>%
    column_spec(1,
    italic = T, bold = T, width = 1) %>%
    footnote(general = Foot_note1,
           number = c("if pvalue cell is red, means that p-value < 0.05"),
           ) %>%
    group_rows(group_label = "ORIGINAL TABLE", 
           start_row = 1, end_row = (n1 + 2), 
           label_row_css = "border-top: 3px solid;", italic = TRUE, 
           color = "#d48a00") %>%
    group_rows(group_label = "EXPECTED TABLE", 
           start_row = (n1 + 3), end_row = (2 * n1 + 4), 
           label_row_css = "border-top: 3px solid;", italic = TRUE,
           color = "#d48a00") %>%
    group_rows(group_label = "TEST RESULTS", 
           start_row = (2*n1 + 5), end_row = (2*n1 + 8), 
           label_row_css = "border-top: 3px solid;", italic = TRUE, 
           color = "#d48a00") -> Table_result
    F_result <- list(ExpEcted_Vals = lambda_hat, 
    test_result = test_result_final, Total_results = Final_result, 
    original_table = tab, table_results = Table_result)
    return(F_result)
}


#' The base Function of R for applying a condition on a vector at the
#'     same time is in the form that  return is the first value of the vector 
#'     This function is designed to return a vector by applying a 
#'     condition on a vector.
#' 
#' @seealso  \code{\link[base:ifelse]{base::ifelse()}}
#' 
#' @usage ifel(cond, x, y)
#' @param cond A logical value, that is TRUE or FALSE
#' @param x if \code{cond = TRUE} return \code{x}
#' @param y if \code{cond = FALSE} return \code{y}
#' 
#' @examples 
#' \dontrun{ifel(TRUE, c(1, 2, 5), c(4, 1, 3))}
#' @export 
ifel <- function(cond, x, y){
    if(cond) return(x) else return(y)
}



#' @title Rounding vectors that have multi-type values (characters and numbers)
#'
#' @description  To round a vector that has both numeric values and character values, 
#'     we know that if a vector contains characters, all the values have character 
#'     format, but sometimes we need to round the numeric values to several decimal 
#'     places when displaying the vector. This function is implemented to round 
#'     numerical values in vectors that contain characters.
#'     
#'     
#' 
#' @seealso  \code{\link[base:round]{base::round()}}
#' 
#' @usage stats_round(x, ndigit = 4)
#' @param x A vector, with numeric and character elements
#' @param ndigit The number of decimal digits we want to round the numbers inside the vector
#' 
#' @author Habib Ezatabadi
#' 
#' @return A vector of the same size but with numbers rounded to an arbitrary number of decimal places.
#' 
#' @examples 
#' \dontrun{stats_round(x = c("a", 2.342341, "stats9", 3.324234235), ndigit = 2)}
#' @export 
stats_round <- function(x, ndigit = 4) {
    res <- unlist(lapply(x, function(a){
        temp1 <- suppressWarnings(as.numeric(a))
        if(is.na(temp1)) return(a) else{
            if(temp1 > 1e+200) return(Inf) else{
                if(temp1 < -1e+200) return(-Inf) else{
            resid <- temp1 %% 1
            if(resid > 0) return(round(temp1, ndigit)) else return(formatC(temp1, 
                format = "f", drop0trailing = TRUE))
            }}}
}))
return(res)
}



#' @title Non-Inferiority and superiority Test in cilinical Trials, for compare two Treatment
#' 
#' @description In order to implement non-inferiority tests and superiority tests, 
#' in randomized clinical trials, when we want to compare two types of treatment or two types of drugs or a drug with a placebo, 
#' there are many softwares, there are various functions in R, some From the functions used in R, 
#' they do not have optimal outputs and even sometimes, their outputs are not very correct. In this function, 
#' we have tried to have integrated outputs with a completely simple table, as well as adding a graph, 
#' two non-derogatory tests. and superiority in studies designed for parallel groups.   
#' 
#' @details for Non-Inferiority and Superiority Test for rates and continuous values like
#' Hypertension, We set a null value based on previous clinical studies or articles and studies already done, then:
#' \describe{
#' \item{Non-Inferiority}{If a treatment is better, i.e. that treatment has a larger mean in 
#' recorded observations or a higher rates, our test is defined as:
#' \deqn{\text{for rates}: \quad \begin{cases}H_0: & p_1 - p_2 \leq \delta \\
#' H_1: & p_1 - p_2 > \delta, \end{cases} \quad \delta < 0.
#' }
#' \deqn{
#' \text{or for continuous values}: \quad \begin{cases}H_0: & \text{Treat}_1 - \text{Treat}_2 \leq \delta \\
#' H_1: & \text{Treat}_1 - \text{Treat}_2 > \delta \end{cases} \quad \delta < 0.
#' }
#' If a treatment is better, i.e. that treatment has a smaller mean in the 
#' recorded observations or a smaller rates, our test is defined as:
#' \deqn{\text{for rates}: \quad \begin{cases}H_0: & p_1 - p_2 \geq \delta \\
#' H_1: & p_1 - p_2 < \delta, \end{cases} \quad \delta > 0. }
#' \deqn{
#' \text{or for continuous values}: \quad \begin{cases}H_0: & \text{Treat}_1 - \text{Treat}_2 \geq \delta \\
#' H_1: & \text{Treat}_1 - \text{Treat}_2 < \delta \end{cases} \quad \delta > 0.
#' }
#' }
#' \item{Superiority}{If a treatment is better, i.e. that treatment has a larger mean in 
#' recorded observations or a higher rates, our test is defined as:
#' \deqn{\text{for rates}: \quad \begin{cases}H_0: & p_1 - p_2 \leq \delta \\
#' H_1: & p_1 - p_2 > \delta, \end{cases} \quad \delta > 0. }
#' \deqn{
#' \text{or for continuous values}: \quad \begin{cases}H_0: & \text{Treat}_1 - \text{Treat}_2 \leq \delta \\
#' H_1: & \text{Treat}_1 - \text{Treat}_2 > \delta \end{cases} \quad \delta > 0.
#' }
#' If a treatment is better, i.e. that treatment has a smaller mean in the 
#' recorded observations or a smaller rates, our test is defined as:
#' \deqn{\text{for rates}: \quad \begin{cases}H_0: & p_1 - p_2 \geq \delta \\
#' H_1: & p_1 - p_2 < \delta, \end{cases} \quad \delta < 0. }
#' \deqn{
#' \text{or for continuous values}: \quad \begin{cases}H_0: & \text{Treat}_1 - \text{Treat}_2 \geq \delta \\
#' H_1: & \text{Treat}_1 - \text{Treat}_2 < \delta \end{cases} \quad \delta < 0.
#'}
#'}
#' \item{Rates}{
#' for rates we add three method in this code; 
#' \code{"wald"}, \code{"Farrington-Manning"} and \code{"Hauck-Anderson"} for get
#' more information about this methods 
#' you can go to this \href{https://1drv.ms/b/s!AjGgWTomcc6MgRw7N9ZTMJjKAEvO?e=SMFLgT}{link.}
#' }
#' \item{Continuous Values}{
#' for continuous values we implement a \code{T Test}:
#' \deqn{
#' \bar{x}_1 = \frac{1}{n_1}\sum_{i = 1}^{n_{1}} x_{i_{1}}, \quad \bar{x}_2 = \frac{1}{n_{2}}\sum_{i = 1}^{n_2} x_{i_{2}}
#' }
#' \deqn{
#' \bar{x}_1 \sim \mathcal{N}(\mu_1, \frac{\sigma_1}{n_1}), \quad \bar{x}_2 \sim \mathcal{N}(\mu_2, \frac{\sigma_2}{n_2}),
#' }
#' \deqn{
#' \text{we want to test}:\quad \begin{cases}H_0: & \mu_1 - \mu_2 \leq \delta \\
#' H_1: & \mu_1 - \mu_2 > \delta\end {cases},
#' }
#' \deqn{
#' \text{if}~~ \sigma_1 = \sigma_2 \implies S^2_{pooled} = \frac{(n_1 -1)S^2_1 + (n_2 -1)S^2_2}{n_1 + n_2 -2},}
#' \deqn{
#' S^2_1 = \frac{1}{n_1 - 1}\sum_{i = 1}^{n_1}(x_{i_{1}} - \bar{x}_1)^2,}
#' \deqn{
#' S^2_2 = \frac{1}{n_2 - 1}\sum_{i = 1}^{n_2}(x_{i_{2}} - \bar{x}_2)^2, \implies
#' }
#' \deqn{
#' \text{Test Statistics}: \frac{\bar{x}_1 - \bar{x}_2 - \delta}{s_{pooled}\sqrt{\frac{1}{n_1}+ \frac{1}{n_2}}} \sim \underset{\text{if}~H_0 ~\text{is TRUE}}{\sim} T_{(n_1 + n_2 - 2)}
#' }
#' \deqn{
#' \text{if}~ \sigma_1 \ne \sigma_2 \implies
#' }
#' We have to use Welch-Satterthwaite, so for more information about this statistics, go to this \href{https://en.wikipedia.org/wiki/Welch%27s_t-test}{link.}
#' }
#'}
#' 
#' @usage Inferiority_superiority_test_pa(
#'     dataType  = "binary",  Dat, alpha = .05, 
#'     Method_estimate_for_binary_data = "fm", 
#'     margin, reff = 1, better = "right", Test_Method = "N", 
#'     Name_groups = c(group1 = "standard", group2 = "new"), 
#'     data_list = NA
#'     )
#' @param dataType It can take two values, \code{c("binary", "continuous")}, to tell the 
#'     function whether our data is to compare rates or continuous values.
#' 
#' @param Dat is our data set, the thing about it is that the data must have two columns, 
#'      the first column is related to values and the second column is related to factors, 
#'      that is, there must be a specific factor in the second column, which determines that observation 
#'      It is related to which treatment, the next point is that the agents can only be of two types. 
#'      And also for binary data, the value column should have only  \code{TRUE} (success) and \code{FALSE} (failure) values for each observation.
#'      if Dat take NA value, that means our data is in data_list list. 
#' 
#' @param alpha \eqn{0 < \alpha < 1}, level of test.
#' 
#' @param Method_estimate_for_binary_data It can take 3 values, \code{c("fm", "ha", "wald")}, 
#'     it should be noted that \code{"fm"} abbreviated of \code{"Farrington-Manning"} Method, 
#'     \code{"ha"} abbreviated of \code{"Hauck-Anderson"} method, and \code{"wald"} implements the famous \code{"wald"} method for binary data.
#' 
#' @param margin \eqn{\delta,~H_0: p_1 - p_2 \leq \delta ~Vs~H_1: p_1 - p_2 > \delta~} Null-value for more information about margin in Non-Inferiority 
#'     
#' and Superiority test go to \code{details.}
#' @param reff It takes two values \code{reff = 1, reff = 2}, 1 means that we want to subtract the average of the second community from the first and 2 means that 
#'     we want to subtract the difference of the first community from the second. if \code{reff = 1} then \eqn{H_0: \mu_1 - \mu_2 \leq \delta}
#'     \code{reff = 2} then \eqn{H_0: \mu_2 - \mu_1 \leq \delta}.
#' 
#' @param better  It takes two values \code{c("left", "right")}. Maybe for certain tests, 
#'     a lower value indicates a better treatment, in which case we should give this argument the \code{"left"} value, 
#'     and if a higher value indicates a better treatment, by default, this argument contains the \code{"right"} value.
#' 
#' @param Test_Method getting two value \code{c("N", "S")} N infer to \code{"Non-Inferiority Test"} and S infer to \code{"Superiority Test"}. 
#' 
#' @param Name_groups A Vector with Two element, name of group 1 and name of group 2. 
#'     It is necessary that the values of this argument be quantified. By default, 
#'     the values of this argument are \code{c("standard", "new")} standard for group 1 and new for group two.
#' 
#' @param data_list This is A list; if \code{Dat == NA}, this argument can handle data for both binary and continuous data, 
#'     For binary data, this list takes 4 values, N11, N12, N21 and N22, of course, the order of input must be exactly the same, 
#'     where N11 refers to the number of successes related to the first group and N12 refers to the number of failures in the first group. 
#'     N21 refers to the number of successes of the second group and N22 refers to the number of failures of the second group, 
#'     but if the data is continuous, this list has two members, the first of which is X or the values related to the first group and the second Y, 
#'     which contains the values related to the second group. has it.
#'     in default; data_list = NA and Dat argument take data. 
#' 
#' @return A List of Three Components 
#'     \tabular{ll}{
#'     \code{TestResult}:\tab "htest" class that contains result of test\cr
#'     \code{plotResult}:\tab Plot of Test \cr
#'     \code{TestTable}:\tab A Html table for show results \cr
#' }
#' 
#' @return TestResult that from class "htest" contains below members:\
#' \tabular{ll}{
#'     \code{statistic}:\tab the value of the Z-statistic\cr
#'     \code{parameter}:\tab delta, rate difference (group 1 - group 2) under the null hypothesis\cr
#'     \code{p.value}:\tab the p-value for the Farrington-Manning test\cr
#'     \code{null.value}:\tab rate difference (group 1 - group 2) under the null\cr
#'     \code{alternative}:\tab a character string indicating the alternative hypothesis\cr
#'     \code{method}:\tab a character string indicating the exact method employed\cr
#'     \code{data.name}:\tab a character string giving the names of the data used\cr
#'     \code{estimate}:\tab the estimated rate difference (maximum likelihood)\cr
#'     \code{conf.int}:\tab a confidence interval for the rate difference\cr
#'     \code{sample.size}:\tab the total sample size used for the test\cr
#' }
#' @examples \dontrun{
#'     data(HyperTension)
#'     Inferiority_superiority_test_pa(dataType = "continuous", Dat = HyperTension, alpha = 0.05,
#'     margin = 5, reff = 2, better = "right", Test_Method = "N", 
#'     Name_groups  = c(group1 = "standard", group2 = "new"), 
#'     data_list = NA)
#' }
#' 
#' @author Habib Ezatabadi
#' 
#' @export 
Inferiority_superiority_test_pa <- function(
dataType  = "binary",  Dat, alpha = .05, 
Method_estimate_for_binary_data = "fm", 
margin, reff = 1, better = "right", Test_Method = "N", 
Name_groups = c(group1 = "standard", group2 = "new"), data_list = NA){

    conf.low <- xmax <- samp_diff <- conf.high <- value <- group <- Groups <- xmin <- . <- NULL
    get_delta <- function(margin){
        a <- margin; alter <- "greater"
        if(better == "right" && Test_Method == "N") a = -margin else{
            if(better == "left" && Test_Method == "S") a = -margin; alter <-  "less"
        }
        if(better == "left" && Test_Method == "N" ) alter = "less"
        return(c(alternative = alter, Delta = a))
    }
    dl_al <- get_delta(margin)
    alter <- dl_al[1]; delta <- dl_al[2] %>% as.numeric;
    LOWER <- ifelse(alter == "greater", FALSE, TRUE)

    ############################################# edit function ###################################################
    
    get_data1 <- function(x = Dat, y = data_list){
        if(!is.na(x)){
        var1 <- Dat[, 1] %>% unlist
        var2 <- Dat[, 2] %>% unlist
        temp1 <- (is.factor(var1) || is.character(var1)); temp2 <- (is.factor(var2) || is.character(var2));
        if((temp1 + temp2) != 1) stop("Dat is not valid, go to help function and insert valid data or use 
        data_list argument for more information about valid data go to help function")
        v1 <- ifel(1 * temp1 == 0, var1, var2)
        v2 <- ifel(1 * temp2 == 0, var2, var1)
        var11 <- v1[v1 == name_groups[1]]; n1 <- length(var11);
        var12 <- v1[v2 == name_groups[2]]; n2 <- length(var12); 
        Dat <- data.frame(value = c(var11, var12), Groups = rep(Name_groups, c(n1, n2)))
        }else{
            if(is.na(x) && dataType != "binary"){
                x <- y[[1]]; y2 <- y[[2]]
                n1 <- length(x); n2 <- length(y2);
                v1 <- c(x, y2); v2 <- rep(Name_groups, c(n1, n2))
                Dat <- data.frame(value = v1, Groups = v2)
            }else{
                n11 <- y[[1]]; n12 <- y[[2]]; n21 <- y[[21]]; n22 <- y[22]
                v1 <- rep(c(T, F), c(n11, n12)); v2 <- rep(c(T, F), c(n21, n22)); 
                vals <- c(v1, v2); Groups <- rep(Name_groups, c(n11 + n12, n21 + n22));
                Dat <- data.frame(value = vals, Groups = Groups)
            }
        }
        return(Dat)
    }

Dat <- get_data1(x = Dat, y = data_list)

name_groups <- Dat[, 2] %>% unlist %>% unique %>% as.character
if(name_groups %>% length != 2) stop("group factor must be two groups")
    if(dataType == "binary" && is.na(data_list)){
    val <- Dat[, 1] %>% unlist %>% as.numeric
    if(setdiff(val, c(0, 1)) %>% length != 0) stop("for binary data, just accepted TRUE or 1 for (succeed) and FALSE or 0 for (fail)")
    }

    ######################################## finish edit #########################################################
   # var1 <- Dat[, 1] %>% unlist
   # var2 <- Dat[, 2] %>% unlist
   # var11 <- var1[var2 == name_groups[1]]; n1 <- length(var11);
   # var12 <- var1[var2 == name_groups[2]]; n2 <- length(var12); 
   # Dat <- data.frame(value = c(var11, var12), Groups = rep(name_groups, c(n1, n2)))
    x <- subset(Dat, Groups == name_groups[1], select = value) %>% unlist %>% setNames(NULL)
    y <- subset(Dat, Groups == name_groups[2], select = value) %>% unlist %>% setNames(NULL)
    Method <- ifelse(Test_Method == "N", "Non-Inferiority", "Superiority")
    if(dataType == "binary"){

        get_estimate_method <- function(x){
        Estimate_Method <- if(x == "fm") return("Farrington-Manning")else{
            if(x == "ha") return("Hauck-Anderson") else return("wald")
        }
        } 
        estimate_binary_method <- get_estimate_method(Method_estimate_for_binary_data)
        if(reff == 1){
            group1 = x
            group2 = y}else{group2 = x; group1 = y}
        Test_Result <- list()
        class(Test_Result) <- "htest"
        Test_Result$null.value <- c(margin)
        names(Test_Result$null.value) <- c("rate difference (group 1 - group 2)")
        Test_Result$alternative <- c(alter)
        str1 <- sprintf("%s test for rates according to %s Approach", Method, estimate_binary_method)
        Test_Result$method <- c(str1)
        Test_Result$data.name <- sprintf("group 1: %s, group 2: %s", Name_groups[1], Name_groups[2]) 
        n11 <- sum(group1); n12 <- sum(!group1); 
        n21 <- sum(group2); n22 <- sum(!group2);
        n <- n11 + n12 + n21 + n22; n1. <- n11 + n12; n2. <- n21 + n22; 
        n.1 <- n11 + n21; n.2 <- n12 + n22
        Test_Result$sample.size <- c(n)
        names(Test_Result$sample.size) <- "sample size"
        Test_Result$estimation.method <- c(estimate_binary_method)
        p1 <- mean(group1); p2 <- mean(group2)
        dhat <- p1 - p2
        Test_Result$estimate <- c(dhat)
        names(Test_Result$estimate) <- c("rate Difference: (group1 - group2)")
        get_ci_pval_zscore <- function(xx){
            sd_wald <- sqrt(p1 * (1-p1)/(n1.) + p2 * (1-p2)/(n2.))
            sd_ha <- sqrt(p1 * (1-p1)/(n1.-1) + p2 * (1-p2)/(n2.-1))

            c_ha <- 1 / (2 * min(c(n1., n2.)))
            ## fm: estimate sd("fm-method")

            theta <- n2./n1.; d <- p1 * abs(delta) * (1-abs(delta));
            c_fm <- abs(delta)^2 - abs(delta) * (2*p1 + theta + 1) + p1 + theta * p2
            b <- -(1 + theta  + p1 + theta * p2 - abs(delta) * (theta + 2))
            a <- 1 + theta
            v <- b^3 / (3 * a)^3 - (b * c_fm) / (6 * a^2) + d/(2 * a)
            u <- sign(v) * sqrt(b^2 / (3 * a)^2 - c_fm / (3 * a))
            w <- (pi + acos(v / u^3))/3
            p_fm_1 <- 2 * u * cos(w) - b/(3 * a); p_fm_2 <- p_fm_1 + abs(delta)
            sd_fm <- sqrt(p_fm_1 * (1 - p_fm_1)/n1. + p_fm_2 * (1 - p_fm_2)/n2.)
            ###################################
            # zwald 
            z_wald <- (dhat - delta)/sd_wald; 
            # z Hauck-Anderson
            z_ha <- (dhat - delta + c_ha)/sd_ha
            # z Farrington-Manning
            z_fm <- (dhat - delta) / sd_fm 
            # conf_int wald 
            zz <- qnorm(alpha/2, lower.tail = F)
            ci_wald <- c(dhat - zz * sd_wald, dhat + zz * sd_wald)
            ci_ha <- c(dhat - c_ha - zz * sd_ha, dhat + c_ha + zz * sd_ha)
            ci_fm <- c(dhat - zz * sd_fm, dhat + zz * sd_fm)
            pval_wald <- pnorm(z_wald, lower.tail = LOWER)
            pval_ha <- pnorm(z_ha, lower.tail = LOWER)
            pval_fm <- pnorm(z_fm, lower.tail = LOWER)
            temp_wald <- c(sd_wald, z_wald, pval_wald, ci_wald)
            temp_ha <- c(sd_ha, z_ha, pval_ha, ci_ha)
            temp_fm <- c(sd_fm, z_fm, pval_fm, ci_fm)
            result <- switch(xx, 
            "fm" = temp_fm, 
            "ha" = temp_ha, 
            temp_wald)
            return(result)
        }
        temp1 <- get_ci_pval_zscore(Method_estimate_for_binary_data)
        Test_Result$statistic <- temp1[2]
        names(Test_Result$statistic) <- "Z-statistic"
        Test_Result$p.value <- temp1[3]
        
        Test_Result$conf.int <- temp1[4:5]
        attr(Test_Result$conf.int, "conf.level") <- 1- alpha
        #############################################################

        p_group1 <- p1; p_group2 <- p2; 
        n <- Test_Result$sample.size; p_diff <- Test_Result$estimate;
        Z_statistic = Test_Result$statistic
        conf_int <- Test_Result$conf.int; p_value <- Test_Result$p.value
        row_1 <- c("p group1", "n group1", "p group2", "n group2", 
        "rate difference (group1 - group2)", "Z-Statistic", "p-value", 
       "CI-LowerBond", "CI-UpperBond")
       row_2 <- c(p_group1, n1, p_group2, n2, p_diff, Z_statistic, p_value, conf_int[1], 
       conf_int[2]) %>% setNames(NULL) %>% round(4)
       word <- ifelse(p_value < alpha, "Accepted", "Rejected")
       Footnote <- sprintf("based on p-value = %.4f, therefore %s Test %s, alpha = %.3f", p_value, Method, word, alpha)
       Footnote2 <- sprintf("Confidence Interval is %.1f %s", (1-alpha) * 100, "%")
       Footnote3 <- "if cell of p-value is red, means that: p-value < alpha"

       Title <- str1
       Table1 <- rbind(row_1, row_2) %>% t %>% as.data.frame %>% setNames(NULL) 
       Table_12 <- Table1
       Table_12[7, 2] = cell_spec(Table_12[7, 2], background  = ifelse(as.numeric(Table_12[7, 2]) > alpha,
        "white", "red"), 
        color = ifelse(as.numeric(Table_12[7, 2]) > alpha, "black", "yellow"))
       Table_12 %>%
           kbl(caption = Title, escape = F) %>%
           kable_paper("hover", full_width = F) %>%
           column_spec(1, background = "green", 
              color = "white", 
              italic = T, bold = T, width = "15em") %>%
           column_spec(2, bold = T) %>%
           pack_rows(group_label = sprintf("Confidence Interval; Level is %.1f %s", (1-alpha) * 100, "%"), 
           start_row = 8, end_row = 9, 
           label_row_css = "border-top: 3px solid;", italic = TRUE,
           color = "#d48a00") %>%
           footnote(general =  Footnote3,
           number = c(Footnote, Footnote2)) -> Table_kable

                   ##############################################################################

        dat_plot <- data.frame(x = c(p_diff), y = c(2), xmin = c(conf_int[1]), xmax = c(conf_int[2]))
          
        P <- dat_plot %>%
            ggplot(aes(x, y)) + 
            geom_point(size = 20, col = "red") + 
            geom_vline(xintercept = margin, linetype = 2, color = "red", linewidth = 2) + 
            geom_vline(xintercept = -margin, linetype = 2, color = "red", linewidth = 2) + 
            geom_errorbar(aes(xmin = xmin, xmax = xmax), color = "darkblue", width = .2, size = 4) + 
            coord_cartesian(ylim = c(1, 3)) + 
            annotate(geom = "text", x = margin, y = 2.5, 
            label = ifelse(margin < 0, "-~delta", "+~delta"), 
            size = 20, parse = T) +
            annotate(geom = "text", x = -margin, y = 2.5, 
            label = ifelse(margin < 0, "+~delta", "-~delta"), 
            size = 20, parse = T) +
            annotate(geom = "text", x = p_diff, y = 2.15, 
            label = "CI for difference Ratio", size = 15)+
            theme_bw() + 
            labs(title = str1, 
            caption = sprintf("%s Test %s", Method, word), x = "Ratio Difference", y = "") + 
            theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
            final_result <- list(TestResult = Test_Result, 
            TableResult = Table_kable, 
            plotResult = P)
            return(final_result)
       }else{

       #######################################

          TestRes <- list()
            class(TestRes) <- "htest"
            TestRes$null.value <- c(delta)
            
            n <- nrow(Dat)
            Dat %>%
            group_by(Groups) %>%
            get_summary_stats(value, type = "common") %>%
            mutate(variable = NULL, variables = Groups) %>%
            relocate("variables", .before = Groups) %>%
            mutate(Groups = NULL) -> listResult; 
            if(reff == 1) REFF = name_groups[1] else REFF = name_groups[2]
            Dat %>%
            t_test(value ~ Groups, mu = delta, alternative = alter, 
            detailed = T, ref.group = REFF) -> tt_result
            CI_satter <- c(tt_result$conf.low, tt_result$conf.high)
             tt_result %>% unlist -> testResult 
            CI_satter_two_sided <- Dat %>% t_test(value ~ Groups, mu = delta, detailed = T, 
            ref.group = REFF) %>% dplyr::select(conf.low, conf.high) %>% unlist

            names(TestRes$null.value) <- sprintf("Mean Difference: %s - %s", REFF, 
            setdiff(Name_groups, c(REFF)))

            TestRes$alternative <- c(alter)
            str1 <- sprintf("%s Test: According T test for Mean Difference",Method)
            TestRes$method <- c(str1)
            TestRes$data.name <- c(sprintf("group 1: %s, group 2: %s", Name_groups[1], 
            Name_groups[2]))

            TestRes$sample.size <- c(n)
            names(TestRes$sample.size) <- "sample size"
            TestRes$estimation.method <- c("T Test")
            TestRes$estimate <- c(testResult[1]) %>% as.numeric
            names(TestRes$estimate) <- c(sprintf("Mean Difference: (%s - %s)", 
            REFF, setdiff(Name_groups, c(REFF))))

            Fvlaue <- var.test(x, y)$statistic
            pVal <- var.test(x, y)$p.value
            Nam <- c(paste(REFF, setdiff(name_groups, c(REFF)), sep = " - "), REFF, 
            setdiff(name_groups, c(REFF)), "n1", "n2", "t_statistic", "p-value", 
            "df", "CI_Lower_bond", "CI_Upper_bond", "Margin")
            TEST_result_satt <- testResult[c(1, 2, 3, 7, 8, 9, 10, 11, 12, 13)]


            TEST_result_satt[length(TEST_result_satt) + 1] <- delta
            cbind(Nam, TEST_result_satt) %>% as.data.frame %>% setNames(NULL) -> testResult2
            rownames(testResult2) <- NULL
            testResult2 %>% t %>% as.data.frame %>% setNames(.[1, ]) %>% .[-1, ] %>% unlist %>% 
            stats_round %>% as.data.frame  %>% rownames_to_column(var = "nam") %>% t %>% as.data.frame %>% 
            setNames(.[1, ]) %>% .[-1, ] -> satter_result
            pvalue_satter <- satter_result[, 7] %>% as.numeric %>% round(4)
            word_satt <- ifelse(pvalue_satter < alpha, "Accepted", "Rejected")


            Dat %>%
            t_test(value ~ Groups, mu = delta, alternative = alter, 
            detailed = T, ref.group = REFF, var.equal = T) -> tt_result_2
            CI_equal <- c(tt_result_2$conf.low, tt_result_2$conf.high)
             tt_result_2 %>% unlist -> simple_result1 
            CI_equal_two_sided <- Dat %>% t_test(value ~ Groups, mu = delta, detailed = T, 
            ref.group = REFF, var.equal = T) %>% dplyr :: select(conf.low, conf.high) %>% unlist

            TEST_result_equal <- simple_result1[c(1, 2, 3, 7, 8, 9, 10, 11, 12, 13)]
           

            TEST_result_equal[length(TEST_result_equal) + 1] <- delta
            cbind(Nam, TEST_result_equal) %>% as.data.frame %>% setNames(NULL) -> testResult2_equal
            rownames(testResult2_equal) <- NULL
            testResult2_equal %>% t %>% as.data.frame %>% setNames(.[1, ]) %>% .[-1, ] %>% unlist %>% 
            stats_round %>% as.data.frame  %>% rownames_to_column(var = "nam") %>% t %>% as.data.frame %>% 
            setNames(.[1, ]) %>% .[-1, ] -> equal_result
            pvalue_equal <- equal_result[, 7] %>% as.numeric %>% round(4)
            word_equal <- ifelse(pvalue_equal < alpha, "Accepted", "Rejected")


            result_stat <- ifel(pVal < 0.05, 
                c(satter_result[, c(2, 3)] %>% unlist, tstatistics = satter_result[, 6] %>% unlist, 
                pvalue = satter_result[, 7] %>% unlist, df = satter_result[, 8] %>% unlist, ci_low = CI_satter[1], 
                ci_high = CI_satter[2]), 
                c(equal_result[, c(2, 3) %>% unlist], tstatistics = equal_result[, 6] %>% unlist, 
                pvalue = equal_result[, 7] %>% unlist, df = equal_result[, 8] %>% unlist, ci_low = CI_equal[1], 
                ci_high = CI_equal[2])) %>% as.numeric
            

               TestRes$statistic <- result_stat[3] 
               names(TestRes$statistic) <- "T-statistic"
               TestRes$p.value <- result_stat[4]
               TestRes$parameters <- result_stat[c(1, 2, 5)]
               names(TestRes$parameters) <- c(c(REFF), setdiff(Name_groups, REFF),"df")
               TestRes$conf.int <- result_stat[c(6, 7)]
               attr(TestRes$conf.int, "conf.level") <- 1- alpha

           final_result <- rbind(equal_result, satter_result) %>%
            as.data.frame 
            
            var_nams <- c("Assumption Equal variance", "Welch-Satterthwaite")
            final_result <- final_result %>% mutate(Test_method = var_nams) %>%
            relocate("Test_method", .before = Nam[1])
            Nam <- names(final_result)
            add_F_test <- data.frame(v1 = c("F Test for equal variance", ""), 
            v2 = c("F-value", Fvlaue %>% round(4)), 
            v3 = c("p-value", pVal %>% round(4)))

            mtemp <- matrix("", 2, 9)
            add_F_test2 <- cbind(add_F_test, mtemp) %>% as.data.frame %>% 
            setNames(NULL)
            rownames(add_F_test2) <- NULL
            rownames(final_result) <- NULL
            add_F_test3 <- add_F_test2 %>% setNames(Nam)
            final_result_2 <- rbind(final_result, add_F_test3)

            Title <- sprintf("Table of %s Test Results", Method)

            Footnote_satter <- sprintf("Welch-Satterthwaite: based on p-value = %.4f, therefore %s Test %s, alpha = %.3f", pvalue_satter, Method, word_satt, alpha)
            Footnote_equal <- sprintf("Assumption Equal Variance: based on p-value = %.4f, therefore %s Test %s, alpha = %.3f", pvalue_equal, Method, word_equal, alpha)
            Footnote2 <- sprintf("Confidence Interval is %.1f %s", (1-alpha) * 100, "%")
            fword1 <- ifelse(pVal < 0.05, "Rejected", "Accepted"); fword2 <- ifelse(pVal < .05, "Welch-Satterthwaite", "Assumption Equal Variance")
            Footnote4 <- sprintf("Test for Homogeniety of Variance; H0 is %s. therefore, we have to look at the %s test results", fword1, fword2)

            final_result_3 <- final_result_2
            
            final_result_2 %>%            
            kbl(caption = Title, escape = F) %>%
            kable_paper("hover", full_width = F) %>%
            row_spec(0, background = "green", 
              color = "white", 
              italic = T, bold = T)  %>%
              row_spec(1, background = "white", 
              bold = T, color = "black") %>%
            row_spec(2, background = ifelse(pVal < 0.05, "red", "white"), 
            bold = T, color = ifelse(pVal < 0.05, "white", "black")) %>% 
            pack_rows(group_label = sprintf("Test of Homogeneity of Variance, level of test = 0.05"), 
                start_row = 3, end_row = 4, 
            label_row_css = "border-top: 3px solid;", italic = TRUE,
           color = "#d48a00") %>%
           row_spec(3, background = "green", color = "white", bold = T, 
           italic = T) %>%
           row_spec(4, background = ifelse(pVal < 0.05, "red", "#bebebeb0"), 
            bold = T, color = ifelse(pVal < 0.05, "white", "black")) %>%
            footnote(general =  Footnote4,
            number = c(Footnote_equal, Footnote_satter, Footnote2)) -> Table_kable


            Word <- ifelse(pVal < 0.05, word_satt, word_equal)
            ind_deter <- which(CI_equal > 1e+16)
            CI_equal[ind_deter] <- CI_equal_two_sided[ind_deter]
            CI_satter[ind_deter] <- CI_satter_two_sided[ind_deter]
            dat_plot_c <- data.frame(samp_diff = rep(TestRes$estimate, 2), y = c(1.5, 1.8), 
            xmin = c(CI_equal[1], CI_satter[1]), 
            xmax = c(CI_equal[2], CI_satter[2]), group = c("Assumption Homogeniety Variance", "Welch-Satterthwaite"))  


            P_c <- dat_plot_c %>%
            ggplot(aes(x = samp_diff, y = y, group = group)) + 
            geom_point(size = 15, shape = 16) + 
            geom_vline(xintercept = delta, linetype = 2, color = "red", linewidth = 2) + 
            geom_vline(xintercept = -delta, linetype = 2, color = "red", linewidth = 2) + 
            geom_segment(aes(x = ifel(alter == "greater", xmin, xmax)
            , y = y, yend = y, xend = ifel(alter == "greater", xmax, xmin), 
            color = group), 
            linewidth = 4, arrow = arrow(length = unit(0.3, "inches")), 
            lineend = "square", linejoin = "round") +
            coord_cartesian(ylim = c(1, 2.1), xlim = range(c(-abs(delta) - 1, abs(delta) + 1))) + 
            annotate(geom = "text", x = delta, y = 2.1, 
            label = ifelse(delta < 0, "-~delta", "+~delta"), 
            size = 20, parse = T) +
            annotate(geom = "text", x = -delta, y = 2.1, 
            label = ifelse(delta < 0, "+~delta", "-~delta"), 
            size = 20, parse = T) + 
            theme_bw() + 
            labs(title = sprintf("plot of CI for %s Test", Method), 
            caption = sprintf("%s Test %s", Method, Word), 
            x = "sample Difference", y = "Test Method") + 
            theme(legend.position = 'bottom', 
            legend.title = element_blank(), 
            axis.ticks.y = element_blank(), axis.text.y = element_blank() )

            
            finall_Result <- list(TtestResult = TestRes, 
            HomogeneityTest = add_F_test2, TestPlot = P_c, Test_Table = Table_kable)
            return(finall_Result)
            }

    }



#'@title  HyperTension Data
#'
#' @description 
#'     A dataset with two variables and 200 observations.
#'
#' \itemize{
#'     \item value: The variable in which blood pressure 
#'         measurement values are stored in two groups of standard treatment and new treatment.
#'     \item Groups: The name of the treatment group
#' }
#'
#' @docType data
#' @keywords datasets
#' @name HypterTension
#' @usage data(HyperTension)
#' @format dataframe
"HyperTension"





#' @title rct_binary_data_1 Data
#' 
#' @description 
#'     A dataset with two variables and 114 observations.
#' 
#' \itemize{
#'     \item values: Recovery or non-recovery for the treatment group.
#'     \item group: The name of the treatment group
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rct_binary_data_1
#' @usage data(rct_binary_data_1)
#' @format dataframe
"rct_binary_data_1"






#' @title rct_continuous_data_2 Data
#' 
#' @description 
#'     A dataset with two variables and 21 observations.
#' 
#' \itemize{
#'     \item values: Measured criteria for each patient's recovery 
#'     \item treat: The Type of Treatment
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rct_continuous_data_2
#' @usage data(rct_continuous_data_2)
#' @format dataframe
"rct_continuous_data_2"




#' @title rct_continuous_data_3 Data
#' 
#' @description 
#'     A dataset with two variables and 22 observations.
#' 
#' \itemize{
#'     \item values: Measured criteria for each patient's recovery 
#'     \item treat: The Type of Treatment
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rct_continuous_data_3
#' @usage data(rct_continuous_data_3)
#' @format dataframe
"rct_continuous_data_3"




#' @title rct_binary_data_2 Data
#' 
#' @description 
#'     A Table with 3 variables.
#' 
#' \itemize{
#'     \item drug: A type of drug 
#'     \item alive: alive = 1, death = 0
#'     \item count number of patients that alive or death.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rct_binary_data_2
#' @usage data(rct_binary_data_2)
#' @format dataframe
"rct_binary_data_2"





#' @title rct_binary_data_3 Data
#' 
#' @description 
#'     A Table with 3 variables.
#' 
#' \itemize{
#'     \item method_treat: A type of Treatment 
#'     \item state_life: alive = 1, death = 0
#'     \item count number of patients that alive or death.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rct_binary_data_3
#' @usage data(rct_binary_data_3)
#' @format dataframe
"rct_binary_data_3"







#' @title Confidence ellipse for bivariate normal
#' 
#' @description To draw confidence ellipses for the mean of a two-variable normal distribution, 
#' there are functions in R, but none of them are theoretically accurate. Here, we have tried to draw this 
#' confidence ellipse in accordance with the academic texts.
#' 
#' @usage draw_ellipse_ci(S, xbar, alpha = .05, n = 100)
#'
#' @param S The Covariance Matrix
#' 
#' @param xbar sample Mean
#' 
#' @param alpha \eqn{1-\alpha} is level of CI.
#' 
#' @param n number of observaions
#' 
#' @return A plot that draw a ellipse with its diagonals and show sample Mean in center of that.
#' 
#' @references Johnson, R. A., & Wichern, D. W. (1992). Applied multivariate statistical analysis. New Jersey, 405.
#' 
#' @author Habib Ezatabadi
#' 
#' @examples 
#' \dontrun{
#'     S <- matrix(c(1, -1, -1, 4), 2, 2)
#'     xbar <- c(0, 0)
#'     draw_ellipse_ci(S = S, xbar = xbar, alpha = .05, n = 100)
#' }
#'
#' @export 
draw_ellipse_ci <- function(S, xbar, alpha = .05, n = 100){
    x0 <- xbar[1]; y0 <- xbar[2]
    la <- eigen(S)$values
    vecs <- eigen(S)$vectors
    l1 <- la[1]; l2 <- la[2];
    vec1 <- vecs[, 1]; vec2 <- vecs[, 2]
    theta <- asin(vec1[2])
    cc <- (2 * (n-1))/(n * (n - 2)) * qf(1-alpha, 2, n-2)
    a <-  sqrt(cc * l1)
    b <- sqrt(cc * l2)


    m1 <- -vec1[2] / vec1[1]
    m2 <- -vec2[2] / vec2[1]

    intercept1 <- xbar[2] - m1 * xbar[1]
    intercept2 <- xbar[2] - m2 * xbar[1]

    P <- ggplot() + 
    geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, 
               angle = theta), linewidth = 1.2, col = "darkblue") + coord_fixed() + 
    theme_bw()

    P + geom_abline(slope = m1, intercept = intercept1, 
                col = 'red', linewidth = 1, linetype = 2) + 
    geom_abline(slope = m2, intercept = intercept2, 
              col = "purple", linetype = 2, linewidth = 1) -> P1
    P2 <- P1 + annotate(geom = "point", x = xbar[1], y = xbar[2], 
              size = 5, col = "red")            
    return(P2)
}