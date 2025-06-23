rm(list=ls())

library(latex2exp)

z_p09 = qnorm(p=0.9)
t_p09_df3  = qt(0.9, df=3)
t_p09_df10 = qt(0.9, df=10)
t_p_01_df3 = -qt(0.1, df=3)
t_p_01_df10 = -qt(0.1, df=10)