# 3.7
# e


alpha <- 0.05
crit_val <- qt(1 - alpha/2, df = 49)  


t_val <- 0.572


plot(x, y, type = "l", lwd = 2, col = "black",
     xlab = "t value", ylab = "f(t)",
     main = "t distribution and rejection region")


polygon(c(x[x >= crit_val], rev(x[x >= crit_val])), 
        c(y[x >= crit_val], rep(0, sum(x >= crit_val))),
        col = rgb(1, 0, 0, 0.5), border = NA)

polygon(c(x[x <= -crit_val], rev(x[x <= -crit_val])), 
        c(y[x <= -crit_val], rep(0, sum(x <= -crit_val))),
        col = rgb(1, 0, 0, 0.5), border = NA)


p_x <- seq(-t_val, t_val, length.out = 100)
p_y <- dt(p_x, df = 49)

polygon(c(p_x, rev(p_x)), c(p_y, rep(0, length(p_x))),
        col = rgb(0, 0, 1, 0.5), border = NA)


text(crit_val, dt(crit_val, df = 49) + 0.02, 
     labels = round(crit_val, 2), col = "red", pos = 3, cex = 1.2)
text(-crit_val, dt(crit_val, df = 49) + 0.02, 
     labels = round(-crit_val, 2), col = "red", pos = 3, cex = 1.2)

text(t_val, dt(t_val, df = 49) + 0.02, 
     labels = round(t_val, 2), col = "blue", pos = 3, cex = 1.2)
text(-t_val, dt(t_val, df = 49) + 0.02, 
     labels = round(-t_val, 2), col = "blue", pos = 3, cex = 1.2)



abline(v = crit_val, col = "red", lty = 2, lwd = 2)
abline(v = -crit_val, col = "red", lty = 2, lwd = 2)
abline(v = t_val, col = "blue", lty = 2, lwd = 2)
abline(v = -t_val, col = "blue", lty = 2, lwd = 2)


legend("topright", legend = c("rejection region (alpha=0.05)", "p value region (0.572)", "critical value", "t value"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5), NA, NA), 
       col = c(NA, NA, "red", "blue"), lty = c(NA, NA, 2, 2), border = NA)
