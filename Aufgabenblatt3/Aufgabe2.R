df = data.frame(
    PTCA_erfolgreich = c(35, 76),
    PTCA_nicht_erfolgreich = c(13, 7),
    row.names = c("vorheriger_Herzinfarkt", "kein_vorheriger_Herzinfarkt")
)
p1 = df[1,1] / sum(df[1,])  # Erfolgsquote bei vorherigem Herzinfarkt
p2 = df[2,1] / sum(df[2,])  # Erfolgsquote ohne vorherigen Herzinfarkt
standartdeviation_p1 = sqrt(p1*(1- p1) / sum(df[1,]))
standartdeviation_p2 = sqrt(p2*(1- p2) / sum(df[2,]))
intervall_p1 = c(p1 - 1.96 * standartdeviation_p1, p1 + 1.96 * standartdeviation_p1)
intervall_p2 = c(p2 - 1.96 * standartdeviation_p2, p2 + 1.96 * standartdeviation_p2)
#aufgabe c
sum1_tilde = sum(df[1,]) + 4
sum2_tilde = sum(df[2,]) + 4
p1_tilde = (df[1,1] +2) / sum1_tilde
p2_tilde = (df[2,1] +2) / sum2_tilde
standartdeviation_p1_c = sqrt(p1_tilde*(1-p1_tilde) / sum1_tilde)
standartdeviation_p2_c = sqrt(p2_tilde*(1-p2_tilde) / sum2_tilde)
intervall_p1_c = c(p1_tilde - 1.96 * standartdeviation_p1_c, p1_tilde + 1.96* standartdeviation_p1_c)
intervall_p2_c = c(p2_tilde - 1.96 * standartdeviation_p2_c, p2_tilde + 1.96* standartdeviation_p2_c)
