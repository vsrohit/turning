exp <- Fz_data[[10]]
slice <- exp#[20000:140000,]
ggplot(slice, aes(x = t, y = Fz)) + geom_line()

# Exp 1  Entry - 40000  Exit - 150000
# Exp 2  Entry - 120000 Exit - 170000
# Exp 3  Entry - 40000  Exit - 145000 
# Exp 4  Entry - 40000  Exit - 110000
# Exp 5  Entry - 100000 Exit - 130000
# Exp 6  Entry - 40000  Exit - 110000
# Exp 7  Entry - 60000  Exit - 150000 
# Exp 8  Entry - 25000  Exit - 130000
# Exp 9  Entry - 40000  Exit - 150000
# Exp 10 Entry - 50000  Exit - 150000