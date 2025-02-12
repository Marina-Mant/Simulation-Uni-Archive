# Αριθμός δειγμάτων
n = 10^5

# Παραγωγή ψευδοτυχαίων αριθμών για U1 και U2
U1 = runif(n)
U2 = runif(n)

# Υπολογισμός της X
X = -log(U1 + U2)

# Εκτίμηση της μέσης τιμής E(X)
mean_X = mean(X)

# Εκτίμηση της διασποράς V(X)
var_X = var(X)

# Δημιουργία ιστογράμματος με εύρος κλάσεων 0.05
hist(X[X>min(X)&X<max(X)], seq(min(X), max(X), 0.05), prob=TRUE, main="Histogram of X = -log(U1 + U2)",
     xlab="X", col="lightblue")

# Προσθήκη γραμμής της θεωρητικής PDF (προαιρετικά αν γνωρίζαμε τη θεωρητική μορφή)
#lines(x, theor_density, type="l", col="slateblue")

# Εκτύπωση αποτελεσμάτων
cat("Μέση τιμή E(X):", mean_X, "\n")
cat("Διασπορά V(X):", var_X, "\n")
