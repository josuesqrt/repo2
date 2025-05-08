# Solicitar al usuario los valores de n y m
n = int(input("Ingrese el valor de n (n < m): "))
m = int(input("Ingrese el valor de m: "))

# Verificar que n < m
if n < m:
    suma = 0
    for i in range(n, m + 1):
        suma += i
    print(f"La sumatoria desde i = {n} hasta i = {m} es: {suma}")
else:
    print("Error: n debe ser menor que m.")
