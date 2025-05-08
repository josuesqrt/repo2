temp = float(input("ingrese la temperatura"))
escala = input("ingresa la escala F(Farenheit) o C(Celcius): ")
if escala == "F":
    temp_conv = 5/9*(temp-32)
else:
    temp_conv = (9/5*temp)-32
print("La temperatura ingresada es: ", temp, "la temperatura convertida es: ", temp_conv)
