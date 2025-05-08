# Funciones de conversión
def celsius_a_fahrenheit(celsius):
    return (celsius * 9/5) + 32

def fahrenheit_a_celsius(fahrenheit):
    return (fahrenheit - 32) * 5/9

# Pedir al usuario que elija la opción
opcion = input("¿Quieres convertir de Celsius a Fahrenheit o de Fahrenheit a Celsius? (C/F): ").strip().lower()

if opcion == 'c':
    celsius = float(input("Ingresa la temperatura en Celsius: "))
    fahrenheit = celsius_a_fahrenheit(celsius)
    print(f"{celsius} grados Celsius son {fahrenheit} grados Fahrenheit.")
elif opcion == 'f':
    fahrenheit = float(input("Ingresa la temperatura en Fahrenheit: "))
    celsius = fahrenheit_a_celsius(fahrenheit)
    print(f"{fahrenheit} grados Fahrenheit son {celsius} grados Celsius.")
else:
    print("Opción no válida. Por favor ingresa 'C' para Celsius o 'F' para Fahrenheit.")
