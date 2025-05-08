n= int(input("valor de n: "))
m= int(input("valor de m: "))

while n>=m:
    print("volver a ingresar los valores n<m")
    n= int(input("valor de n: "))
    m= int(input("valor de m: "))
s=0
while n <= m:
     s = s + n
     n +=1
print(s)
