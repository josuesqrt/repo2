n= int(input("valor de n: "))
m= int(input("valor de m: "))
if n < m:
    s=0
    while n <= m:
        s = s + n
        n +=1
    print(s)
else:
    print("n debe ser mayor a m")
