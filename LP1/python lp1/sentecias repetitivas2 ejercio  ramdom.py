""" for i in range(5):
    if i ==3:
        break
    print(1)

i=0
while i < 5:
    if i ==3:
        break
    print(i)
    i = i+1 """


"""for i in range(7):
    if i ==3:
        continue
    print(1)

i=0
while i < 7:
    if i ==6:
        continue
    print(i)
    i = i+1"""


"""i=-1
while i < 6:
    i= i +1
    if i ==5:
        continue
    print(i)"""



s=0
i=1
while i < 5:
    j=1
    while j<5:
        print(i,j)
        s= s+i+j
        print(s)
        j=j+1
    i=i+1
