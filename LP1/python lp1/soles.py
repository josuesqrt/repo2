monto = float(input("ingrese el monto de compra: "))
if monto > 15000:
    pago = 0.75*monto
else:
    if monto > 7000:
        pago = 0.82*monto
    else:
        if monto > 500:
            pago = 0.95*monto
        else:
            pago = monto
print("el monto a pagar es: ", pago)
