add = lambda a: lambda b: lambda c: a + b + c
print(add(1)(2)(3))
add2 = add(0)
print(add2(1)(2))
