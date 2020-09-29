print(1, True, "str", 1.23)
l = [1,True,[3.141,1.414],"hello"]
print(l)
a = 2
while a <= 100:
    i = 2
    f = True
    while i < a:
        if a % i == 0:
            f = False
        i += 1
    if f:
        print(a)
    a += 1
