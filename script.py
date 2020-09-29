print(1, True, "str", 1.23)
a = 2
while a <= 100:
    i = 2
    f = True
    while i < a:
        if a % i == 0:
            f = False
        i = i + 1
    if f:
        print(a)
    a = a + 1
