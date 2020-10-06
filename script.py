def fibTail(n, a, b):
    if n == 1:
        return a
    return fibTail(n - 1, b, a + b)

def fib(n):
    return fibTail(n, 1, 1)

i = 1
while i <= 10:
    print(fib(i))
    i += 1
inc = lambda x: x + 1
add = lambda x: lambda y: x + y
print(add(1)(1))
print(add(1)(1))
