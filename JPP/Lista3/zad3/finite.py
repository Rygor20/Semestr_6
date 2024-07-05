class FiniteField:
    characteristic = 1234567891

    def __init__(self, val=0):
        self.value = self.to_range(val)

    def to_range(self, x):
        x %= self.characteristic
        while x < 0:
            x += self.characteristic
        return x
    
    def invert(self, x):
        for i in range(self.characteristic):
            if (x * i) % self.characteristic == 1:
                return i
        return -1

    @classmethod
    def get_characteristic(cls):
        return cls.characteristic
    
    def __eq__(self, other):
        return self.value == other.value

    def __ne__(self, other):
        return not self == other

    def __le__(self, other):
        return self.value <= other.value
    
    def __ge__(self, other):
        return self.value >= other.value
    
    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    def __add__(self, other):
        return FiniteField(self.value + other.value)

    def __sub__(self, other):
        return FiniteField(self.value - other.value)

    def __mul__(self, other):
        return FiniteField(self.value * other.value)

    def __truediv__(self, other):
        if other.value == 0:
            raise ValueError("Error: Dividing by zero")
        inv = self.invert(other.value)
        if inv == -1:
            raise ValueError("Error: Irreversible element")
        return FiniteField((self.value * inv) % self.characteristic)

    def __iadd__(self, other):
        self.value = self.to_range(self.value + other.value)
        return self

    def __isub__(self, other):
        self.value = self.to_range(self.value - other.value)
        return self

    def __imul__(self, other):
        self.value = self.to_range(self.value * other.value)
        return self

    def __itruediv__(self, other):
        self.value = (self / other).value
        return self

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return str(self.value)

# if __name__ == "__main__":
#     a = FiniteField(5)
#     b = FiniteField(1234576)
#     c = FiniteField()
#     print("Given 'a':", a, "Given 'b':", b)
#     print("Characteristic:", FiniteField.get_characteristic())

#     print("a == b ?", a == b)
#     print("a != b ?", a != b)
#     print("a <= b ?", a <= b)
#     print("a >= b ?", a >= b)
#     print("a < b ?", a < b)
#     print("a > b ?", a > b)

#     c = a + b
#     print("a + b =", c)

#     c = a - b
#     print("a - b =", c)

#     c = a * b
#     print("a * b =", c)
#     c = a / b
#     print("a / b =", c)

#     c += a
#     print("c += a :", c)
#     c -= a
#     print("c -= a :", c)
#     c *= a
#     print("c *= a :", c)
#     c /= a
#     print("c /= a :", c)

#     a = FiniteField(int(input("Enter a value for a: ")))
#     print("New value for a:", a)
