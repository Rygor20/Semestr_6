from typing import Generic, TypeVar, Callable

import random

T = TypeVar("T")

class DHSetup(Generic[T]):

    def __init__(self, characteristic, field : Callable[..., T]):
        self.modulo = characteristic
        self.field = field
        self.generator : T = self.find_generator()

    def check(self, number):
        tmp = number
        i = 2
        while i * i <= tmp:
            if tmp % i == 0:
                if pow(number, (self.modulo - 1) // i, self.modulo) == 1:
                    return False
                tmp //= i
            else:
                i += 1
        if tmp > 1 and pow(number, (self.modulo - 1) // tmp, self.modulo) == 1:
            return False
        return True

    def find_generator(self) -> T:
        while True:
            g = random.randint(1, self.modulo - 1)
            if self.check(g):
                return self.field(g)

    def power(self, a : T, b) -> T:
        result : T = self.field(1)
        while b > 0:
            if b & 1:
                result = result * a
            a = a * a
            b >>= 1
        return result
    
    def getGenerator(self) -> T:
        return self.generator