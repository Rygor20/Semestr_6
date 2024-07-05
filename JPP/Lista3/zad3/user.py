from typing import Generic, TypeVar, Callable
from dhsetup import DHSetup
import random

T = TypeVar("T")

class User(Generic[T]):

    def __init__(self, newSetup : DHSetup, field : Callable[..., T]):
        self.secret = random.randint(1, 1234567890)
        self.setup = newSetup
        self.field = field
        self.key : T = None

    def get_public_key(self) -> T:
        public : T = self.setup.power(self.setup.getGenerator(), self.secret)
        return public
        # pubKey: T = self.__setup.power(self.__setup.getGenerator(), self.__secret.getValue())
        # return pubKey

    def set_key(self, a : T):
        self.key = self.setup.power(a, self.secret)

    def encrypt(self, m : T):
        if self.key is None:
            raise RuntimeError("Key not set!")
        return m * self.key

    def decrypt(self, c):
        if self.key is None:
            raise RuntimeError("Key not set!")
        return c / self.key