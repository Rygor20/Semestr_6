from finite import FiniteField
from dhsetup import DHSetup
from user import User

def main() -> None:
    message: FiniteField = FiniteField(1337)

    modulo: int = 1234567891
    setup: DHSetup = DHSetup(modulo, FiniteField)

    user1: User = User(setup, FiniteField)
    user2: User = User(setup, FiniteField)

    user2.set_key(user1.get_public_key())
    user1.set_key(user2.get_public_key())

    print(f"Finite Galois field set to: 1234567891")

    print(f"Generator: {setup.getGenerator()}")

    print(f"Message: {message}")

    cipher1: FiniteField = user1.encrypt(message)
    cipher2: FiniteField = user2.encrypt(message)

    print(f"Encrypted message: {cipher1}")

    result1: FiniteField = user1.decrypt(cipher2)
    result2: FiniteField = user2.decrypt(cipher1)

    print(f"Decrypted user1 : {result1}")
    print(f"Decrypted user2 : {result2}")

if __name__ == "__main__":
    main()