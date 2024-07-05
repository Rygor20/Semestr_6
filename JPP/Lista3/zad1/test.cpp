#include <iostream>
#include "dhSetup.hpp"
#include "user.hpp"
#include "finiteField.hpp"

int main(int argc, char const *argv[])
{
    FiniteField m(1337);

    uint64_t modulo = 1234567891;
    DHSetup<FiniteField> setup = DHSetup<FiniteField>(modulo);

    User<FiniteField> user1(setup);
    User<FiniteField> user2(setup);

    user1.setKey(user2.getPublicKey());
    user2.setKey(user1.getPublicKey());

    std::cout << "Finite Galois field set to: 1234567891" << std::endl;

    std::cout << "Generator: " << setup.getGenerator() << std::endl;

    std::cout << "Message: " << m << "\n";

    FiniteField cipher1 = user1.encrypt(m);
    FiniteField cipher2 = user2.encrypt(m);

    FiniteField result1 = user1.decrypt(cipher2);
    FiniteField result2 = user2.decrypt(cipher1);

    if(cipher1 == cipher2){
        std::cout << "Encrypted message: " << cipher1 << std::endl;
    }

    std::cout << "Decrypted user1: " << result1 << std::endl;
    std::cout << "Decrypted user2: " << result2 << std::endl;

    return 0;
}
