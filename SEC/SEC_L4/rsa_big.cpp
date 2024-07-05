#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <string>

#include "BigInt.hpp"

BigInt gcd_extended(BigInt a, BigInt b, BigInt& x, BigInt& y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }

    BigInt x1, y1;
    BigInt gcd = gcd_extended(b % a, a, x1, y1);

    x = y1 - (b / a) * x1;
    y = x1;

    return gcd;
}

BigInt modular_inverse(BigInt e, BigInt k) {
    BigInt x, y;
    BigInt gcd = gcd_extended(e, k, x, y);

    if (gcd != 1) {
        // Modular inverse does not exist
        return -1;
    } else {
        // Ensure d is positive
        BigInt d = (x % k + k) % k;
        return d;
    }
}


// można udoskonalić korzystając z testów probabilistycznych
// korzystając z małego twierdzenia fermata
// gdy p jest pierwsze wtedy a^(p-1) mod p = 1
// testujemy dla różnych wartości a
// powtarzamy dopóki nie jesteśmy wystarczająco pewni
bool is_prime(BigInt number){
    if(number <= 1){
        return false;
    }

    for(BigInt i = 2; i <= sqrt(number); i++){
        if(number % i == 0){
            return false;
        }
    }

    return true;
}

void generate_RSA(BigInt p, BigInt q, BigInt n, BigInt &e, BigInt &d) {
    BigInt random_number;
    std::string phi_string;

    BigInt phi = (p - 1) * (q - 1);

    std::cout << "Phi is: " << phi << std::endl;

    std::cout << "Calculating e" << std::endl;

    phi_string = phi.to_string();

    while(true){
        random_number = big_random(phi_string.length());

        if(gcd(random_number, phi) == 1){
            e = random_number;
            break;
        }
    }

    // d * e przystaje do 1 mod n, stąd:
    // extended gcd oblicza ax + by = gcd(a, b)
    // mamy więc, skoro e względnie pierwsze z phi, że zapisać możemy:
    // ex + phi(n)y = 1
    // biorą modulo phi(n) po obu stronach mamy
    // ex + 0 = 1 (mod phi(n))
    // ex = 1 (mod phi(n))
    // zatem szukając x z extended gcd dostaniemy inverse e
    std::cout << "Calculating d" << std::endl;
    d = modular_inverse(e, phi);
    std::cout << "Calculated d: " << d << std::endl;
}

BigInt mod_pow(BigInt base, BigInt exp, BigInt mod) {
    if(mod == 0){
        return BigInt(1);
    }

    BigInt result = 1;
    base %= mod;

    while (exp > 0) {
        if (exp % 2 == 1) {
            result = (result * base) % mod;
        }
        base = (base * base) % mod;
        exp = exp / 2;
    }
    
    return result;
}

// wiemy, że e_1 * d_1 przystaje do 1 mod phi(n)
// stąd istnieje k, że: e_1 * d_1 = k * phi(n) + 1 --> k * phi(n) = e_1 * d_1 - 1

// z twierdzenia eulera mamy że dla a i n względnie pierwszych zachodzi
// a^phi(n) przystaje do 1 (mod n)
// czyli a^(k * phi(n)) = (a^phi(n))^k = 1^k = 1 mod n

// wiemy, że k * phi(n) jest parzyste (bo phi(n)=(p-1)(q-1) jest parzyste)
// istnieją więc takie s i t, że k * phi(n) = 2^s * t
// czyli a^k*phi(n) = x^2 = 1 (mod n) dla pewnego x --> x^2 - 1 = (x-1)(x+1) jest dzielnikiem
std::pair<BigInt, BigInt> calculate_private(BigInt n, BigInt e, BigInt d){
    BigInt phi = d * e - 1;
    BigInt t = phi;
    BigInt a = 2;
    BigInt k, x, p, q;
    std::pair<BigInt, BigInt> result;

    while(t % 2 == 0){
        t = t / 2;
    }

    // Szukamy a i k takich że (a^k)^2 = 1 mod n
    while(a < 100){
        k = t;
        while(k < phi){
            x = mod_pow(a, k, n);

            if(x != 1 && x != (n-1) && mod_pow(x, 2, n) == 1){
                p = gcd(x-1, n);
                break;
            }
            k *= 2;
        }
        a += 2;
    }
    q = n / p;

    result.first = p;
    result.second = q;

    return result;
}

// Example prime numbers: 61979941769, 87461963471

int main(){
    BigInt p, q;
    BigInt dA, eA, dB, eB, n;
    std::pair<BigInt, BigInt> skA, pkA, skB, pkB;

    std::pair<BigInt, BigInt> private_result;

    std::cout << "Enter two prime numbers p and q:" << std::endl;
    std::cin >> p >> q;
    
    std::cout << "Checking if p and q prime" << std::endl;
    if (!is_prime(p) || !is_prime(q)) {
        std::cout << "p and q must be prime!" << std::endl;
        return 0;
    }

    n = p * q;

    std::cout << "Generating RSA for person A" << std::endl;
    generate_RSA(p, q, n, eA, dA);
    pkA.first = n;
    pkA.second = eA;
    skA.first = n;
    skA.second = dA;

    std::cout << "Generating RSA for person B" << std::endl;
    generate_RSA(p, q, n, eB, dB);
    pkB.first = n;
    pkB.second = eB;
    skB.first = n;
    skB.second = dB;

    std::cout << "Generating private key from algorithm" << std::endl;
    private_result = calculate_private(n, pkA.second, skA.second);

    std::cout << "Calculated p: " << private_result.first << " Calculated q: " << private_result.second << std::endl;
}