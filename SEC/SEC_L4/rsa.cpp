#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <random>

std::random_device rd;
std::mt19937 gen(rd());

uint64_t gcd_extended(uint64_t a, uint64_t b, uint64_t& x, uint64_t& y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }

    uint64_t x1, y1;
    uint64_t gcd = gcd_extended(b % a, a, x1, y1);

    x = y1 - (b / a) * x1;
    y = x1;

    return gcd;
}

uint64_t modular_inverse(uint64_t e, uint64_t k) {
    uint64_t x, y;
    uint64_t gcd = gcd_extended(e, k, x, y);

    if (gcd != 1) {
        // Modular inverse does not exist
        return -1;
    } else {
        // Ensure d is positive
        uint64_t d = (x % k + k) % k;
        return d;
    }
}

bool is_prime(uint64_t number){
    if(number <= 1){
        return false;
    }

    for(uint64_t i = 2; i <= sqrt(number); i++){
        if(number % i == 0){
            return false;
        }
    }

    return true;
}

void generate_RSA(uint64_t p, uint64_t q, uint64_t n, uint64_t &e, uint64_t &d) {
    uint64_t random_number;

    uint64_t phi = (p - 1) * (q - 1);

    std::cout << "Phi is: " << phi << std::endl;

    std::cout << "Calculating e" << std::endl;
    std::uniform_real_distribution<> range(std::floor(phi/4), std::floor((3 * phi)/4));
    while(true){
        random_number = range(gen);

        if(std::__gcd(random_number, phi) == 1){
            e = random_number;
            break;
        }
    }

    std::cout << "Calculating d" << std::endl;
    d = modular_inverse(e, phi);
}

uint64_t mod_pow(uint64_t base, uint64_t exp, uint64_t mod) {
    uint64_t result = 1;
    base %= mod;
    while (exp > 0) {
        if (exp & 1) {
            result = (result * base) % mod;
        }
        base = (base * base) % mod;
        exp >>= 1;
    }
    return result;
}

std::pair<uint64_t, uint64_t> calculate_private(uint64_t n, uint64_t e, uint64_t d){
    uint64_t phi = d * e - 1;
    uint64_t t = phi;
    uint64_t a = 2;
    uint64_t k, x, p, q;
    std::pair<uint64_t, uint64_t> result;

    while(t % 2 == 0){
        t = std::floor(t / 2);
    }

    // Szukamy a i k takich że (a^k)^2 = 1 mod n
    while(a < 100){
        k = t;
        while(k < phi){
            x = mod_pow(a, k, n);

            if(x != 1 && x != (n-1) && mod_pow(x, 2, n) == 1){
                p = std::gcd(x-1, n);
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

// Example prime numbers: 10631, 15299

int main(){
    uint64_t p, q;
    uint64_t dA, eA, dB, eB, n;
    std::pair<uint64_t, uint64_t> skA, pkA, skB, pkB;

    std::pair<uint64_t, uint64_t> private_result;

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

    private_result = calculate_private(n, pkA.second, skA.second);

    std::cout << "Calculated p: " << private_result.first << " Calculated q: " << private_result.second << std::endl;
}