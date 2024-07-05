#ifndef DHSETUP_H
#define DHSETUP_H

#include <random>
#include <cstdint>
#include <cmath>

template <typename T>
class DHSetup
{
private:
    T generator;
    uint64_t modulo;

    bool check(uint64_t number) {
        uint64_t i = 2;
        uint64_t tmp = number;

        while (i * i <= tmp) {
            if (tmp % i == 0) {
                if (std::pow(number, (modulo - 1) / i) == 1) {
                    return false;
                }
                tmp /= i;
            } else {
                i++;
            }
        }
        if (tmp > 1 && std::pow(number, (modulo - 1) / tmp) == 1) {
            return false;
        }
        return true;
    }

public:
    DHSetup(uint64_t characteristic) {
        this->modulo = characteristic;

		std::random_device rd;
		std::mt19937 rng(rd());
		std::uniform_int_distribution<uint64_t> dist(1, modulo - 1);

		uint64_t g;
		do {
			g = dist(rng);
		} while (!check(g));

		this->generator = T(g);
	}

    ~DHSetup() {}

    T getGenerator(){
        return this->generator;
    }

    T power(T a, uint64_t b) {
		T result = T(1);
		while (b > 0) {
			if (b & 1) {
				result *= a;
			}
			a *= a;
			b >>= 1;
		}
		return result;
	}
};


#endif