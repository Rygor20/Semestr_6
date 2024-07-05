#ifndef FINITEFIELD_H
#define FINITEFIELD_H

#include <stdio.h>
#include <stdexcept>
#include <iostream>

class FiniteField{
private:
    static const uint64_t characteristic = 1234567891;

    uint64_t value;

    uint64_t to_range(uint64_t x) const {
        x %= characteristic;
        
        while (x < 0) {
            x += characteristic;
        }

        return x;
    }

    uint64_t invert(uint64_t x) const {
        for(uint64_t i = 0; i < characteristic; i++) {
            if((long long)x*i % characteristic == 1) {
                return i;
            }
        }
        return -1;
    }

public:
    FiniteField() : value(0) {}

    explicit FiniteField(uint64_t val) : value(to_range(val)) {}

    static const uint64_t get_characteristic() {
        return characteristic;
    }

    bool operator==(const FiniteField& other) const {
        return value == other.value;
    }

    bool operator!=(const FiniteField& other) const {
        return !(*this == other);
    }

    bool operator<=(const FiniteField& other) const {
        return value <= other.value;
    }

    bool operator>=(const FiniteField& other) const {
        return value >= other.value;
    }

    bool operator<(const FiniteField& other) const {
        return value < other.value;
    }

    bool operator>(const FiniteField& other) const {
        return value > other.value;
    }

    FiniteField operator+(const FiniteField& other) const {
        return FiniteField(value + other.value);
    }

    FiniteField operator-(const FiniteField& other) const {
        return FiniteField(value - other.value);
    }

    FiniteField operator*(const FiniteField& other) const {
        return FiniteField(value * other.value);
    }

    FiniteField operator/(const FiniteField& other) const {
        if(other.value == 0) {
            throw std::runtime_error("Error: Dividing by zero");
        }
        
        uint64_t inv = invert(other.value);
        if(inv == -1) {
            throw std::runtime_error("Error: Irreversible element");
        }
        return FiniteField((long long)value*inv % characteristic);
    }

    FiniteField& operator=(const FiniteField& other) {
        value = other.value;
        return *this;
    }

    FiniteField& operator+=(const FiniteField& other) {
        value = to_range(value + other.value);
        return *this;
    }

    FiniteField& operator-=(const FiniteField& other) {
        value = to_range(value - other.value);
        return *this;
    }

    FiniteField& operator*=(const FiniteField& other) {
        value = to_range(value * other.value);
        return *this;
    }

    FiniteField& operator/=(const FiniteField& other) {
        value = to_range((*this / other).value);
        return *this;
    }

    friend std::ostream& operator<<(std::ostream& os, const FiniteField& field) {
        return os << field.value;
    }

    friend std::istream& operator>>(std::istream& is, FiniteField& field) {
        return is >> field.value;
        //return is;
    }
};

#endif
