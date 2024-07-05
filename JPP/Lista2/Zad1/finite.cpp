#include <stdio.h>
#include <stdexcept>
#include <iostream>

class FiniteField{
private:
    static const int characteristic = 1234577;

    int value;

    int to_range(int x) const {
        x %= characteristic;
        
        while (x < 0) {
            x += characteristic;
        }

        return x;
    }

    int invert(int x) const {
        for(int i = 0; i < characteristic; i++) {
            if((long long)x*i % characteristic == 1) {
                return i;
            }
        }
        return -1;
    }

public:
    FiniteField() : value(0) {}

    explicit FiniteField(int val) : value(to_range(val)) {}

    static const int get_characteristic() {
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
        
        int inv = invert(other.value);
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

int main(){
    FiniteField a(5);
    FiniteField b(1234576);
    FiniteField c(0);
    std::cout << "Given 'a': " << a << " Given 'b': " << b << std::endl;
    std::cout << "Characteristic: " << FiniteField::get_characteristic() << std::endl;

    std::cout << "a == b ? " << std::endl;
    if(a == b){
        std::cout << "\ta equals b" << std::endl;
    }
    else{
        std::cout << "\ta is not equal b" << std::endl;
    }

    std::cout << "a != b ? " << std::endl;
    if(a != b){
        std::cout << "\ta is not equal b" << std::endl;
    }
    else{
        std::cout << "\ta equals b" << std::endl;
    }

    std::cout << "a <= b ? " <<  std::endl;
    if(a <= b){
        std::cout << "\ta is less or equal than b" << std::endl;
    }
    else{
        std::cout << "\ta is bigger than b" << std::endl;
    }

    std::cout << "a >= b ? " << std::endl;
    if(a >= b){
        std::cout << "\ta is bigger or equal b" << std::endl;
    }
    else{
        std::cout << "\ta is less than b" << std::endl;
    }

    std::cout << "a < b ? " << std::endl;
    if(a < b){
        std::cout << "\ta is less than b" << std::endl;
    }
    else{
        std::cout << "\ta is bigger or equal b" << std::endl;
    }

    std::cout << "a > b ? " << std::endl;
    if(a > b){
        std::cout << "\ta is bigger than b" << std::endl;
    }
    else{
        std::cout << "\ta is less of equal b" << std::endl;
    }

    c = a + b;
    std::cout << "a + b = " << c << std::endl;

    c = a - b;
    std::cout << "a - b = " << c << std::endl;

    c = a * b;
    std::cout << "a * b = " << c << std::endl;

    c = a / b;
    std::cout << "a / b = " << c << std::endl;

    c += a;
    std::cout << "c += a : " << c << std::endl;

    c -= a;
    std::cout << "c -= a : " << c << std::endl;

    c *= a;
    std::cout << "c *= a : " << c << std::endl;

    c /= a;
    std::cout << "c /= a : " << c << std::endl;

    std::cout << "Enter a value for a: ";    
    std::cin >> a;
    std::cout << "New value for a: " << a << std::endl;

    return 0;
}