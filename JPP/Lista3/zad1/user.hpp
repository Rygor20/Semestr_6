#ifndef USER_H
#define USER_H

#include <cstdint>
#include <optional>
#include <stdexcept>
#include "dhSetup.hpp"

template <typename T>
class User
{
private:
    uint64_t secret;
	DHSetup<T> setup;
	std::optional<T> key;
public:
    User(DHSetup<T> & newSetup) : setup(newSetup), secret(rand()){
    }

    ~User() {}

    T getPublicKey(){
		return setup.power(setup.getGenerator(), secret);
	}

    void setKey(T a){
        this->key = setup.power(a, secret);
    }

    T encrypt(T m){
        if (!key) {
			throw std::runtime_error("Key not set!");
		}
		return m * key.value();
    }

    T decrypt(T c){
        if (!key) {
			throw std::runtime_error("Key not set!");
		}
		return c / key.value();
    }
};


#endif