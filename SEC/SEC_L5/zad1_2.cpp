#include <iostream>
#include <vector>
#include <string>
#include <random>
#include <iomanip>
#include <cctype>

std::vector<uint64_t> ksa(const std::vector<uint64_t>& key){
    std::vector<uint64_t> S(256);

    int key_length = key.size();
    for (int i = 0; i < 256; i++) {
        S[i] = i;
    }
    
    int j = 0;
    for (int i = 0; i < 256; i++) {
        j = (j + S[i] + key[i % key_length]) % 256;
        std::swap(S[i], S[j]);
    }

    return S;
}

std::vector<uint64_t> pseudo_random_keystream(std::vector<uint64_t>& S, int length) {
    std::vector<uint64_t> keystream(length);
    uint64_t i = 0, j = 0;

    for (uint64_t k = 0; k < length; k++) {
        i = (i + 1) % 256;
        j = (j + S[i]) % 256;
        std::swap(S[i], S[j]);
        keystream[k] = S[(S[i] + S[j]) % 256];
    }
    
    return keystream;
}

std::vector<uint64_t> rc4_crypt(const std::vector<uint64_t>& data, const std::vector<uint64_t>& key) {
    std::vector<uint64_t> S = ksa(key);
    std::vector<uint64_t> keystream = pseudo_random_keystream(S, data.size());
    
    // ^ denotes bitwise XOR operation
    std::vector<uint64_t> output(data.size());
    for (size_t i = 0; i < data.size(); i++) {
        output[i] = data[i] ^ keystream[i];
    }
    
    return output;
}

std::vector<uint64_t> string_to_bytes(const std::string& str) {
    return std::vector<uint64_t>(str.begin(), str.end());
}

std::string bytes_to_string(const std::vector<uint64_t>& bytes) {
    return std::string(bytes.begin(), bytes.end());
}

void print_hex(const std::vector<uint64_t>& data) {
    for (uint64_t byte : data) {
        std::cout << std::hex << std::setw(2) << std::setfill('0') << byte << " ";
    }
    std::cout << std::dec << std::endl;
}

// To check whether the same key was used we can use xor properties
// C1 ^ C2 = (M1 ^ S) ^ (M2 ^ S) = M1 ^ M2
bool same_key_used(const std::vector<uint64_t>& C1, const std::vector<uint64_t>& C2) {
    std::size_t min_len = std::min(C1.size(), C2.size());

    // Check whether result has any meaning in standard ASCII table
    for (std::size_t i = 0; i < min_len; ++i) {
        if ((C1[i] ^ C2[i]) >= 0x80) {
            return false;
        }
    }

    return true;
}

// Function to replace a letter with corresponding two digits
std::string replace_letters_with_numbers(char letter) {
    if (std::isdigit(letter)) {
        return std::string(1, letter);
    } else {
        return std::to_string(static_cast<int>(letter) - 55);
    }
}

// Helper function to calculate the checksum for the IBAN
uint8_t calculate_checksum(const std::string& bankNumber) {
    std::string rearranged_iban = bankNumber + "252100"; // 'PL00' converted and moved to the end

    std::string converted_iban;
    for (char c : rearranged_iban) {
        converted_iban += replace_letters_with_numbers(c);
    }

    size_t chunk_size = 9;
    size_t position = 0;
    unsigned long long remainder = 0;
    
    while (position < converted_iban.length()) {
        std::string chunk = std::to_string(remainder) + converted_iban.substr(position, chunk_size);
        remainder = std::stoull(chunk) % 97;
        position += chunk_size;
    }

    return 98 - remainder;
}

std::vector<std::string> gen_bank_numbers(size_t q) {
    std::vector<std::string> bank_numbers;

    uint8_t numery_rozliczeniowe[5][8] = {
        {1, 0, 2, 0, 0, 0, 0, 4}, // PKO BP
        {1, 0, 3, 0, 0, 0, 0, 1}, // Bank Handlowy
        {1, 1, 6, 0, 0, 0, 0, 4}, // Millennium
        {1, 2, 4, 0, 0, 0, 0, 3}, // Pekao S.A.
        {1, 0, 5, 0, 0, 0, 0, 2}  // ING
    };

    std::random_device rd;
    std::mt19937_64 rng(rd());

    for (const auto& nr : numery_rozliczeniowe) {
        for (size_t k = 0; k < q; ++k) {
            std::string bank_number;

            // Generate random client number
            uint8_t client_number[16];
            for (size_t i = 0; i < 16; ++i) {
                client_number[i] = rng() % 10;
            }

            // Construct bank number without checksum
            for (size_t i = 0; i < 8; ++i) {
                bank_number += std::to_string(nr[i]);
            }
            for (size_t i = 0; i < 16; ++i) {
                bank_number += std::to_string(client_number[i]);
            }

            // Calculate checksum
            uint8_t checksum = calculate_checksum(bank_number);

            // Construct full bank number with checksum
            std::string full_bank_number = std::to_string(checksum / 10) + std::to_string(checksum % 10) + bank_number;
            bank_numbers.push_back(full_bank_number);
        }
    }

    return bank_numbers;
}

// Function to verify the correctness of a Polish bank number
bool verify_bank_number(const std::string& bankNumber) {
    if (bankNumber.length() != 26) {
        return false;
    }

    std::string iban = "PL" + bankNumber;

    std::string rearranged_iban = iban.substr(4) + iban.substr(0, 4);

    std::string converted_iban;
    for (char c : rearranged_iban) {
        converted_iban += replace_letters_with_numbers(c);
    }

    size_t chunk_size = 9;
    size_t position = 0;
    unsigned long long remainder = 0;
    
    while (position < converted_iban.length()) {
        std::string chunk = std::to_string(remainder) + converted_iban.substr(position, chunk_size);
        remainder = std::stoull(chunk) % 97;
        position += chunk_size;
    }

    return remainder == 1;
}

int main() {
    std::string plaintext1, plaintext2, key1, key2;
    std::vector<uint64_t> plaintext1_bytes, plaintext2_bytes, key1_bytes, key2_bytes, cipher1, cipher2, decrypted1, decrypted2;
    std::vector<std::vector<uint64_t>> cryptograms;
    bool result;

    std::cout << "Enter first message to encrypt: ";
    std::getline(std::cin, plaintext1);

    std::cout << "Enter second message to encrypt: ";
    std::getline(std::cin, plaintext2);

    std::cout << "Enter the first key: ";
    std::getline(std::cin, key1);

    std::cout << "Enter the second key: ";
    std::getline(std::cin, key2);

    key1_bytes = string_to_bytes(key1);
    key2_bytes = string_to_bytes(key2);
    plaintext1_bytes = string_to_bytes(plaintext1);
    plaintext2_bytes = string_to_bytes(plaintext2);

    cipher1 = rc4_crypt(plaintext1_bytes, key1_bytes);
    cipher2 = rc4_crypt(plaintext2_bytes, key2_bytes);

    std::cout << "Ciphered message1 with key1: ";
    print_hex(cipher1);

    std::cout << "Ciphered message2 with key2: ";
    print_hex(cipher2);

    decrypted1 = rc4_crypt(cipher1, key1_bytes);
    decrypted2 = rc4_crypt(cipher2, key2_bytes);

    std::cout << "Decrypted text with key1: " << bytes_to_string(decrypted1) << std::endl;
    std::cout << "Decrypted text with key2: " << bytes_to_string(decrypted2) << std::endl;
    
    result = same_key_used(cipher1, cipher2);
    std::cout << "Same key used: " << (result ? "Yes" : "No") << std::endl;

    return 0;
}
