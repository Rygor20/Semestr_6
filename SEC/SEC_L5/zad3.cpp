#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <string>
#include <random>
#include <algorithm>
#include <iomanip>
#include <cctype>

// uint8_t numery_rozliczeniowe[5][8] = {
//         {1, 0, 2, 0, 0, 0, 0, 4}, // PKO BP
//         {1, 1, 6, 0, 0, 0, 0, 1}, // Millenniu
//         {1, 2, 4, 0, 0, 0, 0, 4}, // Pekao SA
//         {1, 4, 7, 0, 0, 0, 0, 3}, // eurobank
//         {2, 4, 9, 0, 0, 0, 0, 2}  // Alior Bank
//     };

std::vector<std::string> numery_rozliczeniowe{
        "10200004",
        "11600001",
        "12400004",
        "14700003",
        "24900002"
    };

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

    std::random_device rd;
    std::mt19937 rng(rd());

    std::random_device rd_client;
    std::mt19937_64 rng_client(rd_client());

    for (const auto& nr : numery_rozliczeniowe) {
        for (size_t k = 0; k < q; ++k) {
            std::string bank_number;

            // Generate random client number
            uint8_t client_number[16];
            for (size_t i = 0; i < 16; ++i) {
                client_number[i] = rng_client() % 10;
            }

            bank_number += nr;

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

    std::shuffle(std::begin(bank_numbers), std::end(bank_numbers), rng);
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

// Generate the XOR table
std::unordered_map<int, std::vector<std::pair<int, int>>> generate_xor_table() {
    std::unordered_map<int, std::vector<std::pair<int, int>>> xorTable;
    for (int i = 0; i <= 9; ++i) {
        for (int j = 0; j <= 9; ++j) {
            int xorValue = i ^ j;
            xorTable[xorValue].emplace_back(i, j);
        }
    }
    return xorTable;
}

// Get possible digit pairs that result in a given XOR value
std::vector<std::pair<int, int>> get_possible_pairs(int xorResult,
    const std::unordered_map<int, std::vector<std::pair<int, int>>>& xorTable) {
    if (xorTable.find(xorResult) != xorTable.end()) {
        return xorTable.at(xorResult);
    }
    return {};
}

std::vector<std::string> intersection(const std::vector<std::string>& v1, const std::vector<std::string>& v2) {
    std::unordered_set<std::string> set(v1.begin(), v1.end());
    std::vector<std::string> result;

    for (const auto& str : v2) {
        if (set.find(str) != set.end()) {
            result.push_back(str);
        }
    }

    return result;
}

std::string calculateChecksum(const std::string& iban) {
    // Move checksum to end of IBAN
    std::string rearranged_iban = iban.substr(2) + iban.substr(0, 2);

    // Convert letters to digits
    for (char& c : rearranged_iban) {
        if (std::isalpha(c)) {
            c = (c & 31) + 9; // Convert 'A' to 10, 'B' to 11, ..., 'Z' to 35
        }
    }

    // Calculate modulo 97
    unsigned long long remainder = 0;
    for (char c : rearranged_iban) {
        remainder = (remainder * 10 + (c - '0')) % 97;
    }

    // Subtract from 98
    unsigned int checksum = 98 - remainder;

    // Format checksum
    return (checksum < 10 ? "0" : "") + std::to_string(checksum);
}

bool checkFirstTwoChars(const std::string& longString, char firstChar, char secondChar) {
    if(longString.length() < 2) // If the long string is less than 2 characters long
        return false;
    
    // Extract the first two characters from the long string
    std::string firstTwoChars = longString.substr(0, 2);

    // Check if the extracted characters match the given characters
    return (firstTwoChars[0] == firstChar && firstTwoChars[1] == secondChar);
}

int main() {
    std::string key1;
    std::vector<uint64_t> key1_bytes;
    std::vector<std::vector<uint64_t>> cryptograms;

    key1 = "kluczyk";
    key1_bytes = string_to_bytes(key1);

    auto bank_numbers = gen_bank_numbers(4);
    for (const auto& number : bank_numbers) {
        bool isValid = verify_bank_number(number);
        std::string checksum = calculateChecksum(number);
        std::cout << "The bank number " << number << " is " << (isValid ? "valid" : "invalid") << "." << std::endl;
    }

    for (const auto& bank_number : bank_numbers) {
        std::vector<uint64_t> cryptogram = rc4_crypt(string_to_bytes(bank_number), key1_bytes);
        cryptograms.push_back(cryptogram);
    }

    // Generate the XOR table
    auto xorTable = generate_xor_table();

    // XOR cryptograms pairwise and deduce possible digits
    std::vector<std::vector<std::pair<int, int>>> possible_pairs_for_positions;

    for (size_t j = 1; j < cryptograms.size(); ++j) {
        std::vector<std::pair<int, int>> current_pairs;

        for (size_t k = 0; k < cryptograms[0].size() && k < cryptograms[j].size(); ++k) {
            int xor_result = cryptograms[0][k] ^ cryptograms[j][k];
            auto possible_pairs = get_possible_pairs(xor_result, xorTable);

            if (j == 1) {
                // For the first pair comparison (1xor2), add all possible pairs to the list
                possible_pairs_for_positions.push_back(possible_pairs);
            } else {
                // For subsequent comparisons, filter existing pairs
                std::vector<std::pair<int, int>> filtered_pairs;
                for (const auto& pair : possible_pairs) {
                    for (const auto& existing_pair : possible_pairs_for_positions[k]) {
                        if (pair.first == existing_pair.first) {
                            filtered_pairs.push_back(pair);
                        }
                    }
                }
                possible_pairs_for_positions[k] = filtered_pairs;
            }
        }
    }

    std::vector<std::string> possible_banks;
    for (const auto& nr : numery_rozliczeniowe) {
        possible_banks.push_back(nr);
    }

    for (size_t k = 2; k < 10; ++k) {
        std::vector<std::string> current_possible_banks;

        for (const auto& pair : possible_pairs_for_positions[k]) {
            for (const auto& nr : numery_rozliczeniowe) {
                if(nr[k-2] - '0' == pair.first){
                    current_possible_banks.push_back(nr);
                }
            }
        }

        possible_banks = intersection(possible_banks, current_possible_banks);
    }

    std::cout << "Filtered possible banks:" << std::endl;
    for (const auto& bank : possible_banks) {
        std::cout << bank << std::endl;
    }

    // Print the final possible pairs for the first bank number
    std::cout << "Final possible pairs for the first bank number:" << std::endl;
    for (size_t k = 0; k < possible_pairs_for_positions.size(); ++k) {
        std::cout << "Position " << k + 1 << ": ";
        for (const auto& pair : possible_pairs_for_positions[k]) {
            std::cout << "(" << pair.first << ", " << pair.second << ") ";
        }
        std::cout << std::endl;
    }

    std::random_device rd;
    std::mt19937 gen(rd());

    bool not_found = true;

    std::vector<std::string> found_options;

    while(not_found)
    {
        for(const auto& bank : possible_banks){
            std::string string_number;
            std::pair<int, int> random_pair;

            for (size_t k = 0; k < 2; ++k) {
                std::uniform_int_distribution<> distr_pair(0, possible_pairs_for_positions[k].size() - 1);
                int random_pair_index = distr_pair(gen);

                // Choose a random pair from the vector
                std::pair<int, int> random_pair = possible_pairs_for_positions[k][random_pair_index];


                std::uniform_int_distribution<> distr_element(0, 1);
                int random_element_index = distr_element(gen);

                // Choose either the first or second element from the random pair
                int random_element = random_element_index == 0 ? random_pair.first : random_pair.second;

                string_number += std::to_string(random_element);
            }

            string_number += bank;

            for (size_t k = 10; k < possible_pairs_for_positions.size(); ++k) {
                std::uniform_int_distribution<> distr_pair(0, possible_pairs_for_positions[k].size() - 1);
                int random_pair_index = distr_pair(gen);

                // Choose a random pair from the vector
                std::pair<int, int> random_pair = possible_pairs_for_positions[k][random_pair_index];


                std::uniform_int_distribution<> distr_element(0, 1);
                int random_element_index = distr_element(gen);

                // Choose either the first or second element from the random pair
                int random_element = random_element_index == 0 ? random_pair.first : random_pair.second;

                string_number += std::to_string(random_element);
            }

            // std::cout << "Generated number: " << string_number << std::endl;

            if(verify_bank_number(string_number)){
                std::string checksum = calculateChecksum(string_number);

                if(checkFirstTwoChars(string_number, checksum[0], checksum[1])){
                    std::cout << "Generated correct bank number: " << string_number << std::endl;
                    //std::cout << "Calculated checksum: " << checksum << std::endl;
                    not_found = false;
                }
            }
        }
    }

    return 0;
}
