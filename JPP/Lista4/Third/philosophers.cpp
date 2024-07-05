#include <iostream>
#include <thread>
#include <mutex>
#include <chrono>
#include <vector>
#include <random>

class DiningPhilosophers {
public:
    DiningPhilosophers(int n, int meals) : n(n), meals(meals), forks(n), waitGroup(n) {}

    void start() {
        std::vector<std::thread> threads;
        for (int i = 0; i < n; ++i) {
            threads.emplace_back(&DiningPhilosophers::handlePhilosopher, this, i);
        }
        for (auto& t : threads) {
            t.join();
        }
    }

private:
    int n;
    int meals;
    std::vector<std::mutex> forks;
    std::vector<std::thread::id> waitGroup;
    std::mutex printMutex; // Mutex for protecting standard output

    void handlePhilosopher(int i) {
        for (int meal = 0; meal < meals; ++meal) {
            // // Thinking
            // std::this_thread::sleep_for(std::chrono::milliseconds(rand() % 1000 + 200));

            // Pick up forks
            std::unique_lock<std::mutex> lock1(forks[i]);
            {
                std::lock_guard<std::mutex> guard(printMutex); // Lock before printing
                std::cout << "[" << i << "]" << " picked up fork " << i << std::endl;
            }
            std::unique_lock<std::mutex> lock2(forks[(i + 1) % n]);
            {
                std::lock_guard<std::mutex> guard(printMutex); // Lock before printing
                std::cout << "[" << i << "]" << " picked up fork " << (i + 1) % n << std::endl;
            }

            // Eating
            {
                std::lock_guard<std::mutex> guard(printMutex); // Lock before printing
                std::cout << "    [" << i << "]" << " starts eating" << std::endl;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(rand() % 1000 + 200));

            // Put down forks
            lock1.unlock();
            std::cout << "  [" << i << "]" << " put down fork " << i << std::endl;
            lock2.unlock();
            std::cout << "  [" << i << "]" << " put down fork " << (i + 1) % n << std::endl;

        }
    }
};

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Correct usage: ./dpProblem_asymmetric philosophers_count meals_to_eat" << std::endl;
        return 1;
    }

    int n = std::stoi(argv[1]);
    int meals = std::stoi(argv[2]);

    if (n < 2) {
        std::cerr << "Error: One fork per Philosopher, at least 2 Philosophers required" << std::endl;
        return 1;
    }

    if (meals == 0) {
        std::cerr << "Error: Philosophers are hungry! They need to eat at least one meal" << std::endl;
        return 1;
    }

    srand(time(NULL));
    DiningPhilosophers dp(n, meals);
    dp.start();

    return 0;
}
