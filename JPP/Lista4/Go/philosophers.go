package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

var (
	n         int
	forks     []chan bool
	waitGroup sync.WaitGroup
	meals     int
)

func validateArg(arg string, name string) (int, error) {
	val, err := strconv.Atoi(arg)
	if err != nil {
		return 0, fmt.Errorf("Error: %s is not an integer", name)
	}
	return val, nil
}

func main() {
	if len(os.Args) != 3 {
		fmt.Println("Correct usage: go run philosophers.go philosophers_count meals_to_eat")
		return
	}

	var err error
	if n, err = validateArg(os.Args[1], "n"); err != nil {
		fmt.Println(err)
		return
	}

	if meals, err = validateArg(os.Args[2], "meals"); err != nil {
		fmt.Println(err)
		return
	}

	if n < 2 {
		fmt.Println("Error: One fork per Philosopher, at least 2 Philosophers required")
		return
	}

	if meals == 0 {
		fmt.Println("Error: Philosophers are hungry! They need to eat at least one meal")
		return
	}

	forks = make([]chan bool, n)
	for i := 0; i < n; i++ {
		forks[i] = make(chan bool, 1)
	}

	for i := 0; i < n; i++ {
		waitGroup.Add(1)
		go HandlePhilosopher(&waitGroup, i)
	}

	waitGroup.Wait()
}

func HandlePhilosopher(waitGroup *sync.WaitGroup, i int) {
	defer waitGroup.Done()

	for meal := 0; meal < meals; meal++ {
		// Thinking
		thinkingTime := time.Duration(rand.Intn(801) + 200)
		time.Sleep(thinkingTime * time.Millisecond)

		// Pick up Forks but consider parity of i variable
		if i%2 == 0 {
			fmt.Printf("[%d] is picking up fork %d\n", i, i)
			forks[i] <- true
			fmt.Printf("[%d] is picking up fork %d\n", i, (i+1)%n)
			forks[(i+1)%n] <- true
		} else {
			fmt.Printf("[%d] is picking up fork %d\n", i, (i+1)%n)
			forks[(i+1)%n] <- true
			fmt.Printf("[%d] is picking up fork %d\n", i, i)
			forks[i] <- true
		}

		// Eating
		fmt.Printf("    [%d] starts eating\n", i)
		eatingTime := time.Duration(rand.Intn(801) + 200)
		time.Sleep(eatingTime * time.Millisecond)

		// Put down Forks
		fmt.Printf("  [%d] is putting down fork %d\n", i, i)
		<-forks[i]
		fmt.Printf("  [%d] is putting down fork %d\n", i, (i+1)%n)
		<-forks[(i+1)%n]
	}
}
