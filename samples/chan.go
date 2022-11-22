package main

import (
	"fmt"
	"sync"
)

var wg sync.WaitGroup

func sendvals(c chan int, count int) {
	wg.Add(1)
	for i := 0; i < count; i++ {
		c <- i
	}
}

func recvvals(c chan int, count int) {
	for i := 0; i < count; i++ {
		fmt.Println(<-c)
	}
	wg.Done()
}

func main() {
	for i := 0; i < 100; i++ {
		c := make(chan int)
		go sendvals(c, 100)
		go recvvals(c, 100)
	}
	wg.Wait()
}
