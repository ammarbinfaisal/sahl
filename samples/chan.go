package main

import "fmt"

func sendvals(c chan int, count int) {
	for i := 0; i < count; i++ {
		c <- i
	}
}

func recvvals(c chan int, count int) {
	for i := 0; i < count; i++ {
		fmt.Println(<-c)
	}
}

func main() {
	for i := 0; i < 1000; i++ {
		c := make(chan int)
		go sendvals(c, 100)
		go recvvals(c, 100)
	}
}
