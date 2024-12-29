package main

import (
	"fmt"
)

func main() {

	liveCells := []Coordinate{
		{X: 1, Y: 2}, {X: 2, Y: 2}, {X: 3, Y: 2},
	}

	nextGen := gameStep(liveCells)

	fmt.Printf("Test: %v", nextGen)
}
