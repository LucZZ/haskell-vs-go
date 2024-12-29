package main

type Coordinate struct {
	X, Y int
}

func cellInBounds(x, y int) bool {
	return x >= 0 && y >= 0 && x < columns && y < rows
}

func neighbors(x, y int) []Coordinate {
	var result []Coordinate
	for dx := -1; dx <= 1; dx++ {
		for dy := -1; dy <= 1; dy++ {
			nx, ny := x+dx, y+dy
			if (dx != 0 || dy != 0) && cellInBounds(nx, ny) {
				result = append(result, Coordinate{X: nx, Y: ny})
			}
		}
	}
	return result
}

func contains(liveCells []Coordinate, c Coordinate) bool {
	for _, cell := range liveCells {
		if cell == c {
			return true
		}
	}
	return false
}

func allCells() []Coordinate {
	var cells []Coordinate
	for x := 0; x < rows; x++ {
		for y := 0; y < columns; y++ {
			cells = append(cells, Coordinate{X: x, Y: y})
		}
	}
	return cells
}

func gameStep(liveCells []Coordinate) []Coordinate {
	var nextGen []Coordinate

	for x := 0; x < rows; x++ {
		for y := 0; y < columns; y++ {
			cell := Coordinate{X: x, Y: y}

			liveNeighborCount := 0
			for _, neighbor := range neighbors(cell.X, cell.Y) {
				if contains(liveCells, neighbor) {
					liveNeighborCount++
				}
			}

			if contains(liveCells, cell) { //Alive cell stays alive
				if liveNeighborCount == 2 || liveNeighborCount == 3 {
					nextGen = append(nextGen, cell)
				}
			} else if liveNeighborCount == 3 { //Dead cell comes alive
				nextGen = append(nextGen, cell)
			}
		}
	}

	return nextGen
}
