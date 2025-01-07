package main

type Cell struct {
	X, Y int
}

func cellInBounds(x, y int) bool {
	return x >= 0 && y >= 0 && x < columns && y < rows
}

func rangeInt(start, end int) []int {
	if start > end {
		return nil
	}
	return append([]int{start}, rangeInt(start+1, end)...)
}

func cartesianProduct(xs, ys []int) []Cell {
	if len(xs) == 0 {
		return nil
	}
	if len(ys) == 0 {
		return []Cell{}
	}
	head := xs[0]
	tail := xs[1:]
	return append(mapSlice(ys, func(y int) Cell {
		return Cell{X: head, Y: y}
	}), cartesianProduct(tail, ys)...)
}

func mapSlice(xs []int, f func(int) Cell) []Cell {
	if len(xs) == 0 {
		return nil
	}
	return append([]Cell{f(xs[0])}, mapSlice(xs[1:], f)...)
}

func filter(xs []Cell, predicate func(Cell) bool) []Cell {
	if len(xs) == 0 {
		return []Cell{}
	}
	head := xs[0]
	tail := xs[1:]
	if predicate(head) {
		return append([]Cell{head}, filter(tail, predicate)...)
	}
	return filter(tail, predicate)
}

func contains(xs []Cell, c Cell) bool {
	if len(xs) == 0 {
		return false
	}
	if xs[0] == c {
		return true
	}
	return contains(xs[1:], c)
}

func neighbors(x, y int) []Cell {
	return filter(cartesianProduct(
		rangeInt(x-1, x+1),
		rangeInt(y-1, y+1),
	), func(c Cell) bool {
		return (c.X != x || c.Y != y) && cellInBounds(c.X, c.Y)
	})
}

func allCells() []Cell {
	return cartesianProduct(rangeInt(0, rows-1), rangeInt(0, columns-1))
}

func gameStep(liveCells []Cell) []Cell {
	return filter(allCells(), func(cell Cell) bool {
		liveNeighborCount := len(filter(neighbors(cell.X, cell.Y), func(n Cell) bool {
			return contains(liveCells, n)
		}))

		if contains(liveCells, cell) {
			return liveNeighborCount == 2 || liveNeighborCount == 3
		}
		return liveNeighborCount == 3
	})
}
