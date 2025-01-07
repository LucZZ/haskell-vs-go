package main

import (
	"image/color"
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/vector"
)

type GameState struct {
	liveCells []Coordinate
	frame     int
	isRunning bool

	prevSpace bool
	prevMouse bool
}

func (g *GameState) Update() error {
	g.isRunning, g.prevSpace = updateRunningState(g.isRunning, g.prevSpace, ebiten.IsKeyPressed(ebiten.KeySpace))

	g.liveCells, g.prevMouse = handleMouseInput(g.liveCells, g.prevMouse, ebiten.IsMouseButtonPressed(ebiten.MouseButtonLeft), ebiten.CursorPosition)

	g.frame, g.liveCells = updateFrame(g.frame, g.liveCells, g.isRunning)

	return nil
}

func handleMouseInput(cells []Coordinate, prevMouse bool, isMousePressed bool, cursorPosition func() (int, int)) ([]Coordinate, bool) {
	if isMousePressed && !prevMouse {
		x, y := cursorPosition()
		cellX := (x - padding) / cellDimensions
		cellY := (y - padding) / cellDimensions

		if cellX >= 0 && cellX < rows && cellY >= 0 && cellY < columns {
			cell := Coordinate{X: cellX, Y: cellY}
			if cellExists(cells, cell) {
				cells = removeCell(cells, cell)
			} else {
				cells = append(cells, cell)
			}
		}
	}

	return cells, isMousePressed
}

func updateFrame(frame int, cells []Coordinate, isRunning bool) (int, []Coordinate) {
	if !isRunning {
		return frame, cells
	}
	newFrame := frame + 1
	if newFrame%10 == 0 {
		cells = gameStep(cells)
	}
	return newFrame, cells
}

func updateRunningState(isRunning, prevSpace, isKeyPressed bool) (bool, bool) {
	newRunning := isRunning
	if !prevSpace && isKeyPressed {
		newRunning = !isRunning
	}
	return newRunning, isKeyPressed
}

func cellExists(cells []Coordinate, cell Coordinate) bool {
	if len(cells) == 0 {
		return false
	}
	if cells[0] == cell {
		return true
	}
	return cellExists(cells[1:], cell)
}

func removeCell(cells []Coordinate, cell Coordinate) []Coordinate {
	if len(cells) == 0 {
		return []Coordinate{}
	}
	if cells[0] == cell {
		return removeCell(cells[1:], cell)
	}
	return append([]Coordinate{cells[0]}, removeCell(cells[1:], cell)...)
}

func (g *GameState) Draw(screen *ebiten.Image) {
	screen.Fill(color.White)

	gridColor := color.Black
	for x := 0; x <= rows; x++ {
		startX := float32(padding + x*cellDimensions)
		startY := float32(padding)
		endX := startX
		endY := float32(padding) + gridHeight
		vector.StrokeLine(screen, startX, startY, endX, endY, 1, gridColor, false)
	}
	for y := 0; y <= columns; y++ {
		startX := float32(padding)
		startY := float32(padding + y*cellDimensions)
		endX := float32(padding) + gridWidth
		endY := startY
		vector.StrokeLine(screen, startX, startY, endX, endY, 1, gridColor, false)
	}

	cellColor := color.Black
	for _, cell := range g.liveCells {
		x := float64(padding + cell.X*cellDimensions)
		y := float64(padding + cell.Y*cellDimensions)
		rect := ebiten.NewImage(cellDimensions, cellDimensions)
		rect.Fill(cellColor)

		opts := &ebiten.DrawImageOptions{}
		opts.GeoM.Translate(x, y)
		screen.DrawImage(rect, opts)
	}
}

func (g *GameState) Layout(outsideWidth, outsideHeight int) (int, int) {
	return windowWidth, windowHeight
}

func main() {
	initialLiveCells := []Coordinate{
		{X: 1, Y: 1}, {X: 2, Y: 2}, {X: 0, Y: 3}, {X: 1, Y: 3}, {X: 2, Y: 3},
	}

	game := &GameState{liveCells: initialLiveCells, isRunning: false, prevSpace: false, prevMouse: false}

	ebiten.SetWindowSize(windowWidth, windowHeight)
	ebiten.SetWindowTitle("Conway's Game of Life")
	if err := ebiten.RunGame(game); err != nil {
		log.Fatal(err)
	}
}
