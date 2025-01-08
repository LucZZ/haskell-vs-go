package main

import (
	"image/color"
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/vector"
)

type GameState struct {
	liveCells []Cell
	frame     int
	isRunning bool

	prevSpace bool
	prevMouse bool
}

func (g *GameState) Draw(screen *ebiten.Image) {
	screen.Fill(color.White)

	gridColor := color.Black
	drawVerticalLines(screen, columns, gridColor)
	drawHorizontalLines(screen, rows, gridColor)

	cellColor := color.Black
	drawCells(screen, g.liveCells, cellColor)
}

func updateFrame(frame int, cells []Cell, isRunning bool) (int, []Cell) {
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

func cellExists(cells []Cell, cell Cell) bool {
	if len(cells) == 0 {
		return false
	}
	if cells[0] == cell {
		return true
	}
	return cellExists(cells[1:], cell)
}

func removeCell(cells []Cell, cell Cell) []Cell {
	if len(cells) == 0 {
		return []Cell{}
	}
	if cells[0] == cell {
		return removeCell(cells[1:], cell)
	}
	return append([]Cell{cells[0]}, removeCell(cells[1:], cell)...)
}

func drawVerticalLines(screen *ebiten.Image, index int, color color.Color) {
	if index < 0 {
		return
	}
	x := float32(padding + index*cellDimensions)
	vector.StrokeLine(screen, x, float32(padding), x, float32(padding)+gridHeight, 1, color, false)
	drawVerticalLines(screen, index-1, color)
}

func drawHorizontalLines(screen *ebiten.Image, index int, color color.Color) {
	if index < 0 {
		return
	}
	y := float32(padding + index*cellDimensions)
	vector.StrokeLine(screen, float32(padding), y, float32(padding)+gridWidth, y, 1, color, false)
	drawHorizontalLines(screen, index-1, color)
}

func drawCells(screen *ebiten.Image, cells []Cell, color color.Color) {
	if len(cells) == 0 {
		return
	}
	cell := cells[0]
	x := float64(padding + cell.X*cellDimensions)
	y := float64(padding + cell.Y*cellDimensions)

	rect := ebiten.NewImage(cellDimensions, cellDimensions)
	rect.Fill(color)

	opts := &ebiten.DrawImageOptions{}
	opts.GeoM.Translate(x, y)
	screen.DrawImage(rect, opts)

	drawCells(screen, cells[1:], color)
}

func handleMouseInput(cells []Cell, prevMouse bool, isMousePressed bool, cursorPosition func() (int, int)) ([]Cell, bool) {
	if isMousePressed && !prevMouse {
		x, y := cursorPosition()
		cellX := (x - padding) / cellDimensions
		cellY := (y - padding) / cellDimensions

		if cellX >= 0 && cellX < columns && cellY >= 0 && cellY < rows {
			cell := Cell{X: cellX, Y: cellY}
			if cellExists(cells, cell) {
				cells = removeCell(cells, cell)
			} else {
				cells = append(cells, cell)
			}
		}
	}

	return cells, isMousePressed
}

func (g *GameState) Update() error {
	g.isRunning, g.prevSpace = updateRunningState(g.isRunning, g.prevSpace, ebiten.IsKeyPressed(ebiten.KeySpace))

	g.liveCells, g.prevMouse = handleMouseInput(g.liveCells, g.prevMouse, ebiten.IsMouseButtonPressed(ebiten.MouseButtonLeft), ebiten.CursorPosition)

	g.frame, g.liveCells = updateFrame(g.frame, g.liveCells, g.isRunning)

	return nil
}

func (g *GameState) Layout(outsideWidth, outsideHeight int) (int, int) {
	return windowWidth, windowHeight
}

func main() {
	initialLiveCells := []Cell{
		{X: 1, Y: 1}, {X: 2, Y: 2}, {X: 0, Y: 3}, {X: 1, Y: 3}, {X: 2, Y: 3},
	}

	game := &GameState{liveCells: initialLiveCells, isRunning: false, prevSpace: false, prevMouse: false}

	ebiten.SetWindowSize(windowWidth, windowHeight)
	ebiten.SetWindowTitle("Conway's Game of Life")
	if err := ebiten.RunGame(game); err != nil {
		log.Fatal(err)
	}
}
