package main

import (
	"image/color"
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/vector"
)

type Game struct {
	liveCells []Coordinate
	frame     int
	isRunning bool
	prevSpace bool
	prevMouse bool
}

func (g *Game) Update() error {
	if ebiten.IsKeyPressed(ebiten.KeySpace) {
		if !g.prevSpace {
			g.isRunning = !g.isRunning
		}
		g.prevSpace = true
	} else {
		g.prevSpace = false
	}

	if g.isRunning {
		g.frame++
		if g.frame%10 == 0 {
			g.liveCells = gameStep(g.liveCells)
		}
	} else {
		isMousePressed := ebiten.IsMouseButtonPressed(ebiten.MouseButtonLeft)
		if isMousePressed && !g.prevMouse {
			x, y := ebiten.CursorPosition()
			cellX := (x - padding) / cellDimensions
			cellY := (y - padding) / cellDimensions

			if cellX >= 0 && cellX < rows && cellY >= 0 && cellY < columns {
				cell := Coordinate{X: cellX, Y: cellY}
				if g.cellExists(cell) {
					g.removeCell(cell)
				} else {
					g.liveCells = append(g.liveCells, cell)
				}
			}
		}
		g.prevMouse = isMousePressed
	}

	return nil
}

func (g *Game) cellExists(cell Coordinate) bool {
	for _, c := range g.liveCells {
		if c == cell {
			return true
		}
	}
	return false
}

func (g *Game) removeCell(cell Coordinate) {
	for i, c := range g.liveCells {
		if c == cell {
			g.liveCells = append(g.liveCells[:i], g.liveCells[i+1:]...)
			return
		}
	}
}

func (g *Game) Draw(screen *ebiten.Image) {
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

func (g *Game) Layout(outsideWidth, outsideHeight int) (int, int) {
	return windowWidth, windowHeight
}

func main() {
	initialLiveCells := []Coordinate{
		{X: 10, Y: 10}, {X: 11, Y: 10}, {X: 12, Y: 10},
	}

	game := &Game{liveCells: initialLiveCells}

	windowWidth = rows*cellDimensions + padding*2
	windowHeight = columns*cellDimensions + padding*2

	ebiten.SetWindowSize(windowWidth, windowHeight)
	ebiten.SetWindowTitle("Conway's Game of Life")
	if err := ebiten.RunGame(game); err != nil {
		log.Fatal(err)
	}
}
