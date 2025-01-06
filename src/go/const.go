package main

const (
	rows           int = 100
	columns        int = 100
	padding        int = 50
	cellDimensions int = 10
)

var (
	cellDimensionsFloat float32 = float32(cellDimensions)
	gridHeight          float32 = float32(columns) * cellDimensionsFloat
	gridWidth           float32 = float32(rows) * cellDimensionsFloat
	windowHeight        int     = columns*cellDimensions + padding
	windowWidth         int     = rows*cellDimensions + padding
)
