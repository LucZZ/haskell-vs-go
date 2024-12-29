package main

const (
	rows           int = 100
	columns        int = 100
	padding        int = 100
	cellDimensions int = 10
)

var (
	cellDimensionsFloat float64 = float64(cellDimensions)
	gridHeight          float64 = float64(columns) * cellDimensionsFloat
	gridWidth           float64 = float64(rows) * cellDimensionsFloat
	windowHeight        int     = columns*cellDimensions + padding
	windowWidth         int     = rows*cellDimensions + padding
)
