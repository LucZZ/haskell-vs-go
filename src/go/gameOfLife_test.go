package main

import (
	"reflect"
	"testing"
)

func TestCellInBounds(t *testing.T) {

	tests := []struct {
		name           string
		x, y           int
		expectedResult bool
	}{
		{"Inside bounds", 2, 3, true},
		{"Outside bounds (negative x)", -1, 3, false},
		{"Outside bounds (negative y)", 3, -1, false},
		{"Outside bounds (x too large)", columns, 3, false},
		{"Outside bounds (y too large)", 3, rows, false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := cellInBounds(test.x, test.y)
			if result != test.expectedResult {
				t.Errorf("cellInBounds(%d, %d) = %v; want %v", test.x, test.y, result, test.expectedResult)
			}
		})
	}
}

func TestRangeInt(t *testing.T) {
	tests := []struct {
		name           string
		start, end     int
		expectedResult []int
	}{
		{"Valid range", 1, 5, []int{1, 2, 3, 4, 5}},
		{"Single element range", 3, 3, []int{3}},
		{"Start greater than end", 5, 3, nil},
		{"Negative range", -2, 2, []int{-2, -1, 0, 1, 2}},
		{"Empty range", 0, -1, nil},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := rangeInt(test.start, test.end)
			if !reflect.DeepEqual(result, test.expectedResult) {
				t.Errorf("rangeInt(%d, %d) = %v; want %v", test.start, test.end, result, test.expectedResult)
			}
		})
	}
}

func TestCartesianProduct(t *testing.T) {
	tests := []struct {
		name           string
		xs, ys         []int
		expectedResult []Coordinate
	}{
		{"Non-empty xs and ys", []int{1, 2}, []int{3, 4}, []Coordinate{
			{X: 1, Y: 3}, {X: 1, Y: 4},
			{X: 2, Y: 3}, {X: 2, Y: 4},
		}},
		{"Empty xs", []int{}, []int{3, 4}, nil},
		{"Empty ys", []int{1, 2}, []int{}, []Coordinate{}},
		{"Both empty", []int{}, []int{}, nil},
		{"Single element in xs and ys", []int{1}, []int{2}, []Coordinate{
			{X: 1, Y: 2},
		}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := cartesianProduct(test.xs, test.ys)
			if !equalCoordinates(result, test.expectedResult) {
				t.Errorf("cartesianProduct(%v, %v) = %v; want %v", test.xs, test.ys, result, test.expectedResult)
			}
		})
	}
}

func TestMapSlice(t *testing.T) {
	xs := []int{1, 2, 3}
	transform := func(x int) Coordinate {
		return Coordinate{X: x, Y: x * 2}
	}
	expected := []Coordinate{
		{X: 1, Y: 2},
		{X: 2, Y: 4},
		{X: 3, Y: 6},
	}

	result := mapSlice(xs, transform)

	if len(result) != len(expected) {
		t.Errorf("mapSlice(%v) returned %v, want %v", xs, result, expected)
		return
	}

	for i := range result {
		if result[i] != expected[i] {
			t.Errorf("mapSlice(%v) = %v, want %v", xs, result, expected)
			return
		}
	}
}

func TestFilter(t *testing.T) {
	tests := []struct {
		name           string
		xs             []Coordinate
		predicate      func(Coordinate) bool
		expectedResult []Coordinate
	}{
		{
			name: "Filter coordinates with X > 0",
			xs: []Coordinate{
				{X: 1, Y: 2},
				{X: -1, Y: 3},
				{X: 4, Y: -2},
			},
			predicate: func(c Coordinate) bool {
				return c.X > 0
			},
			expectedResult: []Coordinate{
				{X: 1, Y: 2},
				{X: 4, Y: -2},
			},
		},
		{
			name: "Filter coordinates with Y > 0",
			xs: []Coordinate{
				{X: 1, Y: 2},
				{X: -1, Y: -3},
				{X: 4, Y: 0},
			},
			predicate: func(c Coordinate) bool {
				return c.Y > 0
			},
			expectedResult: []Coordinate{
				{X: 1, Y: 2},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := filter(test.xs, test.predicate)

			if len(result) != len(test.expectedResult) {
				t.Errorf("filter(%v) = %v, want %v", test.xs, result, test.expectedResult)
				return
			}

			for i := range result {
				if result[i] != test.expectedResult[i] {
					t.Errorf("filter(%v) = %v, want %v", test.xs, result, test.expectedResult)
					return
				}
			}
		})
	}
}

func TestContains(t *testing.T) {
	tests := []struct {
		name           string
		xs             []Coordinate
		c              Coordinate
		expectedResult bool
	}{
		{
			name:           "Coordinate is present",
			xs:             []Coordinate{{X: 1, Y: 2}, {X: -1, Y: 3}, {X: 4, Y: -2}},
			c:              Coordinate{X: -1, Y: 3},
			expectedResult: true,
		},
		{
			name:           "Coordinate is not present",
			xs:             []Coordinate{{X: 1, Y: 2}, {X: -1, Y: 3}, {X: 4, Y: -2}},
			c:              Coordinate{X: 0, Y: 0},
			expectedResult: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := contains(tt.xs, tt.c)
			if result != tt.expectedResult {
				t.Errorf("contains(%v, %v) = %v, want %v", tt.xs, tt.c, result, tt.expectedResult)
			}
		})
	}
}

func TestGameStep(t *testing.T) {
	tests := []struct {
		name           string
		liveCells      []Coordinate
		expectedResult []Coordinate
	}{
		{
			name: "Still Life (Block)",
			liveCells: []Coordinate{
				{X: 1, Y: 1}, {X: 1, Y: 2},
				{X: 2, Y: 1}, {X: 2, Y: 2},
			},
			expectedResult: []Coordinate{
				{X: 1, Y: 1}, {X: 1, Y: 2},
				{X: 2, Y: 1}, {X: 2, Y: 2},
			},
		},
		{
			name: "Oscillator (Blinker)",
			liveCells: []Coordinate{
				{X: 2, Y: 1}, {X: 2, Y: 2}, {X: 2, Y: 3},
			},
			expectedResult: []Coordinate{
				{X: 1, Y: 2}, {X: 2, Y: 2}, {X: 3, Y: 2},
			},
		},
		{
			name:           "Empty Board",
			liveCells:      []Coordinate{},
			expectedResult: []Coordinate{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gameStep(tt.liveCells)
			if !equalCoordinates(result, tt.expectedResult) {
				t.Errorf("gameStep(%v) = %v, want %v", tt.liveCells, result, tt.expectedResult)
			}
		})
	}
}

// Helper methods
func equalCoordinates(a, b []Coordinate) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
