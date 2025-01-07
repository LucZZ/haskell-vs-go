# haskell-vs-go: A Functional Perspective on Conway's Game of Life

This project explores the functional programming aspects of Haskell and Go by implementing Conway's Game of Life in both languages.

## Project Overview
- **Haskell Implementation**:
  - Built with [**Cabal**](https://github.com/haskell/cabal) for build and package management and [**Gloss**](https://github.com/benl23x5/gloss) for UI elements

- **Go Implementation**:
  - Build with **Go** and [**Ebitenengine V2**](https://github.com/hajimehoshi/ebiten) for UI elements

## How to Build and Run

### Haskell (src\haskell)
1. Ensure GHC and Cabal is installed
2. Build the project:
   ```bash
   cabal build
   ```
3. Run the project:
   ```bash
   cabal run
   ```
4. Usage: 
    - Left click to place or delete a cell
    - Space bar to resume or pause the simulation
5. There is a known bug for windows on "cabal run": 
    ```
    haskell: user error (unknown GLUT entry glutInit)
    ```
    This happens because GLUT is not correctly configured by Cabal and Gloss.
    
    A easy solution for this is to place the "glut32.dll" found in dll\glut32.dll in C:\Windows\System32

### Go (src\go)
1. Ensure Go is installed
2. Run the project:
  ```bash
  go run .
  ```
3. Build and run the project
  ```bash
  go build
  .\haskell-vs-go.exe
  ```
4. Usage: 
    - Left click to place or delete a cell
    - Space bar to resume or pause the simulation
5. Run tests (only the gameOfLife.go file is unit tested)
  ```bash
  go test -v
  ```
