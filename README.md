# haskell-vs-go: A Functional Perspective on Conway's Game of Life

This project explores the functional programming aspects of Haskell and Go by implementing Conway's Game of Life in both languages.

## Project Overview
- **Haskell Implementation**:
  - Built with **Cabal** for build and package management and **Gloss** for UI elements

- **Go Implementation**:
  - Only the core logic is implemented without the UI

## How to Build and Run

### Haskell
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
    This happens because GLUT is not correctly configured by Cabal and Gloss
    A easy solution for this is to place the "glut32.dll" found in dll\glut32.dll in C:\Windows\System32

