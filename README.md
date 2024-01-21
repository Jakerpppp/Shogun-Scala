# Shogun Game in Scala

Welcome to the Shogun Game implemented in Scala!
- Made in December 2023

## Overview

This Scala project brings to life the classic game of Shogun, where strategic moves and careful planning are key. The implementation includes determining all possible moves for each piece and ensuring the legality of those moves, with special consideration given to the king to prevent moves that put it in check or lead to capture.

## Features

1. **Piece Movement:**
   - All pieces, including the king, have their possible moves calculated based on the rules of Shogun.

2. **Legal Move Validation:**
   - Each possible move is validated to ensure it adheres to the rules of the game.
   - Special attention is given to the king to prevent moves that result in check or capture.

## Game Rules

- The game follows the standard rules of Shogun.
- Pieces include the king, pawns, knights, rooks, bishops, and more.
- Moves are determined based on the type of piece and the board state.
- Illegitimate moves, especially those that put the king in check, are restricted.
