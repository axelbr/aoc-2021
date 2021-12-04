package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Result struct {
	id        int
	score     int
	completed bool
}

func play(boardNo int, board Board, guessChannel chan int, resultChannel chan Result) {
	guess := <-guessChannel
	board.markNumber(guess)
	result := Result{boardNo, board.score(), board.isCompleted()}
	resultChannel <- result
}

func parseGuesses(line string) []int {
	fields := strings.Split(line, ",")
	guesses := make([]int, len(fields))
	for i, field := range fields {
		guess, _ := strconv.ParseInt(field, 10, 64)
		guesses[i] = int(guess)
	}
	return guesses
}

func loadGame(path string) ([]int, []Board) {
	file, _ := os.ReadFile(path)
	game := string(file)
	guesses := parseGuesses(strings.Split(game, "\n\n")[0])
	boardStrings := strings.Split(game, "\n\n")[1:]
	boards := make([]Board, len(boardStrings))
	for i, boardString := range boardStrings {
		boards[i], _ = FromString(boardString)
	}
	return guesses, boards
}

func main() {
	guesses, boards := loadGame("input.txt")
	guessChan := make(chan int, len(boards))
	resultChan := make(chan Result, len(boards))
	for _, guess := range guesses {
		for i, board := range boards {
			go play(i, board, guessChan, resultChan)
			guessChan <- guess
		}

		for i := 0; i < len(boards); i++ {
			result := <-resultChan
			if result.completed {
				fmt.Printf("Board %d won! Score: %d", result.id, result.score*guess)
				os.Exit(0)
			}
		}
	}
}
