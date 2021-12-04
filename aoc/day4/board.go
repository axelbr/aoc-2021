package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Board struct {
	values [][]int
	marks  [][]bool
}

func FromString(boardString string) (Board, error) {
	lines := strings.Split(boardString, "\n")
	board := make([][]int, len(lines))
	marks := make([][]bool, len(lines))
	for i, line := range lines {
		fields := strings.Fields(line)
		row := make([]int, len(fields))
		marksRow := make([]bool, len(fields))
		board[i] = row
		marks[i] = marksRow
		for j, field := range strings.Fields(line) {
			value, err := strconv.ParseInt(field, 10, 64)
			if err != nil {
				return Board{}, err
			}
			row[j] = int(value)
		}
	}
	return Board{board, marks}, nil
}

func (receiver *Board) isCompleted() bool {
	gridSize := len(receiver.marks)
	marks := make([]bool, 2*gridSize+2)
	for i := range marks[:gridSize*2] {
		marks[i] = true
	}
	for i := 0; i < gridSize; i++ {
		for j := 0; j < gridSize; j++ {
			value := receiver.marks[i][j]
			marks[i] = marks[i] && value
			marks[j+gridSize] = marks[j+gridSize] && value
			if i == j && false {
				marks[2*gridSize] = marks[2*gridSize] && value
			}
			if i == gridSize-j-1 && false {
				marks[2*gridSize+1] = marks[2*gridSize+1] && value
			}
		}
	}
	for _, val := range marks {
		if val == true {
			return true
		}
	}
	return false
}

func (receiver *Board) setMark(i int, j int, mark bool) {
	receiver.marks[i][j] = mark
}

func (receiver *Board) markNumber(number int) {
	for i := 0; i < len(receiver.values); i++ {
		for j := 0; j < len(receiver.values[i]); j++ {
			if number == receiver.values[i][j] {
				receiver.setMark(i, j, true)
			}
		}
	}
}

func (receiver Board) score() int {
	score := 0
	for i := 0; i < len(receiver.values); i++ {
		for j := 0; j < len(receiver.values[i]); j++ {
			if receiver.marks[i][j] == false {
				score += receiver.values[i][j]
			}
		}
	}
	return score
}

func (receiver *Board) String() string {
	return fmt.Sprint(receiver.values)
}
