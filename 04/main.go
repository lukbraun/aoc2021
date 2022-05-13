package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Board struct {
	content [][]int
	marked  [][]bool
}

func (b *Board) Append(line string) {
	var s []string = strings.Split(strings.TrimSpace(line), " ")
	var numbers []int
	for _, i := range s {
		j, err := strconv.Atoi(i)
		if err != nil {
			panic(err)
		}
		numbers = append(numbers, j)
	}
	b.content = append(b.content, numbers)
}

func (b *Board) Mark(num_in int) {
	for i, arr := range b.content {
		for j, num := range arr {
			if num == num_in {
				b.marked[i][j] = true
			}
		}
	}
}

func allTrue(b []bool) bool {
	for _, value := range b {
		if value == false {
			return false
		}
	}
	return true
}

func (b Board) Bingo() bool {

	var width, height int = len(b.content[0]), len(b.content)

	// check horizontal
	for _, arr := range b.marked {
		if allTrue(arr) {
			return true
		}
	}
	// check vertical
	for i := 0; i < width; i++ {
		var r bool = true
		for j := 0; j < height; j++ {
			if b.marked[j][i] == false {
				r = false
				break
			}
		}
		if r {
			return true
		}
	}
	var r bool = true
	// check diagonal
	for i := 0; i < width && i < height; i++ {
		if b.marked[i][i] == false {
			r = false
		}
	}
	for i := 0; i < width; i++ {
		for j := height - 1; j >= 0; j-- {
			if b.marked[j][i] == false {
				r = false
			}
		}
	}
	return r
}

func (b Board) copyContent() [][]int {
	duplicate := make([][]int, len(b.content))
	for i := range b.content {
		duplicate[i] = make([]int, len(b.content[i]))
		copy(duplicate[i], b.content[i])
	}
	return duplicate
}

func (b Board) copyMarked() [][]bool {
	duplicate := make([][]bool, len(b.marked))
	for i := range b.marked {
		duplicate[i] = make([]bool, len(b.marked[i]))
		copy(duplicate[i], b.marked[i])
	}
	return duplicate
}

func (b Board) Copy() Board {
	if len(b.marked) == 0 {
		b.marked = make([][]bool, len(b.content))
		for i := 0; i < len(b.content); i++ {
			b.marked[i] = make([]bool, len(b.content[0]))
			for j := 0; j < len(b.content[0]); j++ {
				b.marked[i][j] = false
			}
		}
	}
	return Board{
		content: b.copyContent(),
		marked:  b.copyMarked(),
	}
}

func (b Board) Score(calledNum int) int {
	res := 0
	for i := 0; i < len(b.content); i++ {
		for j := 0; j < len(b.content[0]); j++ {
			if b.marked[i][j] == false {
				res = res + b.content[i][j]
			}
		}
	}
	return res * calledNum
}

type Numbers struct {
	content []int
}

func (n *Numbers) read(s string) {
	var split = strings.Split(s, ",")
	for _, i := range split {
		j, err := strconv.Atoi(i)
		if err != nil {
			panic(err)
		}
		n.content = append(n.content, j)
	}
}

func readLines(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
}

func readInput(file_input string) (Numbers, []Board) {
	var input Numbers
	var boards []Board
	var current_board_content Board
	space := regexp.MustCompile(`\s+`)
	// open file for reading
	// read line by line
	lines, err := readLines(file_input)
	if err != nil {
		log.Fatalf("readLines: %s", err)
	}
	for i, line := range lines {
		if i == 0 {
			input.read(line)
		} else {
			if strings.TrimSpace(line) != "" {
				s := space.ReplaceAllString(line, " ")
				current_board_content.Append(s)
			}
			if len(current_board_content.content) == 5 {
				boards = append(boards, current_board_content.Copy())
				current_board_content = Board{}
			}
		}
		fmt.Println(line)
	}
	return input, boards
}

func findFirst(nums Numbers, boards []Board) (Board, int, int) {
	tries := 0
	for _, num := range nums.content {
		tries = tries + 1
		for _, b := range boards {
			b.Mark(num)
			if b.Bingo() {
				return b, num, tries
			}
		}
	}
	return boards[0], nums.content[len(nums.content)-1], tries
}

type Result struct {
	board  Board
	called int
	tries  int
}

func findLast(nums Numbers, boards []Board) (Board, int) {
	var res []Result
	for _, board := range boards {
		r, called, tries := findFirst(nums, []Board{board})
		res = append(res, Result{board: r.Copy(), called: called, tries: tries})
	}
	var r Result = res[0]
	for i := 0; i < len(res); i++ {
		if r.tries < res[i].tries {
			r = res[i]
		}
	}
	return r.board, r.called
}

func firstAssignment(file_input string) {
	numbers, boards := readInput(file_input)
	res, called, _ := findFirst(numbers, boards)
	fmt.Println("Score: %s", res.Score(called))
}

func secondAssignment(file_input string) {
	numbers, boards := readInput(file_input)
	res, called := findLast(numbers, boards)
	fmt.Println("Score: ", res.Score(called))
}

func main() {
	var fileName string = "input"
	firstAssignment(fileName)
	secondAssignment(fileName)
}
