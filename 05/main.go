package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

// -- point

type Point struct {
	x int
	y int
}

func NewPoint(str string) *Point {
	tmp := strings.Split(str, ",")
	x, _ := strconv.Atoi(tmp[0])
	y, _ := strconv.Atoi(tmp[1])
	return &Point{x: x, y: y}
}

// -- Board

type Board struct {
	points []Point
}

func (b *Board) Mark(point Point) {
	b.points = append(b.points, point)
}

func (b *Board) MarkPoints(points []Point) {
	for _, p := range points {
		b.Mark(p)
	}
}

func (b *Board) CountOverlappingPointsGreater(greaterOrEquals int) int {
	res := 0
	pointMap := make(map[Point]int)
	for _, point := range b.points {
		if _, ok := pointMap[point]; ok {
			pointMap[point] = pointMap[point] + 1
		} else {
			pointMap[point] = 1
		}
	}
	for _, element := range pointMap {
		if element >= greaterOrEquals {
			res = res + 1
		}
	}
	return res
}

func (b *Board) Draw() {
	pointMap := make(map[Point]int)
	for _, point := range b.points {
		if _, ok := pointMap[point]; ok {
			pointMap[point] = pointMap[point] + 1
		} else {
			pointMap[point] = 1
		}
	}
	var dimX, dimY int
	for _, p := range b.points {
		if p.x > dimX {
			dimX = p.x
		}
		if p.y > dimY {
			dimY = p.y
		}
	}
	for y := 0; y <= dimY; y++ {
		for x := 0; x <= dimX; x++ {
			p := Point{x: y, y: x}

			if value, ok := pointMap[p]; ok {
				fmt.Print(value)
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

// -- functions

func minFirst(in1 int, in2 int) (int, int) {
	if in1 < in2 {
		return in1, in2
	}
	return in2, in1
}

func maxFirst(in1, in2 int) (int, int) {
	if in1 > in2 {
		return in1, in2
	}
	return in2, in1
}

func is45Degrees(p1, p2 Point) bool {
	return math.Abs(float64(p1.x)-float64(p2.x)) == math.Abs(float64(p1.y)-float64(p2.y))
}

func isVertical(p1, p2 Point) bool {
	return p1.x == p2.x
}

func isHorizontal(p1, p2 Point) bool {
	return p1.y == p2.y
}

func createPoints(p1 Point, p2 Point) []Point {
	var res []Point

	if !is45Degrees(p1, p2) {
		return createPointsHV(p1, p2)
	}

	incX, incY := 1, 1
	if p1.x > p2.x {
		incX = -1
	}
	if p1.y > p2.y {
		incY = -1
	}
	for {
		res = append(res, Point{x: p1.y, y: p1.x})
		if p1 == p2 {
			break
		}
		p1.x = p1.x + incX
		p1.y = p1.y + incY
	}
	return res
}

func createPointsHV(p1, p2 Point) []Point {
	var res []Point
	var start, end int
	var horizontal, vertical = isHorizontal(p1, p2), isVertical(p1, p2)
	if !vertical && !horizontal {
		return res
	}
	var fixed int
	if horizontal {
		start, end = minFirst(p1.x, p2.x)
		fixed = p1.y
	} else if vertical {
		start, end = minFirst(p1.y, p2.y)
		fixed = p1.x
	}
	for {
		if start > end {
			break
		}
		if horizontal {
			res = append(res, Point{x: fixed, y: start})
		} else {
			res = append(res, Point{x: start, y: fixed})
		}
		start = start + 1
	}
	return res
}

func readFile(filename string) ([]Point, []Point) {
	var points1, points2 []Point
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var line = strings.TrimSpace(scanner.Text())
		if line != "" {
			splitString := strings.Split(line, " -> ")
			points1 = append(points1, *NewPoint(splitString[0]))
			points2 = append(points2, *NewPoint(splitString[1]))
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	return points1, points2
}

// -- assignments
func firstAssignment(filename string, debug bool) {
	points1, points2 := readFile(filename)
	board := Board{}
	for i := 0; i < len(points1) && i < len(points2); i++ {
		p1, p2 := points1[i], points2[i]
		points := createPointsHV(p1, p2)
		board.MarkPoints(points)
	}
	if debug {
		board.Draw()
	}
	res := board.CountOverlappingPointsGreater(2)
	fmt.Println("Intersections: ", res)
}

func secondAssignment(filename string, debug bool) {
	points1, points2 := readFile(filename)
	board := Board{}
	for i := 0; i < len(points1) && i < len(points2); i++ {
		p1, p2 := points1[i], points2[i]
		points := createPoints(p1, p2)
		board.MarkPoints(points)
	}
	if debug {
		board.Draw()
	}
	res := board.CountOverlappingPointsGreater(2)
	fmt.Println("Intersections: ", res)
}

func main() {
	filename, debug := "input", false
	fmt.Println("-- Assignment1: ")
	firstAssignment(filename, debug)
	fmt.Println("-- Assignment2: ")
	secondAssignment(filename, debug)
}
