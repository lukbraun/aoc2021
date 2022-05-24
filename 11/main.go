package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Cell struct {
	value   int
	flashed bool
}

func (c *Cell) New(value int) {
	c.value = value
	c.flashed = false
}

func (c *Cell) Inc() {
	c.value += 1
}

func (c *Cell) Flash() {
	c.flashed = true
}

func (c *Cell) Reset() {
	c.value = 0
	c.flashed = false
}

type EnergyLevelMap struct {
	EnergyLevel [][]Cell
	Level       int
	Flashes     int
	dimX        int
	dimY        int
	debug       bool
}

func (em *EnergyLevelMap) New(debug bool, m [][]int) {
	em.debug = debug
	em.dimY = len(m)
	em.dimX = len(m[0])
	em.Flashes = 0
	em.Level = 0
	em.EnergyLevel = make([][]Cell, em.dimY)
	for x := 0; x < em.dimX; x++ {
		em.EnergyLevel[x] = make([]Cell, em.dimX)
		for y := 0; y < em.dimY; y++ {
			em.EnergyLevel[x][y].New(m[x][y])
		}
	}
}

func (em *EnergyLevelMap) inc(x, y int) {
	if x < 0 || y < 0 {
		return
	}
	if x >= em.dimX || y >= em.dimY {
		return
	}
	em.EnergyLevel[x][y].Inc()
}

func (em *EnergyLevelMap) Inc() {
	for y := 0; y < em.dimY; y++ {
		for x := 0; x < em.dimX; x++ {
			em.inc(x, y)
		}
	}
}

func (em *EnergyLevelMap) flashAround(x, y int) {
	if x < 0 || y < 0 {
		return
	}
	if x >= em.dimX || y >= em.dimY {
		return
	}
	if em.EnergyLevel[x][y].flashed {
		return
	}
	if em.EnergyLevel[x][y].value <= 9 {
		return
	}
	em.EnergyLevel[x][y].Flash()
	// -1,-1
	em.inc(x-1, y-1)
	em.flashAround(x-1, y-1)
	// -1,0
	em.inc(x-1, y)
	em.flashAround(x-1, y)
	// -1,1
	em.inc(x-1, y+1)
	em.flashAround(x-1, y+1)
	// 0,-1
	em.inc(x, y-1)
	em.flashAround(x, y-1)
	// 0, 1
	em.inc(x, y+1)
	em.flashAround(x, y+1)
	// 1,-1
	em.inc(x+1, y-1)
	em.flashAround(x+1, y-1)
	// 1,0
	em.inc(x+1, y)
	em.flashAround(x+1, y)
	// 1,1
	em.inc(x+1, y+1)
	em.flashAround(x+1, y+1)
}

func (em *EnergyLevelMap) Flash() {
	for y := 0; y < em.dimY; y++ {
		for x := 0; x < em.dimX; x++ {
			if em.EnergyLevel[x][y].value > 9 {
				em.flashAround(x, y)
			}
		}
	}
}

func (em *EnergyLevelMap) Finally() {
	for y := 0; y < em.dimY; y++ {
		for x := 0; x < em.dimX; x++ {
			if em.EnergyLevel[x][y].flashed {
				em.EnergyLevel[x][y].Reset()
				em.Flashes += 1
			}
		}
	}
	if em.debug {
		em.Draw()
	}
	em.Level = em.Level + 1
}

func (em *EnergyLevelMap) Draw() {
	fmt.Println("After step ", em.Level, ":")
	for y := 0; y < em.dimY; y++ {
		for x := 0; x < em.dimX; x++ {
			fmt.Print(em.EnergyLevel[x][y].value)
		}
		fmt.Println()
	}
	fmt.Println()
}

func (em *EnergyLevelMap) Run() {
	em.Inc()
	em.Flash()
	em.Finally()
}

func (em *EnergyLevelMap) allFlashed() bool {
	for y := 0; y < em.dimY; y++ {
		for x := 0; x < em.dimX; x++ {
			if em.EnergyLevel[x][y].value != 0 {
				return false
			}
		}
	}
	return true
}

func (em *EnergyLevelMap) FindWhenAllFlash() {
	for !em.allFlashed() {
		em.Run()
	}
}

func readFile(filename string) [][]int {
	i := make([][]int, 0)
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var line = strings.TrimSpace(scanner.Text())
		if line != "" {
			var l []int
			for x := 0; x < len(line); x++ {
				va, _ := strconv.Atoi(string(line[x]))
				l = append(l, va)
			}
			i = append(i, l)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	return i
}

func firstAssignment(filename string) {
	debug := true
	iterations := 100
	content := readFile(filename)
	em := EnergyLevelMap{}
	em.New(debug, content)

	for i := 1; i <= iterations; i++ {
		em.Run()
	}
	fmt.Println("Flashes: ", em.Flashes)
}

func secondAssignment(filename string) {
	debug := true
	content := readFile(filename)
	em := EnergyLevelMap{}
	em.New(debug, content)
	em.FindWhenAllFlash()
	fmt.Println("Finished after step: ", em.Level)
}

func main() {
	filename := "input"
	firstAssignment(filename)
	secondAssignment(filename)
}
