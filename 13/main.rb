# Parsing

class Dot
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    "Dot {x: #{@x}, y: #{@y}}"
  end

  def ==(other)
    @x == other.x && @y == other.y
  end
end

class Instruction
  def initialize(axis, number)
    @axis = axis
    @number = number
  end

  def to_s
    "Instruction {axis: #{@axis}, number: #{@number}}"
  end

  def apply(dots)
    res = []
    if @axis == 'x'
      dots.each do |dot|
        if dot.x > @number
          distance = (dot.x - @number).abs
          dot = Dot.new(dot.x - (distance * 2), dot.y)
        end
        res << dot unless res.include?(dot)
      end
    else
      dots.each do |dot|
        if dot.y > @number
          distance = (dot.y - @number).abs
          dot = Dot.new(dot.x, dot.y - (distance * 2))
        end
        res << dot unless res.include?(dot)
      end
    end
    res
  end
end

def parse_dot(str)
  str = str.split(',')
  Dot.new(str[0].to_i, str[1].to_i)
end

def parse_instruction(str)
  str = str.split.last
  str = str.split('=')
  Instruction.new(str[0], str[1].to_i)
end

# Logic

def find_max(dots)
  max_x = -1
  max_y = -1
  dots.each do |e|
    max_x = e.x > max_x ? e.x : max_x
    max_y = e.y > max_y ? e.y : max_y
  end
  [max_x, max_y]
end

def print_board(dots, max_x, max_y)
  (0..max_y).each do |y|
    (0..max_x).each do |x|
      if dots.include?(Dot.new(x, y))
        print('#')
      else
        print('.')
      end
    end
    puts('')
  end
end

# States and Transitions

def read_from_file(name, transitions)
  state = :reading_dots
  dots = []
  File.readlines(name).each do |line|
    if line.length == 1
      state = transitions[state][:empty_line]
    elsif state == :reading_dots
      dots << parse_dot(line)
      state = transitions[state][:dot]
    elsif state == :reading_instructions
      instruction = parse_instruction(line)
      dots = instruction.apply(dots)
      state = transitions[state][:instruction]
    end
  end
  dots
end

def first_assignment
  transitions = {
    reading_dots: {
      dot: :reading_dots,
      empty_line: :reading_instructions
    },
    reading_instructions: {
      instruction: :done,
      empty_line: :done
    }
  }
  puts("===")
  puts("First assignment: ")
  dots = read_from_file('input', transitions)
  puts("Contains #{dots.length} dots!")
end

def second_assignment
  transitions = {
    reading_dots: {
      dot: :reading_dots,
      empty_line: :reading_instructions
    },
    reading_instructions: {
      instruction: :reading_instructions,
      empty_line: :done
    }
  }
  puts("===")
  puts("Second assignment: ")
  dots = read_from_file('input', transitions)
  max_x, max_y = find_max(dots)
  (0..max_y).each { |y| 
    (0..max_x).each { |x| 
      char = dots.include?(Dot.new(x, y)) ? '#' : '.'
      print(char)
    }
    puts("")
  }
end

first_assignment
second_assignment