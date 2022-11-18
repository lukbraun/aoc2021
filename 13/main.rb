class Dot
  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    "Dot {x: #{@x}, y: #{@y}}"
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

def ignore(str); end

TRANSITIONS = {
  reading_dots: {
    dot: :reading_dots,
    empty_line: :reading_instructions
  },
  reading_instructions: {
    instruction: :reading_instructions,
    empty_line: :done
  }
}

def read_from_file(name)
  state = :reading_dots
  dots = []
  instructions = []
  File.readlines(name).each do |line|
    if line.length == 1
      state = TRANSITIONS[state][:empty_line]
    elsif state == :reading_dots
      dots << parse_dot(line)
    elsif state == :reading_instructions
      instructions << parse_instruction(line)
    end
  end
  dots.each { |e| puts(e) }
  instructions.each { |e| puts(e) }
end

read_from_file('input_test')
