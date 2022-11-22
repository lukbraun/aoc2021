# FSM
TRANSITIONS = {
  read_polymer_template: {
    default: :read_polymer_template,
    empty_line: :read_pair_insertion
  },
  read_pair_insertion: {
    default: :read_pair_insertion,
    empty_line: :done
  }
}

# Parsing
def parse_file(filename)
  state = :read_polymer_template
  polymer_template = ''
  rules = {}
  File.readlines(filename).each do |line|
    if line.length == 1
      state = TRANSITIONS[state][:empty_line]
    elsif state == :read_polymer_template
      line = line.delete("\n")
      polymer_template += line
    elsif state == :read_pair_insertion
      line = line.delete("\n")
      line = line.split(' -> ')
      rules[line[0]] = line[1]
    end
  end
  [polymer_template, rules]
end

# Logic
def split_polymer(polymer_template)
  temp1 = polymer_template.chars
  temp1.pop
  temp2 = polymer_template.chars
  temp2 = temp2.drop(1)
  combined = temp1.zip(temp2)
  res = []
  combined.each do |pair|
    res.append(pair[0] + pair[1])
  end
  res
end

def create_occurence_map(str)
  str.chars.each_cons(2).to_a.map { |a, b| a + b }.tally
end

def insert_occurence(m, polymer, value)
  m[polymer] ||= 0
  m[polymer] += value
  m
end

def update_occurence_map(occurence_map, rules)
  res = {}
  occurence_map.each do |key, value|
    to_insert = rules[key]
    new_polymer1 = key[0] + to_insert
    new_polymer2 = to_insert + key[1]
    res = insert_occurence(res, new_polymer1, value)
    res = insert_occurence(res, new_polymer2, value)
  end
  res
end

def split_map_by_characters(occurence_map)
  res = {}
  occurence_map.each do |key, value|
    value_to_insert = value / 2
    res = insert_occurence(res, key[0], value_to_insert)
    res = insert_occurence(res, key[1], value_to_insert)
  end
  res
end

def calculate_result(occurence_map, last_char)
  res = {last_char => 1}
  occurence_map.each do |k, count|
    char = k[0]
    res[char] ||= 0
    res[char] += count
  end
  res.values.max - res.values.min
end

# # naive approach
# def process_simple_insert(polymer_template, rules)
#   polymer = split_polymer(polymer_template)
#   res = []
#   polymer.each do |e|
#     res.append(e[0] + rules[e])
#   end
#   res.join + polymer[-1][-1]
# end

# Main Logic

def first_assignment(filename)
  temp, rules = parse_file(filename)
  occurence_map = create_occurence_map(temp)
  10.times do |_|
    occurence_map = update_occurence_map(occurence_map, rules)
  end
  puts('First Assignment:')
  puts("Result: #{calculate_result(occurence_map, temp[-1])}")
end

def second_assignment(filename)
  temp, rules = parse_file(filename)
  occurence_map = create_occurence_map(temp)
  40.times do |_|
    occurence_map = update_occurence_map(occurence_map, rules)
  end
  puts('Second Assignment:')
  puts("Result: #{calculate_result(occurence_map, temp[-1])}")
end

first_assignment('input')
puts('===========')
second_assignment('input')
