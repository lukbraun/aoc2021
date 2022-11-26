INT_MAX = (2**(0.size * 8 - 2) - 1)
INT_MIN = -(2**(0.size * 8 - 2))

def read_packet_version(packet)
  [packet.shift(3).join.to_i(2), packet]
end

def read_type_version(packet)
  [packet.shift(3).join.to_i(2), packet]
end

def read_standard_header(packet)
  version, packet = read_packet_version(packet)
  type_version, packet = read_type_version(packet)
  [[version, type_version], packet]
end

def read_literal_packet(packet)
  res = []
  res << packet.shift(4) while packet.shift == '1'
  res << packet.shift(4)
  [res.join.to_i(2), packet]
end

# operator

def read_operator_packet_0(packet)
  packet_length = packet.shift(15).join.to_i(2)
  sub_packet = packet.shift(packet_length)
  res = []
  until sub_packet.empty?
    sub_result, sub_packet = parse_packet(sub_packet)
    res << sub_result
  end
  res
end

def read_operator_packet_1(packet)
  packet_count = packet.shift(11).join.to_i(2)
  res = []
  packet_count.times do
    sub_result, packet = parse_packet(packet)
    res << sub_result
  end
  res
end

def read_operator_packet(packet)
  if packet.shift == '0'
    read_operator_packet_0(packet)
  else
    read_operator_packet_1(packet)
  end
end

# packet

def parse_packet(packet)
  header, packet = read_standard_header(packet)
  version, type = header
  if type == 4
    lit, = read_literal_packet(packet)
    [{ version: version, type: type, literal: lit }, packet]
  else
    [{ version: version, type: type, operator: read_operator_packet(packet) }, packet]
  end
end

# read from input

def read_input(filename)
  File.open(filename).read.strip.chars.map { |c| c.hex.to_s(2).rjust(4, '0').chars }.flatten
end

# Assignments

def first_assignment(parsed_input)
  # calc result of first assignment
  def calc(inp)
    acc = inp[:version]
    inp[:operator].each { |o| acc += calc(o) } if inp.key?(:operator)
    acc
  end
  parsed, = parsed_input
  puts 'First Assignment: '
  puts("#{calc(parsed)}")
end

def second_assignment(parsed_input)
  def calc(inp)
    res = 0
    if inp.key?(:literal)
      res = inp[:literal]
    elsif inp.key?(:operator)
      type = inp[:type]
      tmp = 0
      case type
      when 0
        inp[:operator].each { |e| tmp += calc(e) }
      when 1
        r1 = 1
        inp[:operator].each { |e| r1 *= calc(e) }
        tmp += r1
      when 2
        tmp = INT_MAX
        inp[:operator].each { |e| tmp = [calc(e), tmp].min }
      when 3
        tmp = INT_MIN
        inp[:operator].each { |e| tmp = [calc(e), tmp].max }
      when 5
        r1 = calc(inp[:operator][0])
        r2 = calc(inp[:operator][1])
        tmp = r1 > r2 ? 1 : 0
      when 6
        r1 = calc(inp[:operator][0])
        r2 = calc(inp[:operator][1])
        tmp = r1 < r2 ? 1 : 0
      when 7
        r1 = calc(inp[:operator][0])
        r2 = calc(inp[:operator][1])
        tmp = r1 == r2 ? 1 : 0
      end
      res = tmp
    end
    res
  end
  parsed, = parsed_input
  puts 'Second Assignment:'
  puts "#{calc(parsed)}"
end

input = parse_packet(read_input('input'))
# puts("#{input}")
first_assignment(input)
puts '================'
second_assignment(input)
