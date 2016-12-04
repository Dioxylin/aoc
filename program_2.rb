test = false

##
# This is a keypad.
#
# Abstracts the keypad given from:
#
#     1 2 3
#     4 5 6
#     7 8 9
#
# Really, this is coupled highly with the Finger class.
class Keypad
	##
	# Creates a new keypad with initial key position 5.
	def initialize
		@key = 5
	end
	def up
		@key -= 3
		if @key < 1 then
			@key += 3
		end
	end
	def down
		@key += 3
		if @key > 9 then
			@key -= 3
		end
	end
	def left
		case @key
		when 1,4,7
		else
			@key -= 1
		end
	end
	def right
		case @key
		when 3,6,9
		else
			@key += 1
		end
	end
	attr_reader :key
end

##
# This is the over-engineered keypad.
#
# This abstracts the following keypad:
#
#        1
#      2 3 4
#    5 6 7 8 9
#      A B C
#        D
#
# This is highly coupled with Finger to produce our results.
class ShittyKeypad
	def initialize
		@key = 5
	end
	def up
		case @key
		when 3,0xD
			@key -= 2
		when 6,7,8,0xA,0xB,0xC
			@key -= 4
		else
			nil
		end
	end
	def down
		case @key
		when 1,0xB
			@key += 2
		when 2,3,4,6,7,8
			@key += 4
		else
			nil
		end
	end
	def left
		case @key
		when 3,4,6,7,8,9,0xB,0xC
			@key -= 1
		else
			nil
		end
	end
	def right
		case @key
		when 2,3,5,6,7,8,0xA,0xB
			@key += 1
		else
			nil
		end
	end
	attr_reader :key
end

##
# Abstracts a finger that points to the keypad to work out the solution.
#
# Highly coupled with Keypad and ShittyKeypad
class Finger
	def initialize(keypad)
		@keypad = keypad
		@keyToPress = []
	end
	def parse_lines(str)
		str.each_char do |c|
			parse_character(c)
		end
	end
	def parse_character(c)
		if c == "U" then
			@keypad.up
		elsif c == "D" then
			@keypad.down
		elsif c == "L" then
			@keypad.left
		elsif c == "R" then
			@keypad.right
		elsif c == "\n" then
			@keyToPress.push(@keypad.key.to_s(16).upcase)
			#puts @keypad.key.to_s(16).upcase
		else
			raise "Invalid character: `#{c}`."
		end
	end
	attr_reader :keyToPress
end

test = true
if test then
	test1 = "UUL\nRRDDD\nLURDL\nUUUUD\n"

	keypad = Keypad.new
	finger = Finger.new(keypad)
	finger.parse_lines(test1)
	print finger.keyToPress
	puts

	shittyKeypad = ShittyKeypad.new
	fingerButt = Finger.new(shittyKeypad)
	fingerButt.parse_lines(test1)
	print fingerButt.keyToPress
	puts
end

f = File.open("input_2.txt")

keypad = Keypad.new
finger = Finger.new(keypad)

shittyKeypad = ShittyKeypad.new
fingerButt = Finger.new(shittyKeypad)

string = f.read
finger.parse_lines(string)
print finger.keyToPress
puts

fingerButt.parse_lines(string)
print fingerButt.keyToPress
puts
