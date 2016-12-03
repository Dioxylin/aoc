test = false

class Keypad
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
		end
	end
	def down
		case @key
		when 1,0xB
			@key += 2
		when 2,3,4,6,7,8
			@key += 4
		end
	end
	def left
		case @key
		when 3,4,6,7,8,9,0xB,0xC
			@key -= 1
		end
	end
	def right
		case @key
		when 2,3,5,6,7,8,0xA,0xB
			@key += 1
		end
	end
	attr_reader :key
end

class Finger
	def initialize(keypad)
		@keypad = keypad
		@keyToPress = []
	end
	def parseLines(str)
		str.each_char do |c|
			parseCharacter(c)
		end
	end
	def parseCharacter(c)
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
	finger.parseLines(test1)
	print finger.keyToPress
	puts

	shittyKeypad = ShittyKeypad.new
	fingerButt = Finger.new(shittyKeypad)
	fingerButt.parseLines(test1)
	print fingerButt.keyToPress
	puts
end

f = File.open("input_2.txt")

keypad = Keypad.new
finger = Finger.new(keypad)

shittyKeypad = ShittyKeypad.new
fingerButt = Finger.new(shittyKeypad)

string = f.read
finger.parseLines(string)
print finger.keyToPress
puts

fingerButt.parseLines(string)
print fingerButt.keyToPress
puts
