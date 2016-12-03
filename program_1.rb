inputFile = "input_1.txt"

class Travel
	def initialize
		@direction = "north"
		@blocksNorth = 0
		@blocksEast = 0
	end
	def left=(value)
		if @direction == "north" then
			@direction = "west"
			@blocksEast -= value
		elsif @direction == "south" then
			@direction = "east"
			@blocksEast += value
		elsif @direction == "east" then
			@direction = "north"
			@blocksNorth += value
		elsif @direction == "west" then
			@direction = "south"
			@blocksNorth -= value
		end
		#puts to_s
	end
	def right=(value)
		self.left = 0
		self.left = 0
		self.left = value
	end
	def parse(s)
		dir = s[0]
		s[0] = ''
		i = Integer(s)
		if dir == "R" then
			self.right = i
		elsif dir == "L" then
			self.left = i
		else
			raise "Error parsing: #{s}"
		end
		#puts to_s
	end
	def distance
		@blocksNorth.abs + @blocksEast.abs
	end
	attr_reader :blocksNorth, :blocksEast, :direction
	def to_s
		"#{@blocksNorth},#{@blocksEast}; facting #{@direction}; distance from origin: #{distance}"
	end
end


test1 = "R2, L3".split(', ')
taxi1 = Travel.new
test2 = "R2, R2, R2".split(', ')
taxi2 = Travel.new
test3 = "R5, L5, R5, R3".split(', ')
taxi3 = Travel.new

test1.each do |d|
	taxi1.parse(d)
end
test2.each do |d|
	taxi2.parse(d)
end
test3.each do |d|
	taxi3.parse(d)
end

puts taxi1.to_s
puts taxi2.to_s
puts taxi3.to_s



taxi = Travel.new

f = File.open(inputFile)
s = f.readline
f.close

# No newlines in string please
s = s[/[^\n]*/]
puts s

directions = s.split(', ')
#puts directions
directions.each do |d|
	taxi.parse(d)
end

puts taxi.to_s
puts taxi.distance
