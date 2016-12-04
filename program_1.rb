inputFile = "input_1.txt"

class Coordinates
	def initialize(north,east)
		@blocksNorth = north
		@blocksEast = east
	end
	def distance
		@blocksNorth.abs + @blocksEast.abs
	end
	def to_s
		"#{@blocksNorth},#{@blocksEast}"
	end
	def ==(other)
		self.blocksNorth == other.blocksNorth and
			self.blocksEast == other.blocksEast
	end
	attr_reader :blocksNorth, :blocksEast
	attr_writer :blocksNorth, :blocksEast
end

class Travel
	def initialize
		@direction = "north"
		@currentCoordinates = Coordinates.new(0, 0)
		@visited = Array.new
		# We need to copy the object or else we change it with += and -=.
		@visited.push(@currentCoordinates.dup)
		# Print as we visit everywhere
		@loudlyVisit = false
		@bunnyHQ = nil
	end
	def check_location(coord)
		if @visited.include?(coord) then
			if @loudlyVisit then
				puts "*** FOUND: #{coord}; distance: #{coord.distance}"
			end
			if @bunnyHQ.nil? then
				@bunnyHQ = coord.dup
			end
		end
	end
	def left=(value)
		north = @currentCoordinates.blocksNorth
		east = @currentCoordinates.blocksEast
		if @direction == "north" then
			@direction = "west"
			north = north..north
			east = (east-value)..(east-1)
			@currentCoordinates.blocksEast -= value
		elsif @direction == "south" then
			@direction = "east"
			north = north..north
			east = (east+1)..(east+value)
			@currentCoordinates.blocksEast += value
		elsif @direction == "east" then
			@direction = "north"
			north = (north+1)..(north+value)
			east = east..east
			@currentCoordinates.blocksNorth += value
		elsif @direction == "west" then
			@direction = "south"
			north = (north-value)..(north-1)
			east = east..east
			@currentCoordinates.blocksNorth -= value
		end
		# We don't want to record turns in place.
		if value == 0 then
			return 0
		end
		# either north or east will stay constant and allow the other to loop.
		north.each do |n|
			east.each do |e|
				coord = Coordinates.new(n,e)
				check_location(coord)
				@visited.push(coord.dup)
				if @loudlyVisit then
					puts "Visited #{coord}"
				end
			end
		end
	end
	# 3 lefts make a right.
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
	end
	def distance
		@currentCoordinates.blocksNorth.abs + @currentCoordinates.blocksEast.abs
	end
	attr_reader :previousCoordinates, :currentCoordinates, :visited, :loudlyVisit, :bunnyHQ
	def to_s
		"#{@currentCoordinates.blocksNorth},#{@currentCoordinates.blocksEast}; facting #{@direction}; distance from origin: #{distance}"
	end
end

taxi = Travel.new

f = File.open(inputFile)
s = f.readline
f.close

# No newlines in string please
s = s[/[^\n]*/]
#puts s

directions = s.split(', ')
directions.each do |d|
	taxi.parse(d)
end

puts "Easter bunny's HQ is #{taxi.bunnyHQ.distance} blocks away at N,E #{taxi.bunnyHQ}."
