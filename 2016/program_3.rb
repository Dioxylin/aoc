input = "./input_3.txt"

class InvalidTriangleError < ArgumentError
end

class Triangle
	private

	def initialize(*args)
		if args.length != 1 && args.length != 3 then
			raise ArgumentError.new("Invalid arguments.  Provide an array or side lengths individually.");
		end
		if args.length == 3 then
			initialize_with_array(args)
			return
		end
		if not args[0].is_a?(Array) then
			raise ArgumentError.new("Invalid arguments.  Provide an array.")
		end
		initialize_with_array(args[0])
	end
	def initialize_with_array(sides)
		if sides.length != 3 then
			raise ArgumentError.new("Invalid number of arguments.  #{sides}")
		end
		sides = sides.sort
		@side1 = sides[0]
		@side2 = sides[1]
		@hypotenuse = sides[2]
		if @side1 <= 0 then
			raise InvalidTriangleError.new("Invalid arguments.  Must be positive: #{sides}")
		end
		if @side1 + @side2 <= @hypotenuse then
			raise InvalidTriangleError.new("Invalid arguments.  #@side1 + #@side2 <= #@hypotenuse.")
		end
	end

	public
	
	def to_s
		"<Triangle: #@side1,#@side2,#@hypotenuse.>"
	end
end

def test_triangle_exceptional
	begin
		yield
		raise "***Not an invalid triangle!"
	rescue InvalidTriangleError
	end
end

def test_triangle_not_exceptional
	begin
		yield
	rescue InvalidTriangleError => ite
		raise "***Invalid triangle!"
	end

end

def test
	bad_test1 = [10,15,25]
	bad_test2 = [0,1,3]
	bad_test3 = [200, 300, 500]
	test_triangle_exceptional { t = Triangle.new(bad_test1) }
	test_triangle_exceptional { t = Triangle.new(bad_test2) }
	test_triangle_exceptional { t = Triangle.new(bad_test3) }

	good_test1 = [10,15,24]
	good_test2 = [1,3,3]
	good_test3 = [200, 300, 499]
	test_triangle_not_exceptional { t = Triangle.new(good_test1) }
	test_triangle_not_exceptional { t = Triangle.new(good_test2) }
	test_triangle_not_exceptional { t = Triangle.new(good_test3) }
end

# Run the tests.  If the test is broken, don't run the actual solution.
test

f = File.open(input)
lines = f.readlines
sides_list = []

lines.each do |line|
	line[-1] = ''
	numbers = line.split(' ')
	sides = numbers.collect do |n|
		Integer(n)
	end
	sides_list.push(sides)
end

triangles = []

sides_list.collect do |sides|
	begin
		triangle = Triangle.new(sides)
		triangles.push(triangle)
	rescue InvalidTriangleError
	end
end

print "Valid row-based triangles: #{triangles.length}\n"

triangles = []

sides_list.transpose.flatten.each_slice(3).collect do |sides|
	begin
		triangle = Triangle.new(sides)
		triangles.push(triangle)
	rescue InvalidTriangleError
	end
end

print "Valid column-based triangles: #{triangles.length}\n"
