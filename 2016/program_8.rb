Input = "input_8.txt"

def print_screen(screen)
	screen.each do |row|
		row.each do |cell|
			print cell
		end
		puts
	end
	puts
end

def rect(screen, row, column)
	(0...row).each do |r|
		(0...column).each do |c|
			screen[r][c] = "#"
		end
	end
end

def rotate_row(screen, row_num, magnitude)
	row = screen[row_num]
	length = row.length
	temp_row = Array.new(length, ".")
	row.each_with_index do |cell, i|
		if cell == "#" then
			new_position = (i + magnitude).modulo(length)
			temp_row[new_position] = "#"
		end
	end
	screen[row_num] = temp_row
end

def rotate_col(screen, column_num, magnitude)
	height = screen.length
	col = Array.new(height, ".")
	(0...height).each do |i|
		col[i] = screen[i][column_num]
	end
	temp_col = Array.new(height, ".")
	col.each_with_index do |cell, i|
		if cell == "#" then
			new_position = (i + magnitude).modulo(height)
			temp_col[new_position] = "#"
		end
	end
	temp_col.each_with_index do |cell, i|
		screen[i][column_num] = temp_col[i]
	end
end

def parse_instructions(screen, lines)
	lines.each do |line|
		if line.start_with?("rect ") then
			parts = line.split(" ")
			dimensions = parts[1].split("x")
			column = Integer(dimensions[0])
			row = Integer(dimensions[1])
			rect(screen, row, column)
		elsif line.start_with?("rotate row y=") then
			parts = line.split(" ")
			row_num = Integer(parts[2].split("=")[1])
			magnitude = Integer(parts[4])
			rotate_row(screen, row_num, magnitude)
		elsif line.start_with?("rotate column x=") then
			parts = line.split(" ")
			column_num = Integer(parts[2].split("=")[1])
			magnitude = Integer(parts[4])
			rotate_col(screen, column_num, magnitude)
		end
	end
end

def count_lit_pixels(screen)
	count = 0
	screen.each do |row|
		row.each do |cell|
			if cell == "#" then
				count += 1
			end
		end
	end
	count
end

def main
	f = File.open(Input)
	lines = f.readlines
	f.close

	# screen is row by column
	screen = Array.new(6) { Array.new(50,".") }
	parse_instructions(screen, lines)
	print_screen(screen)
	lit_pixel_count = count_lit_pixels(screen)
	puts "Number of lit pixels: #{lit_pixel_count}."
end

main
