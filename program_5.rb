require 'digest/md5'

input = 'input_5.txt'

class DoorPassword
	POSITIONAL=true
	NOT_POSITIONAL=false
	UPDATE=true
	NO_UPDATE=false
	def initialize(door_id, positional=NOT_POSITIONAL, puts_updates=NO_UPDATE)
		@door_id = door_id
		@index = 0
		@full = Array.new(8)
		@update = puts_updates
		@positional = positional
		generate_password
	end
	def to_s
		@full
	end
	private
	INFINITY = 1.0/0.0
	def print_progress(n)
		if @update && n.modulo(100000) == 0 then
			puts "#{n}: #@full"
		end
	end
	def generate_password
		(0..INFINITY).each do |n|
			md5 = Digest::MD5.hexdigest(@door_id+n.to_s)
			print_progress n
			if md5.start_with?("00000") then
				if @positional then
					position = md5[5]
					if position =~ /[0-7]/ then
						position = Integer(position)
						if not full[position].nil? then
							next
						end
						full[position] = md5[6]
						if full.all? then
							break
						end
					else
						next
					end
				else
					@full[@index] = md5[5]
					@index += 1
					if @index == 8 then
						@final_try = n
						break
					end
				end
			end
		end
		if @update then
			puts "#@final_try: #@full"
		end
		@full = @full.join
		nil
	end
	public
	attr_reader :full
end

def test
	test_message = ""
	test_door_id = "abc"

	test_password = DoorPassword.new(test_door_id, false, true)

	if test_password.full != "18f47a30" then
		test_message += "Password is not 18f47a30.  Password generated: #{test_password.full}\n"
	end

	test_password = DoorPassword.new(test_door_id, true, true)

	if test_password.full != "05ace8e3" then
		test_message += "Password is not 05ace8e3.  Password generated: #{test_password.full}\n"
	end

	if test_message != "" then
		puts test_message
		raise RuntimeError.new("Test failed!")
	end
end

test

f = File.open(input)
door_id = f.readline.chomp
f.close

puts DoorPassword.new(door_id, DoorPassword::NOT_POSITIONAL, DoorPassword::UPDATE)
puts DoorPassword.new(door_id, DoorPassword::POSITIONAL, DoorPassword::UPDATE)
