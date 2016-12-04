input = "input_4.txt"

class BadRoomError < RuntimeError
end

class EncryptedName
	def initialize(str)
		@full = str
		@name = parse_name(str)
		@sector_id = parse_sector_id(str)
		@checksum = parse_checksum(str)
		check_room_real
		@decrypted_name = decrypt_name
	end
	def parse_name(str)
		retstr = ""
		str.each_char do |c|
			if c =~ /[0-9]/ then
				# Kill last hyphen
				retstr[-1] = ""
				break
			else
				retstr += c
			end
		end
		retstr
	end
	def parse_sector_id(str)
		str =~ /[0-9]+/
		Integer($&)
	end
	def parse_checksum(str)
		# match checksum
		str = str.match(/\[[a-z]+\]/).to_s
		# kill square brackets
		str = str.gsub(/\[|\]/,"")
		str
	end
	def check_room_real
		prevcount = nil
		prevchar = nil
		@checksum.each_char do |c|
			if c == "-" then
				next
			end
			count = @name.count(c)
			if count <= 0 then
				raise BadRoomError.new(@full)
			end
			if prevcount.nil? then
				prevcount = count
				prevchar = c
				next
			end
			if count > prevcount then
				raise BadRoomError.new(@full)
			end
			if count == prevcount && prevchar.ord >= c.ord then
				raise BadRoomError.new(@full)
			end
			prevcount = count
			prevchar = c
		end
	end
	def decrypt_name
		retstr = ""
		@name.each_char do |char|
			if char == "-" then
				retstr += " "
				next
			end
			num = letter_to_number(char)
			num += sector_id
			num = num.modulo(26)
			retstr += number_to_letter(num)
		end
		retstr
	end
	private
	def letter_to_number(c)
		if not c =~ /[a-z]/ then
			raise ArgumentError.new("Lowercase only.")
		end
		c.ord - 97
	end
	def number_to_letter(n)
		if n > 25 || n < 0 then
			raise ArgumentError.new("0 <= n <= 25 is valid.")
		end
		(n+97).chr
	end
	public
	attr_reader :name, :sector_id, :checksum, :full, :decrypted_name
end


def test
	test1 = "aaaaa-bbb-z-y-x-123[abxyz]"
	test2 = "a-b-c-d-e-f-g-h-987[abcde]"
	test3 = "not-a-real-room-404[oarel]"
	bad_test1 = "totally-real-room-200[decoy]"
	# This comes from my input file since the above worked through a bug.
	bad_test2 = "xgvnndadzy-xviyt-omvdidib-863[xzgmn]"

	name1 = EncryptedName.new(test1)
	if name1.name != "aaaaa-bbb-z-y-x" then
		puts name1.name
		raise RuntimeError.new("Bad name1!!")
	end
	if name1.sector_id != 123 then
		puts name1.sector_id
		raise RuntimeError.new("Bad sector_id!!")
	end
	if name1.checksum != "abxyz" then
		puts name1.checksum
		raise RuntimeError.new("Bad checksum!!")
	end

	name2 = EncryptedName.new(test2)
	if name2.name != "a-b-c-d-e-f-g-h" then
		puts name2.name
		raise RuntimeError.new("Bad name2!!")
	end
	if name2.sector_id != 987 then
		puts name2.sector_id
		raise RuntimeError.new("Bad sector_id!!")
	end
	if name2.checksum != "abcde" then
		puts name2.checksum
		raise RuntimeError.new("Bad checksum!!")
	end

	name3 = EncryptedName.new(test3)
	if name3.name != "not-a-real-room" then
		puts name3.name
		raise RuntimeError.new("Bad name3!!")
	end
	if name3.sector_id != 404 then
		puts name3.sector_id
		raise RuntimeError.new("Bad sector_id!!")
	end
	if name3.checksum != "oarel" then
		puts name3.checksum
		raise RuntimeError.new("Bad checksum!!")
	end

	begin
		bad_name1 = EncryptedName.new(bad_test1)
		raise RuntimeError.new("Should have BadRoomError!")
	rescue BadRoomError
	end

	begin
		bad_name2 = EncryptedName.new(bad_test2)
		raise RuntimeError.new("Should have BadRoomError!")
	rescue BadRoomError
	end

	#############################################
	full_test = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]\nxgvnndadzy-xviyt-omvdidib-863[xzgmn]\n"
	rooms = []
	full_test.split("\n").each do |line|
		begin
			room = EncryptedName.new(line)
			rooms.push(room)
		rescue BadRoomError
		end
	end
	if rooms.length != 3 then
		raise RuntimeError.new("Only 3 cases are real rooms!")
	end
	sum = 0
	rooms.each do |room|
		sum += Integer(room.sector_id)
	end
	if sum != 1514 then
		raise RuntimeError.new("Sum of test cases should be 1514")
	end
end

# This next function will stop if the tests fail.
test

f = File.open(input)
rooms = []
f.readlines.each do |line|
	begin
		rooms.push(EncryptedName.new(line))
	rescue BadRoomError
	end
end
f.close

sum = 0

rooms.each do |room|
	sum += Integer(room.sector_id)
end

print "Sum of sector IDs: #{sum}."
puts

rooms.each do |room|
	if room.decrypted_name =~ /north ?pole/ then
		puts "#{room.decrypted_name}: #{room.sector_id}"
	end
end
