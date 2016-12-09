input = "input_7.txt"

class HypernetCellError < RuntimeError; end

class AddressIPV7
	def initialize(address)
		@full_address = address.dup
		@ip_cells = get_ip_cells @full_address
		@hypernet_cells = get_hypernet_cells @full_address
		@support_tls = check_tls
	end
	def support_tls?
		@support_tls
	end
	private
	def check_tls
		@hypernet_cells.each do |string|
			string.each_char.with_index do |char, i|
				if string[i+3].nil? then
					break
				elsif char == string[i+1] then
					next
				elsif char == string[i+3] && string[i+1] == string[i+2] then
					return false
				end
			end
		end

		@ip_cells.each do |string|
			string.each_char.with_index do |char, i|
				if string[i+3].nil? then
					break
				elsif char == string[i+1] then
					next
				elsif char == string[i+3] && string[i+1] == string[i+2] then
					return true
				end
			end
		end
		return false
	end
	def get_ip_cells(address)
		in_hypernet = false
		ip_cells = []
		ip_cell = ""
		address.each_char do |char|
			if char == "[" then
				ip_cells << ip_cell
				ip_cell = ""
				in_hypernet = true
			elsif char == "]" then
				in_hypernet = false
				next
			end

			if in_hypernet then
				next
			end

			ip_cell += char
		end
		if in_hypernet then
			raise HypernetCellError.new("Bad address format: Unterminated hypernet cell for address #@full_address.")
		end
		ip_cells << ip_cell
		ip_cells
	end
	def get_hypernet_cells(address)
		in_hypernet = false
		hypernet_cells = []
		hypernet_cell = ""
		address.each_char do |char|
			if char == "[" then
				in_hypernet = true
				next
			elsif char == "]" then
				in_hypernet = false
				hypernet_cells << hypernet_cell
				hypernet_cell = ""
				in_hypernet = false
				next
			end

			if not in_hypernet then
				next
			end
			hypernet_cell += char
		end
		if in_hypernet then
			raise HypernetCellError.new("Bad address format: Unterminated hypernet cell for address #@full_address.")
		end
		hypernet_cells
	end
	public
	attr_reader :address, :ip_cells, :hypernet_cells
end

def test
	test_message = ""

	good_input_1 = "abba[mnop]qrst"
	output = AddressIPV7.new(good_input_1)
	if not output.support_tls? then
		test_message += "1.1: #{good_input_1} supports TLS, but it is not indicated!\n"
	end
	good_ip_cells_1 = ["abba","qrst"]
	if output.ip_cells != good_ip_cells_1 then
		test_message += "1.2: #{good_input_1} does not result in ip_cells == #{good_ip_cells_1}.  Value is: #{output.ip_cells}\n"
	end
	good_hypernet_cells_1 = ["mnop"]
	if output.hypernet_cells != good_hypernet_cells_1 then
		test_message += "1.3: #{good_input_1} does not result in hypernet_cells == #{good_hypernet_cells_1}.  Value is: #{output.hypernet_cells}\n"
	end

	good_input_2 = "ioxxoj[asdfgh]zxcvbn"
	output = AddressIPV7.new(good_input_2)
	if not output.support_tls? then
		test_message += "2.1: #{good_input_2} supports TLS, but it is not indicated!\n"
	end
	good_ip_cells_2 = ["ioxxoj","zxcvbn"]
	if output.ip_cells != good_ip_cells_2 then
		test_message += "2.2: #{good_input_2} does not result in ip_cells == #{good_ip_cells_2}.  Value is: #{output.ip_cells}\n"
	end
	good_hypernet_cells_2 = ["asdfgh"]
	if output.hypernet_cells != good_hypernet_cells_2 then
		test_message += "2.3: #{good_input_2} does not result in hypernet_cells == #{good_hypernet_cells_2}.  Value is: #{output.hypernet_cells}\n"
	end

	good_input_3 = "asdfghjhjhjk[abfdgesgh]vcdfghjty[lkjufngj]djjdodnfg"
	output = AddressIPV7.new(good_input_3)
	if not output.support_tls? then
		test_message += "3.1: #{good_input_3} supports TLS, but it is not indicated!\n"
	end
	good_ip_cells_3 = ["asdfghjhjhjk","vcdfghjty","djjdodnfg"]
	if output.ip_cells != good_ip_cells_3 then
		test_message += "3.2: #{good_input_3} does not result in ip_cells == #{good_ip_cells_3}.  Value is: #{output.ip_cells}"
	end
	good_hypernet_cells_3 = ["abfdgesgh","lkjufngj"]
	if output.hypernet_cells != good_hypernet_cells_3 then
		test_message += "3.3: #{good_input_3} does not result in hypernet_cells == #{good_hypernet_cells_3}.  Value is: #{output.hypernet_cells}\n"
	end


	bad_input_1 = "abcd[bddb]xyyx"
	output = AddressIPV7.new(bad_input_1)
	if output.support_tls? then
		test_message += "4: #{bad_input_1} does not support TLS, but it is not indicated!\n"
	end

	bad_input_2 = "aaaa[qwer]tyui"
	output = AddressIPV7.new(bad_input_2)
	if output.support_tls? then
		test_message += "5: #{bad_input_2} does not support TLS, but it is not indicated!\n"
	end

	bad_input_3 = "asdfghjhjhjk[abfdggdgh]vcdfghjty[lkjufngj]djjdodnfg"
	output = AddressIPV7.new(bad_input_3)
	if output.support_tls? then
		test_message += "6: #{bad_input_3} does not support TLS, but it is not indicated!\n"
	end


	if test_message != "" then
		puts test_message
		raise RuntimeError.new("Tests failed!")
	end
end

test

f = File.open(input)
addresses = f.readlines
f.close

tls_count = 0
addresses.each do |address|
	if AddressIPV7.new(address).support_tls? then
		tls_count += 1
	end
end

puts "Number of addresses that support TLS: #{tls_count}."
