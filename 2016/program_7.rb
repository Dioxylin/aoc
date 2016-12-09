input = "input_7.txt"

class HypernetCellError < RuntimeError; end

class AddressIPV7
	def initialize(address)
		@full_address = address.dup
		@supernet_cells = get_supernet_cells @full_address
		@hypernet_cells = get_hypernet_cells @full_address
		@support_tls = check_tls
		@support_ssl = check_ssl
	end
	def support_tls?
		@support_tls
	end
	def support_ssl?
		@support_ssl
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

		@supernet_cells.each do |string|
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
	def check_ssl
		aba_sequences = []
		@supernet_cells.each do |cell|
			cell.each_char.with_index do |_, i|
				if cell[i+2].nil? then
					break
				elsif cell[i] != cell[i+1] && cell[i] == cell[i+2] then
					aba_sequences << cell[i..i+2]
				end
			end
		end
		corresponding_bab_sequences = aba_sequences.map do |sequence|
			"" + sequence[1] + sequence[0] + sequence[1]
		end

		bab_sequences = []
		@hypernet_cells.each do |cell|
			cell.each_char.with_index do |_, i|
				if cell[i+2].nil? then
					break
				elsif cell[i] != cell[i+1] && cell[i] == cell[i+2] then
					bab_sequences << cell[i..i+2]
				end
			end
		end
		bab_sequences.each do |seq|
			if corresponding_bab_sequences.include?(seq) then
				return true
			end
		end
		return false
	end
	def get_supernet_cells(address)
		in_hypernet = false
		supernet_cells = []
		ip_cell = ""
		address.each_char do |char|
			if char == "[" then
				supernet_cells << ip_cell
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
		supernet_cells << ip_cell
		supernet_cells
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
	attr_reader :address, :supernet_cells, :hypernet_cells
end

def test
	test_message = ""

	good_input_1 = "abbab[mabanop]qrst"
	output = AddressIPV7.new(good_input_1)
	if not output.support_tls? then
		test_message += "1.1: #{good_input_1} supports TLS, but it is not indicated!\n"
	end
	good_supernet_cells_1 = ["abbab","qrst"]
	if output.supernet_cells != good_supernet_cells_1 then
		test_message += "1.2: #{good_input_1} does not result in supernet_cells == #{good_supernet_cells_1}.  Value is: #{output.supernet_cells}\n"
	end
	good_hypernet_cells_1 = ["mabanop"]
	if output.hypernet_cells != good_hypernet_cells_1 then
		test_message += "1.3: #{good_input_1} does not result in hypernet_cells == #{good_hypernet_cells_1}.  Value is: #{output.hypernet_cells}\n"
	end
	if not output.support_ssl? then
		test_message += "1.4: #{good_input_1} supports SSL, but this is not indicated!\n"
	end

	good_input_2 = "ioxxojij[asdfijigh]zxcvbn"
	output = AddressIPV7.new(good_input_2)
	if not output.support_tls? then
		test_message += "2.1: #{good_input_2} supports TLS, but it is not indicated!\n"
	end
	good_supernet_cells_2 = ["ioxxojij","zxcvbn"]
	if output.supernet_cells != good_supernet_cells_2 then
		test_message += "2.2: #{good_input_2} does not result in supernet_cells == #{good_supernet_cells_2}.  Value is: #{output.supernet_cells}\n"
	end
	good_hypernet_cells_2 = ["asdfijigh"]
	if output.hypernet_cells != good_hypernet_cells_2 then
		test_message += "2.3: #{good_input_2} does not result in hypernet_cells == #{good_hypernet_cells_2}.  Value is: #{output.hypernet_cells}\n"
	end
	if not output.support_ssl? then
		test_message += "2.4: #{good_input_2} supports SSL, but this is not indicated!\n"
	end

	good_input_3 = "asdfghjhjhjk[abfdgesgh]vcdfghjty[lkjuhjhjhjhjhjhfngj]djjdodnfg"
	output = AddressIPV7.new(good_input_3)
	if not output.support_tls? then
		test_message += "3.1: #{good_input_3} supports TLS, but it is not indicated!\n"
	end
	good_supernet_cells_3 = ["asdfghjhjhjk","vcdfghjty","djjdodnfg"]
	if output.supernet_cells != good_supernet_cells_3 then
		test_message += "3.2: #{good_input_3} does not result in supernet_cells == #{good_supernet_cells_3}.  Value is: #{output.supernet_cells}"
	end
	good_hypernet_cells_3 = ["abfdgesgh","lkjuhjhjhjhjhjhfngj"]
	if output.hypernet_cells != good_hypernet_cells_3 then
		test_message += "3.3: #{good_input_3} does not result in hypernet_cells == #{good_hypernet_cells_3}.  Value is: #{output.hypernet_cells}\n"
	end
	if not output.support_ssl? then
		test_message += "3.4: #{good_input_3} supports SSL, but this is not indicated!\n"
	end


	bad_input_1 = "abcd[bddb]xyyx"
	output = AddressIPV7.new(bad_input_1)
	if output.support_tls? then
		test_message += "4.1: #{bad_input_1} does not support TLS, but it is not indicated!\n"
	end
	if output.support_ssl? then
		test_message += "4.2: #{bad_input_1} does not support SSL, but this is not indicated!\n"
	end


	bad_input_2 = "aaaa[qwer]tyui"
	output = AddressIPV7.new(bad_input_2)
	if output.support_tls? then
		test_message += "5.1: #{bad_input_2} does not support TLS, but it is not indicated!\n"
	end
	if output.support_ssl? then
		test_message += "5.2: #{bad_input_2} does not support SSL, but this is not indicated!\n"
	end


	bad_input_3 = "asdfghjhjhjk[abfdggdgh]vcdfghjty[lkjufngj]djjdodnfg"
	output = AddressIPV7.new(bad_input_3)
	if output.support_tls? then
		test_message += "6.1: #{bad_input_3} does not support TLS, but it is not indicated!\n"
	end
	if output.support_ssl? then
		test_message += "6.2: #{bad_input_3} does not support SSL, but this is not indicated!\n"
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
ssl_count = 0
addresses.each do |address|
	addr = AddressIPV7.new(address)
	if addr.support_tls? then
		tls_count += 1
	end
	if addr.support_ssl? then
		ssl_count += 1
	end
end

puts "Number of addresses that support TLS: #{tls_count}."
puts "Number of addresses that support SSL: #{ssl_count}."
