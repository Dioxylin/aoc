input = "input_6.txt"

class JammedCommunications
	MOST_LIKELY = "max"
	LEAST_LIKELY = "min"
	def initialize(input, strategy=JammedCommunications::MOST_LIKELY)
		if strategy != MOST_LIKELY && strategy != LEAST_LIKELY then
			raise BadStrategyError.new("Bad strategy.  Use only MOST_LIKELY or LEAST_LIKELY.")
		end
		@method = strategy
		if input.instance_of?(String) then
			@input_array = input.chomp.split("\n")
		elsif input.instance_of?(Array) then
			@input_array = input
		else
			raise RuntimeError.new("Unrecognized input class.  Input class was: #{input.class}")
		end
		transposed_input = transpose_message
		@recovered_message = recover_message(transposed_input)
	end
	private
	def transpose_message
		# ["abcde","fghij","klmno"]
		input_expanded = @input_array.map { |string| string.split("") }
		# [["a","b","c","d","e"], ["f","g",...], ["k","l",...]]
		transposed_expanded = input_expanded.transpose
		# [["a","f","k"], ["b", "g", "l"], ...]
		transposed_input = transposed_expanded.map { |array| array.join("") }
		# ["afk","bgl","chm",...]
		transposed_input
	end
	def recover_message(transposed_input)
		output = ""
		transposed_input.each do |string|
			output += most_common_char(string)
		end
		output
	end
	def most_common_char(string)
		char_counts = string.chars.map do |char|
			string.count(char)
		end
		char_num = char_counts.send(@method)
		char_pos = char_counts.index(char_num)
		string[char_pos]
	end
	public
	def to_s
		@recovered_message.dup.chomp
	end
end


def test
	test_message = ""
	test_input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar\n"
	test_output = JammedCommunications.new(test_input).to_s

	if test_output != "easter" then
		test_message += "Test output is not \"easter\"!  Output is \"#{test_output}\"."
	end

	test_output = JammedCommunications.new(test_input, JammedCommunications::LEAST_LIKELY).to_s

	if test_output != "advent" then
		test_message += "Test output is not \"advent\"!  Output is \"#{test_output}\"."
	end

	test_message
end

message = test
if message != "" then
	raise RuntimeError.new("Test failed: #{message}")
end


f = File.open(input)
input_array = f.readlines
f.close

puts "Using maximum: #{JammedCommunications.new(input_array)}."
puts "Using minimum: #{JammedCommunications.new(input_array, "min")}."
