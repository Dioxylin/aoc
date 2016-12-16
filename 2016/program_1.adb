with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;

procedure Program_1 is
	package Char_IO is new Ada.Sequential_IO (Character);
	use Char_IO;

	type Direction is (North, East, South, West);

	Input : Char_IO.File_Type;
	Buffer : Unbounded_String := To_Unbounded_String("");
	Char : Character;
	Magnitude : Integer;
	Blocks_Away : Integer := 0;

	Blocks_North : Integer := 0;
	Blocks_East : Integer := 0;

	Current_Heading : Direction := North;
begin
	Open(File => Input, Mode => In_File, Name => "input_1.txt");
	loop
		Read(File => Input, Item => Char);
		if Char = 'L' then
			case Current_Heading is
				when East => Current_Heading := North;
				when South => Current_Heading := East;
				when West => Current_Heading := South;
				when North => Current_Heading := West;
			end case;
		elsif Char = 'R' then
			case Current_Heading is
				when North => Current_Heading := East;
				when East => Current_Heading := South;
				when South => Current_Heading := West;
				when West => Current_Heading := North;
			end case;
		elsif Char >= '0' and then Char <= '9' then
			Buffer := Buffer & Char;
		elsif Char = ',' or else End_Of_File(Input) then
			Magnitude := Integer'Value(To_String(Buffer));
			Buffer := To_Unbounded_String("");
			if Current_Heading = South or Current_Heading = West then
				Magnitude := -Magnitude;
			end if;
			case Current_Heading is
				when North|South => Blocks_North := Blocks_North + Magnitude;
				when East|West => Blocks_East := Blocks_East + Magnitude;
			end case;
		elsif Char = ' ' then
			null;
		elsif Char = ASCII.LF or Char = ASCII.CR then
			null;
		else
			Put_Line("Invalid character: """ & Char & """.");
		end if;
		exit when End_Of_File(Input);
	end loop;
	Close(Input);

	if Blocks_North < 0 then
		Blocks_North := -Blocks_North;
	end if;
	if Blocks_East < 0 then
		Blocks_East := -Blocks_East;
	end if;
	Blocks_Away := Blocks_North + Blocks_East;
	Put_Line("Blocks away: " & Integer'Image(Blocks_Away) & ".");
end Program_1;
