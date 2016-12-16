with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

-- Generic packages.
with Ada.Sequential_IO;
with Ada.Containers.Vectors;

procedure Program_1 is
	type Coordinates_Record is record
		North : Integer;
		East : Integer;
	end record;

	package Char_IO is new Ada.Sequential_IO (Character);
	use Char_IO;

	package Coordinates_Vector is new Ada.Containers.Vectors (Natural, Coordinates_Record);
	use Coordinates_Vector;

	Visited_Coordinates : Coordinates_Vector.Vector;

	type Direction is (North, East, South, West);
	type Relative_Direction is (Left, Right);

	procedure Change_Heading(Relative_Dir : in Relative_Direction; Dir : in out Direction) is
	begin
		if Relative_Dir = Left then
			case Dir is
				when East => Dir := North;
				when South => Dir := East;
				when West => Dir := South;
				when North => Dir := West;
			end case;
		elsif Relative_Dir = Right then
			case Dir is
				when North => Dir := East;
				when East => Dir := South;
				when South => Dir := West;
				when West => Dir := North;
			end case;
		end if;
	end Change_Heading;

	-- Yay our types.
	Input : Char_IO.File_Type;
	Buffer : Unbounded_String := To_Unbounded_String("");
	Char : Character;
	Magnitude : Integer;
	Blocks_Away : Integer := 0;

	Coordinates : Coordinates_Record;
	Blocks_North : Integer := 0;
	Blocks_East : Integer := 0;

	--Current_Block : Integer;
	Found_HQ : Boolean := false;

	Current_Heading : Direction := North;
begin
	Coordinates.North := 0;
	Coordinates.East := 0;
	Visited_Coordinates := Visited_Coordinates & Coordinates;
	Open(File => Input, Mode => In_File, Name => "input_1.txt");
	loop
		Read(File => Input, Item => Char);
		if Char = 'L' then
			Change_Heading(Left, Current_Heading);
		elsif Char = 'R' then
			Change_Heading(Right, Current_Heading);
		elsif Char >= '0' and then Char <= '9' then
			Buffer := Buffer & Char;
		elsif Char = ',' or else End_Of_File(Input) then
			Magnitude := Integer'Value(To_String(Buffer));
			Buffer := To_Unbounded_String("");

			-- Not sure how to pop everything into a procedure...  Lots of code duped.
			case Current_Heading is
				when North =>
					Blocks_North := Blocks_North + Magnitude;
					if not Found_HQ then
						for Block in Coordinates.North+1 .. Blocks_North loop
							Coordinates.North := Block;
							if Contains(Visited_Coordinates, Coordinates) then
								Found_HQ := true;
							end if;
							Visited_Coordinates := Visited_Coordinates & Coordinates;
							exit when Found_HQ;
						end loop;
					end if;
				when South =>
					Blocks_North := Blocks_North - Magnitude;
					if not Found_HQ then
						for Block in reverse Blocks_North .. Coordinates.North-1 loop
							Coordinates.North := Block;
							if Contains(Visited_Coordinates, Coordinates) then
								Found_HQ := true;
							end if;
							Visited_Coordinates := Visited_Coordinates & Coordinates;
							exit when Found_HQ;
						end loop;
					end if;
				when East =>
					Blocks_East := Blocks_East + Magnitude;
					if not Found_HQ then
						for Block in Coordinates.East+1 .. Blocks_East loop
							Coordinates.East := Block;
							if Contains(Visited_Coordinates, Coordinates) then
								Found_HQ := true;
							end if;
							Visited_Coordinates := Visited_Coordinates & Coordinates;
							exit when Found_HQ;
						end loop;
					end if;
				when West =>
					Blocks_East := Blocks_East - Magnitude;
					if not Found_HQ then
						for Block in reverse Blocks_East .. Coordinates.east-1 loop
							Coordinates.East := Block;
							if Contains(Visited_Coordinates, Coordinates) then
								Found_HQ := true;
							end if;
							Visited_Coordinates := Visited_Coordinates & Coordinates;
							exit when Found_HQ;
						end loop;
					end if;
			end case;
		elsif Char = ' ' or Char = ASCII.LF or Char = ASCII.CR then
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
	Put_Line("HQ is at " & Integer'Image(Coordinates.North) & "," & Integer'Image(Coordinates.East) & ".");
	if Coordinates.North < 0 then
		Coordinates.North := -Coordinates.North;
	end if;
	if Coordinates.East < 0 then
		Coordinates.East := -Coordinates.East;
	end if;
	Blocks_Away := Coordinates.North + Coordinates.East;
	Put_Line("HQ is " & Integer'Image(Blocks_Away) & " blocks away.");
end Program_1;
