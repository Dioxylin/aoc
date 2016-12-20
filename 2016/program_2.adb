with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed; 

with Ada.Sequential_IO;

procedure Program_2 is
	package Char_IO is new Ada.Sequential_IO (Character);
	use Char_IO;

	subtype Keypad_Type is Integer range 1..2;

	subtype Row_Range is Integer range 0..6;
	subtype Column_Range is Integer range 0..6;

	subtype Row_Change_Value is Integer range -1..1;
	subtype Column_Change_Value is Integer range -1..1;

	-- 0 is the designated invalid value.
	subtype Key_Value is Integer range 0..13;
	Invalid_Value : constant Key_Value := 0;

	type Coordinates_Record is record
		Row : Row_Range;
		Column : Column_Range;
	end record;

	Current_Key : Coordinates_Record;

	keypad_1 : constant Array (Row_Range,Column_Range) of Key_Value := (
		(0,0,0,0,0,0,0),
		(0,0,0,0,0,0,0),
		(0,0,1,2,3,0,0),
		(0,0,4,5,6,0,0),
		(0,0,7,8,9,0,0),
		(0,0,0,0,0,0,0),
		(0,0,0,0,0,0,0)
	);
	keypad_2 : constant Array (Row_Range,Column_Range) of Key_Value := (
		(0, 0, 0, 0, 0, 0, 0),
		(0, 0, 0, 1, 0, 0, 0),
		(0, 0, 2, 3, 4, 0, 0),
		(0, 5, 6, 7, 8, 9, 0),
		(0, 0,10,11,12, 0, 0),
		(0, 0, 0,13, 0, 0, 0),
		(0, 0, 0, 0, 0, 0, 0)
	);

	keypad : Array(Row_Range,Column_range) of Key_Value := (others => (0,0,0,0,0,0,0));

	function Is_Invalid_Key Return Boolean is
	begin
		return keypad(Current_Key.Row, Current_Key.Column) = Invalid_Value;
	end Is_Invalid_Key;

	Input : Char_IO.File_Type;
	Char : Character;
	Backspace : constant Character := Character'Val(8);

	Row_Change : Row_Change_Value;
	Column_Change : Column_Change_Value;

	Which_Keypad : Keypad_Type := 1;
begin
	-- We jump back up here with Which_Keypad set to 2.
	<<Again>>
	Current_Key := (
		  Row => 2
		, Column => 2
	);
	keypad := (others => (others => 0));
	for I in keypad'Range(1) loop
		for J in keypad'Range(2) loop
			if Which_Keypad = 1 then
				keypad(I,J) := keypad_1(I,J);
			elsif Which_Keypad = 2 then
				keypad(I,J) := keypad_2(I,J);
			else
				Put_Line("Warning: Bad keypad.");
				exit;
			end if;
		end loop;
	end loop;
	Open( File => Input, Mode => In_File, Name => "input_2.txt" );
	-- Put a singular space so we can always backspace before printing.
	Put(" ");
Main_Loop:
	loop
		Read(Input, Char);

		Row_Change := 0;
		Column_Change := 0;

		-- Queue the change for row or column.
		case Char is
			when 'U' => 
				Row_Change := -1;
			when 'D' =>
				Row_Change := +1;
			when 'L' =>
				Column_Change := -1;
			when 'R' =>
				Column_Change := +1;
			when ASCII.CR => null;
			when ASCII.LF => 
				Set_Col(Col+1);
			when others => 
				Put_Line(ASCII.LF & "Unexpected character: " & Integer'Image(Character'Pos(Char)));
		end case;

		-- Change row or column (either the row or column change will be 0)
		Current_Key.Row := Current_Key.Row + Row_Change;
		Current_Key.Column := Current_Key.Column + Column_Change;

		-- Rollback our changes if we need to.
		if Is_Invalid_Key then
			Current_Key.Row := Current_Key.Row - Row_Change;
			Current_Key.Column := Current_Key.Column - Column_Change;
		end if;
		
		-- We already have the last character printed from the last
		-- go-around (and second to last if we have \r\n), so we
		-- exit if we need to.  Otherwise we get an extra character we
		-- don't want.
		exit Main_Loop when End_Of_File(Input);

		-- For dramatic effect, overwrite previous character and put the number, then delay.
		Put_Number_Dramatically: declare
			-- Also compile-time check a valid key here.
			subtype Valid_Key_Value is Key_Value range Key_Value'First+1..Key_Value'Last;
			Keypad_Number : constant Valid_Key_Value := Keypad(Current_Key.Row, Current_Key.Column);
			Keypad_Number_String : constant String := Trim(Integer'Image(Keypad_Number), Ada.Strings.Left);
		begin
			Put(Backspace);
			case Keypad_Number is
				when 1..9 => Put(Keypad_Number_String);
				when 10 => Put("A");
				when 11 => Put("B");
				when 12 => Put("C");
				when 13 => Put("D");
			end case;
		end Put_Number_Dramatically;
		delay 0.001;
	end loop Main_Loop;

	Close(Input);

	if Which_Keypad = 1 then
		Put_Line("is the keypad 1 code.");
		Which_Keypad := 2;
		-- WHEE!
		goto Again;
	end if;
	Put_Line("is the keypad 2 code.");
end Program_2;
