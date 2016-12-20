all: program_1.exe program_2.exe

program_1.exe: program_1.adb
	gnatmake -gnatwa -Wall program_1.adb

program_2.exe: program_2.adb
	gnatmake -gnatwa -Wall program_2.adb
clean:
	rm -f *.o *.ali *.exe

