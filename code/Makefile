CC=cobc
#FLAGS=-xj -free #x builds an executable, j runs the program after compile, free removes column restrictions
FLAGS=-x -free

all: p2_if p2_switch p2_while hello-world dollars-to-euros wheresTheMoney int-type array-type INT-TYPE-DIV-BY-ZERO string-type FLOAT-DIV-BY-ZERO string-type FLOAT-DIV-BY-ZERO float-type ARRAY-ARITHMETIC-ERR ARITHMETIC-OVERFLOW-ERR
	
install_compiler: 
	sudo apt-get update
	sudo apt-get install gnucobol

p2_if: p2_if.cbl
	$(CC) $(FLAGS) p2_if.cbl
	
p2_switch: p2_switch.cbl
	$(CC) $(FLAGS) p2_switch.cbl
	
p2_while: p2_while.cbl
	$(CC) $(FLAGS) p2_while.cbl
	
hello-world: hello-world.cbl
	$(CC) $(FLAGS) hello-world.cbl
	
dollars-to-euros: dollars-to-euros.cbl
	$(CC) $(FLAGS) dollars-to-euros.cbl

wheresTheMoney: wheresTheMoney.cbl
	$(CC) $(FLAGS) wheresTheMoney.cbl

ARRAY-ARITHMETIC-ERR: ARRAY-ARITHMETIC-ERR.cbl
	-$(CC) $(FLAGS) ARRAY-ARITHMETIC-ERR.cbl

int-type: int-type.cbl
	$(CC) $(FLAGS) int-type.cbl

array-type: array-type.cbl
	$(CC) $(FLAGS) array-type.cbl

INT-TYPE-DIV-BY-ZERO: INT-TYPE-DIV-BY-ZERO.cbl
	$(CC) $(FLAGS) INT-TYPE-DIV-BY-ZERO.cbl

string-type: string-type.cbl
	$(CC) $(FLAGS) string-type.cbl

FLOAT-DIV-BY-ZERO: FLOAT-DIV-BY-ZERO.cbl
	$(CC) $(FLAGS) FLOAT-DIV-BY-ZERO.cbl

float-type: float-type.cbl
	$(CC) $(FLAGS) float-type.cbl

ARITHMETIC-OVERFLOW-ERR: ARITHMETIC-OVERFLOW-ERR.cbl
	$(CC) $(FLAGS) ARITHMETIC-OVERFLOW-ERR.cbl
clean:
	-rm p2_if 
	-rm p2_switch 
	-rm p2_while hello-world 
	-rm dollars-to-euros 
	-rm wheresTheMoney
	-rm array-type
	-rm float-type
	-rm INT-TYPE-DIV-BY-ZERO
	-rm FLOAT-DIV-BY-ZERO
	-rm int-type
	-rm string-type
	-rm ARITHMETIC-OVERFLOW-ERR
