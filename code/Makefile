CC=cobc
FLAGS=-xj -free

all: p2_if p2_switch p2_while hello-world dollars-to-euros wheresTheMoney
	
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

clean:
	rm p2_if 
	rm p2_switch 
	rm p2_while hello-world 
	rm dollars-to-euros 
	rm wheresTheMoney
