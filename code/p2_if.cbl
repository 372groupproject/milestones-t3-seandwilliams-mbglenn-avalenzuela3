*> Simple program that shows how if statements work 
*> Name: Sean Williams Michael Glenn Aaron Valenzuela
IDENTIFICATION DIVISION.
PROGRAM-ID. IF-EXPRESSION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC 9(6)V99. 


PROCEDURE DIVISION.
ADD 1 TO X.
IF (X >=0)  THEN
    DISPLAY "X is positive."
ELSE DISPLAY "X is negative."
END-IF
STOP RUN.
