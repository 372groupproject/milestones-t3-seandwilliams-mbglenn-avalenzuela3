IDENTIFICATION DIVISION.
PROGRAM-ID. ARRAY-TYPE.

DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 ARRAY-STRINGS.
      05 ARRAY-ITEM PIC X(3) VALUE "Hi!" OCCURS 5 TIMES INDEXED BY I.

PROCEDURE DIVISION.
DISPLAY "The array was set to 'Hi!' 5 times, see "ARRAY-STRINGS.
DISPLAY "Here is a loop showing our items each on a newline.".
PERFORM OUR-DISP VARYING I FROM 1 BY 1 UNTIL I>5

MOVE "NEW" TO ARRAY-ITEM(1).
DISPLAY "Now loop with 'NEW' taking place of array[1] (COBOL is not zero based indexing)".
PERFORM OUR-DISP VARYING I FROM 1 BY 1 UNTIL I>5
STOP RUN.

OUR-DISP.
DISPLAY ARRAY-ITEM(I).
