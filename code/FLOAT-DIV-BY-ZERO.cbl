IDENTIFICATION DIVISION.
PROGRAM-ID. FLOAT-DIV-BY-ZERO.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 ValueX PIC 9(3)V99.
PROCEDURE DIVISION.
DISPLAY "We will show how basic primitive floats work in COBOL ValueX = "ValueX.
ADD 35 TO ValueX GIVING ValueX.
DISPLAY "After adding 35 to ValueX, ValueX = "ValueX.
MULTIPLY Valuex BY 10 GIVING ValueX.
DISPLAY "After multiplying by ten to ValueX, ValueX = "ValueX.
ADD .15 TO ValueX GIVING ValueX.
DISPLAY "After adding .15 to ValueX, ValueX = "ValueX.
MULTIPLY ValueX BY 2 GIVING ValueX.
DISPLAY "After multiplying by two to ValueX, ValueX = "ValueX.
MULTIPLY ValueX BY 0 GIVING ValueX.
DISPLAY "After multiplying by 0 to ValueX, ValueX = "ValueX.
ADD 1000000000001 TO ValueX.
DISPLAY "After adding 1 trillion to ValueX, ValueX = "ValueX.
DIVIDE 99 BY 0 GIVING ValueX.
DISPLAY "After dividing 99 by 0 giving ValueX, ValueX = "ValueX.
STOP RUN.
