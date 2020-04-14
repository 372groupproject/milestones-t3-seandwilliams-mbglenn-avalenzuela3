
IDENTIFICATION DIVISION.
PROGRAM-ID. INT-TYPE-DIV-BY-ZERO.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 ValueX PIC 999.
PROCEDURE DIVISION.
DISPLAY "We will show how basic primitive ints work in COBOL ValueX = "ValueX.
ADD 35 TO ValueX GIVING ValueX.
DISPLAY "After adding 35 to ValueX, ValueX = "ValueX.
MULTIPLY Valuex BY 10 GIVING ValueX.
DISPLAY "After multiplying by ten to ValueX, ValueX = "ValueX.
DIVIDE ValueX BY 0 GIVING ValueX.
DISPLAY "After dividing by zero to ValueX, ValueX = "ValueX.
STOP RUN.
