IDENTIFICATION DIVISION.
PROGRAM-ID. FLOAT-TYPE-ERR.
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
SET "HELLO WORLD!" TO ValueX.
DISPLAY "After setting 'HELLO WORLD!' to ValueX, ValueX = "ValueX.
STOP RUN.
