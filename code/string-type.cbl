IDENTIFICATION DIVISION.
PROGRAM-ID. STRING-TYPE.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 ValueX PIC X(15) VALUE SPACE.

PROCEDURE DIVISION.
DISPLAY "We will show how basic primitive strings work in COBOL ValueX = "ValueX.
STRING ValueX DELIMITED BY SPACE "HELLO WORLD!" DELIMITED BY SIZE INTO ValueX.
DISPLAY "Primitive string using concatenation with 'HELLO WORLD!' = "ValueX.
STOP RUN.
