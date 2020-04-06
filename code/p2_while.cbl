IDENTIFICATION DIVISION.
PROGRAM-ID.  While-Loop.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Iterations   PIC 99 VALUE 00.

PROCEDURE DIVISION.
PERFORM VARYING Iterations FROM 0 BY 1 UNTIL Iterations > 10
    DISPLAY "Number of Iterations = ", Iterations
END-PERFORM
STOP RUN.