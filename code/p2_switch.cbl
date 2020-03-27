*> Program that "gives" financial advice (just to show how a switch case works
*> NAME: Sean Williams Michael Glenn Aaron Valenzuela
IDENTIFICATION DIVISION.
PROGRAM-ID. FINANCIAL-ADVICE.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Money PIC 9(6)V99. 


PROCEDURE DIVISION.
ACCEPT Money.
EVALUATE TRUE 
    WHEN Money < 5 DISPLAY "broke."
    WHEN Money <100 DISPLAY "You are still broke."
    WHEN Money <=1000 DISPLAY "You need to save up more."
    WHEN Money <=5000 DISPLAY "You are doing better."
    WHEN Money >5000 DISPLAY "You dont need advice!"
END-EVALUATE
STOP RUN.
