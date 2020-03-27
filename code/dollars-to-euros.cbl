*> PROGRAM FOR CONVERTING DOLLARS TO EUROS
*> Name: SEAN WILLIAMS MICHAEL GLENN AARON VALENZUELA
IDENTIFICATION DIVISION.
PROGRAM-ID. DOLLAR-EURO-CONV.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Dollars PIC 9(6)V99. 
01 Conversion PIC 9(6)V99.


PROCEDURE DIVISION.
DISPLAY 'We will convert your money into Euros ;)'.
DISPLAY 'Enter in the amount of money in your wallet'.
DISPLAY "We can accept up to and including $999999.99"
ACCEPT Dollars.
MULTIPLY Dollars BY 0.94 GIVING Conversion.
DISPLAY 'Your money in $ is equal to €', Conversion.
STOP RUN.
