IDENTIFICATION DIVISION.
PROGRAM-ID. WHERES_THE_MONEY.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Salary PIC 9(7)V99 Value 0000001.00. 
01 Rent PIC 9(7)V99.
01 Bills PIC 9(7)V99.
01 Food PIC 9(7)V99.
01 Travel PIC 9(7)V99.
01 Tax_Percent PIC 9(7)V99.
01 Yearly_Rent PIC 9(7)V99.
01 Yearly_Bills PIC 9(7)V99.
01 Yearly_Food PIC 9(7)V99.
01 Yearly_Taxes PIC 9(7)V99.
01 Extra PIC 9(7)V99.
01 Rent_Percent PIC 9(7)V99.
01 Bills_Percent PIC 9(7)V99.
01 Food_Percent PIC 9(7)V99.
01 Travel_Percent PIC 9(7)V99.
01 Taxes_Percent PIC 9(7)V99.
01 Extra_Percent PIC 9(7)V99.


PROCEDURE DIVISION.
DISPLAY '--------------------------------------------------------------------------------------------------'.
DISPLAY '-----------------------------WHERES THE MONEY? YOU BETTER BUDGET!---------------------------------'.
DISPLAY '--------------------------------------------------------------------------------------------------'.
DISPLAY "We can accept up to and including $9999999.99".
DISPLAY 'First enter your salary, then enter your expenses in the order of your'.
DISPLAY 'rent/mortgage, bills, food expenses, and travel expenses. Then enter your tax percentage.'.
DISPLAY 'To correctly EXIT the program, enter 0 as your salary. Entering 0 for anything else will result in an error.'.
PERFORM UNTIL Salary = 0
    ACCEPT Salary
    IF (Salary = 0) THEN
        STOP RUN
    END-IF
    ACCEPT Rent
    IF (Rent = 0) THEN
        DISPLAY 'INPUT ERROR: You dont pay a rent or a mortgage? Looks like you dont need my help! Rent must be over $0.'
        STOP RUN
    END-IF
    ACCEPT Bills
    IF (Bills = 0) THEN
        DISPLAY 'INPUT ERROR: WOW you pay no bills huh? MUST BE NICE. Bills must be over $0.'
        STOP RUN
    END-IF
    ACCEPT Food
    IF (Food = 0) THEN
        DISPLAY 'INPUT ERROR: You mean to tell me you NEVER eat?! Lies! Food expense must be over $0.'
        STOP RUN
    END-IF
    ACCEPT Travel
    IF (Travel = 0) THEN
        DISPLAY 'INPUT ERROR: Travel includes car expenses, bike expenses, bus fares, etc. Must be at least $1.'
        STOP RUN
    END-IF
    ACCEPT Tax_Percent
    IF (Tax_Percent = 0) OR (Tax_Percent > 100) THEN
        DISPLAY 'INPUT ERROR: 2 things you cant avoid in life, death and taxes. Tax percentage must be between 0% and 100%.'
        STOP RUN
    END-IF
    DISPLAY 'Annual salary $', Salary
    DISPLAY 'Monthly mortgage/rent: $', Rent
    DISPLAY 'Monthly bills: $', Bills
    DISPLAY 'Weekly grocery/food expenses: $', Food
    DISPLAY 'Annual travel expenses: $', Travel
    DISPLAY 'Tax Percentage: $', Tax_Percent
    COMPUTE Yearly_Rent = Rent * 12
    COMPUTE Yearly_Bills = Bills * 12
    COMPUTE Yearly_Food = Food * 52
    COMPUTE Yearly_Taxes = Salary * (Tax_Percent / 100)
    COMPUTE Extra = Salary - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + Travel)
    COMPUTE Rent_Percent = (Yearly_Rent/Salary) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/Salary) * 100
    COMPUTE Food_Percent = (Yearly_Food/Salary) * 100
    COMPUTE Travel_Percent = (Travel/Salary) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/Salary) * 100
    COMPUTE Extra_Percent = (Extra/Salary) * 100
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of $', Salary
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | $', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | $', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | $', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | $', Travel, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | $', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | $', Extra, '  |  ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'
END-PERFORM.
STOP RUN.
