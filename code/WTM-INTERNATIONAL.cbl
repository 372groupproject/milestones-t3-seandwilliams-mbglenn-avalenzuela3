IDENTIFICATION DIVISION.
PROGRAM-ID. WTM-INTERNATIONAL.

ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.
      SELECT LEDGER ASSIGN TO 'input.txt'
      ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD LEDGER.
01 LEDGER-FILE.
   05 LEDGER-SALARY PIC 9(7)V99.
   05 LEDGER-RENT PIC 9(7)V99.
   05 LEDGER-BILLS PIC 9(7)V99.
   05 LEDGER-FOOD PIC 9(7)V99.
   05 LEDGER-TRAVEL PIC 9(7)V99.
   05 LEDGER-TAXES PIC 9(3)V99.

WORKING-STORAGE SECTION.
01 WS-LEDGER.
   05 WS-LEDGER-SALARY PIC 9(7)V99.
   05 WS-LEDGER-RENT PIC 9(7)V99.
   05 WS-LEDGER-BILLS PIC 9(7)V99.
   05 WS-LEDGER-FOOD PIC 9(7)V99.
   05 WS-LEDGER-TRAVEL PIC 9(7)V99.
   05 WS-LEDGER-TAXES PIC 9(3)V99.

01 Yearly_Rent PIC 9(7)V99.
01 Yearly_Bills PIC 9(7)V99.
01 Yearly_Food PIC 9(7)V99.
01 Yearly_Taxes PIC 9(7)V99.
01 Extra PIC S9(7)V99.
01 Rent_Percent PIC 9(3)V99.
01 Bills_Percent PIC 9(3)V99.
01 Food_Percent PIC 9(3)V99.
01 Travel_Percent PIC 9(3)V99.
01 Taxes_Percent PIC 9(3)V99.
01 Extra_Percent PIC S9(3)V99.
01 Curr PIC X(15) VALUE SPACE.

PROCEDURE DIVISION.

OPEN INPUT LEDGER.
READ LEDGER INTO WS-LEDGER
END-READ.
CLOSE LEDGER.

DISPLAY '--------------------------------------------------------------------------------------------------'.
DISPLAY '-------------------------------WHERES THE MONEY? INTERNATIONAL!-----------------------------------'.
DISPLAY '--------------------------------------------------------------------------------------------------'.
DISPLAY "We will convert your $ into one of six popular global currencies."
DISPLAY "You will be able to see what your budget would look like after conversion"
DISPLAY "Please enter the curency you would like to use for your budgeting plan".
DISPLAY "We can accept 'Euros'(Europe) 'Dollars'(US) 'Krona'(Swedish)"
DISPLAY "'Yen'(Japanese) 'Pounds'(Britain) 'Franc'(Swiss)"

ACCEPT Curr.
MOVE Curr(1:3) TO Curr.
IF (Curr IS EQUAL TO "Eur") THEN
    PERFORM EUROS-SECT 1 TIMES
END-IF
IF (Curr IS EQUAL TO "Dol") THEN
    PERFORM DOLLARS-SECT 1 TIMES
END-IF
IF (Curr IS EQUAL TO "Kro") THEN
    PERFORM KRONA-SECT 1 TIMES
END-IF
IF (Curr IS EQUAL TO "Yen") THEN
    PERFORM YEN-SECT 1 TIMES
END-IF
IF (Curr IS EQUAL TO "Pou") THEN
    PERFORM POUNDS-SECT 1 TIMES
END-IF
IF (Curr IS EQUAL TO "Fra") THEN
    PERFORM FRANC-SECT 1 TIMES
END-IF
STOP RUN.

FRANC-SECT.
    MULTIPLY WS-LEDGER-SALARY BY .97 GIVING WS-LEDGER-SALARY
    MULTIPLY WS-LEDGER-RENT BY .97 GIVING WS-LEDGER-RENT
    MULTIPLY WS-LEDGER-FOOD BY .97 GIVING WS-LEDGER-FOOD
    MULTIPLY WS-LEDGER-TRAVEL BY .97 GIVING WS-LEDGER-TRAVEL
    MULTIPLY WS-LEDGER-BILLS BY .97 GIVING WS-LEDGER-BILLS

    DISPLAY 'Annual salary SFr. ', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: SFr. ', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: SFr. ', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: SFr. ', WS-LEDGER-FOOD
    DISPLAY 'Annual travel expenses: SFr. ', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: SFr. ', LEDGER-TAXES

    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100

    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of SFr. ', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | SFr. ', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | SFr. ', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | SFr. ', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | SFr. ', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | SFr. ', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | SFr. ', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.

POUNDS-SECT.
    MULTIPLY WS-LEDGER-SALARY BY .81 GIVING WS-LEDGER-SALARY
    MULTIPLY WS-LEDGER-RENT BY .81 GIVING WS-LEDGER-RENT
    MULTIPLY WS-LEDGER-FOOD BY .81 GIVING WS-LEDGER-FOOD
    MULTIPLY WS-LEDGER-TRAVEL BY .81 GIVING WS-LEDGER-TRAVEL
    MULTIPLY WS-LEDGER-BILLS BY .81 GIVING WS-LEDGER-BILLS

    DISPLAY 'Annual salary £', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: £', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: £', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: £', WS-LEDGER-FOOD
    DISPLAY 'Annual travel expenses: £', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: £', LEDGER-TAXES

    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100

    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of £', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | £', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | £', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | £', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | £', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | £', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | £', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.

YEN-SECT.
    MULTIPLY WS-LEDGER-SALARY BY 107.6 GIVING WS-LEDGER-SALARY
    MULTIPLY WS-LEDGER-RENT BY 107.6 GIVING WS-LEDGER-RENT
    MULTIPLY WS-LEDGER-FOOD BY 107.6 GIVING WS-LEDGER-FOOD
    MULTIPLY WS-LEDGER-TRAVEL BY 107.6 GIVING WS-LEDGER-TRAVEL
    MULTIPLY WS-LEDGER-BILLS BY 107.6 GIVING WS-LEDGER-BILLS

    DISPLAY 'Annual salary ¥', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: ¥', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: ¥', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: ¥', WS-LEDGER-FOOD
    DISPLAY 'Annual travel expenses: ¥', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: ¥', LEDGER-TAXES

    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100

    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of ¥', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | ¥', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | ¥', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | ¥', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | ¥', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | ¥', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | ¥', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.

EUROS-SECT.
    MULTIPLY WS-LEDGER-SALARY BY .92 GIVING WS-LEDGER-SALARY
    MULTIPLY WS-LEDGER-RENT BY .92 GIVING WS-LEDGER-RENT
    MULTIPLY WS-LEDGER-FOOD BY .92 GIVING WS-LEDGER-FOOD
    MULTIPLY WS-LEDGER-TRAVEL BY .92 GIVING WS-LEDGER-TRAVEL
    MULTIPLY WS-LEDGER-BILLS BY .92 GIVING WS-LEDGER-BILLS

    DISPLAY 'Annual salary €', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: €', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: €', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: €', WS-LEDGER-FOOD
    DISPLAY 'Annual travel expenses: €', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: €', LEDGER-TAXES

    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100

    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of €', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | €', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | €', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | €', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | €', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | €', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | €', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.

KRONA-SECT.
    MULTIPLY WS-LEDGER-SALARY BY 10.04 GIVING WS-LEDGER-SALARY
    MULTIPLY WS-LEDGER-RENT BY 10.04 GIVING WS-LEDGER-RENT
    MULTIPLY WS-LEDGER-FOOD BY 10.04 GIVING WS-LEDGER-FOOD
    MULTIPLY WS-LEDGER-TRAVEL BY 10.04 GIVING WS-LEDGER-TRAVEL
    MULTIPLY WS-LEDGER-BILLS BY 10.04 GIVING WS-LEDGER-BILLS

    DISPLAY 'Annual salary kr', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: kr', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: kr', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: kr', WS-LEDGER-FOOD
    DISPLAY 'Annual travel expenses: kr', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: kr', LEDGER-TAXES

    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100

    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of kr', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | kr', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | kr', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | kr', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | kr', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | kr', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | kr', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.

DOLLARS-SECT.
    DISPLAY 'Annual salary $', WS-LEDGER-SALARY
    DISPLAY 'Monthly mortgage/rent: $', WS-LEDGER-RENT
    DISPLAY 'Monthly bills: $', WS-LEDGER-BILLS
    DISPLAY 'Weekly grocery/food expenses: $', WS-LEDGER-FOOD
    DISPLAY 'Annual WS-LEDGER-TRAVEL expenses: $', WS-LEDGER-TRAVEL
    DISPLAY 'Tax Percentage: $', LEDGER-TAXES
    COMPUTE Yearly_Rent = WS-LEDGER-RENT * 12
    COMPUTE Yearly_Bills = WS-LEDGER-BILLS * 12
    COMPUTE Yearly_Food = WS-LEDGER-FOOD * 52
    COMPUTE Yearly_Taxes = WS-LEDGER-SALARY * (LEDGER-TAXES / 100)
    COMPUTE Extra = WS-LEDGER-SALARY - (Yearly_Rent + Yearly_Bills + Yearly_Food + Yearly_Taxes + WS-LEDGER-TRAVEL)
    COMPUTE Rent_Percent = (Yearly_Rent/WS-LEDGER-SALARY) * 100
    COMPUTE Bills_Percent = (Yearly_Bills/WS-LEDGER-SALARY) * 100
    COMPUTE Food_Percent = (Yearly_Food/WS-LEDGER-SALARY) * 100
    COMPUTE Travel_Percent = (WS-LEDGER-TRAVEL/WS-LEDGER-SALARY) * 100
    COMPUTE Taxes_Percent = (Yearly_Taxes/WS-LEDGER-SALARY) * 100
    COMPUTE Extra_Percent = (Extra/WS-LEDGER-SALARY) * 100
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY 'See the financial breakdown below, based on a salary of $', WS-LEDGER-SALARY
    DISPLAY '--------------------------------------------------------------------------------------------------'
    DISPLAY '| Mortgage/Rent | $', Yearly_Rent, '  |  ', Rent_Percent, '% | #########################'
    DISPLAY '| Bills         | $', Yearly_Bills, '  |  ', Bills_Percent, '% | #########################'
    DISPLAY '| Food          | $', Yearly_Food, '  |  ', Food_Percent, '% | #########################'
    DISPLAY '| Travel        | $', WS-LEDGER-TRAVEL, '  |  ', Travel_Percent, '% | #########################'
    DISPLAY '| Taxes         | $', Yearly_Taxes, '  |  ', Taxes_Percent, '% | #########################'
    DISPLAY '| Extra         | $', Extra, ' | ', Extra_Percent, '% | #########################'
    DISPLAY '--------------------------------------------------------------------------------------------------'.
