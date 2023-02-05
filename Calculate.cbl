       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE.
       


       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OPERAND1 PIC 9999.
       01 OPERAND2 PIC 9999.
       01 RESULT PIC 9(12)V99.
       
       PROCEDURE DIVISION.
       0100-START-HERE.
           DISPLAY "Please enter a number: ".
           ACCEPT OPERAND1.
           DISPLAY "Please enter the other number: ".
           ACCEPT OPERAND2.
           COMPUTE RESULT = OPERAND1 / OPERAND2.
           DISPLAY "Dividing operand1 by operand2 = ", RESULT.
           COMPUTE RESULT = OPERAND1 * OPERAND2.
           DISPLAY "Multiplying operand1 by operand2 = ", RESULT.
           COMPUTE RESULT = OPERAND1 + OPERAND2.
           DISPLAY "Adding operand1 to operand2 = ", RESULT.
           COMPUTE RESULT = OPERAND1 - OPERAND2.
           DISPLAY "The difference between operand1 and operand2 = ", 
           RESULT.
       STOP RUN.
       END PROGRAM CALCULATE.
       