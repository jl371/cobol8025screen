      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 i PIC 9(2) VALUE 1.
       01 j PIC 9(2) VALUE 1.
       01  WS-CR     PIC X    VALUE X'0D'.
       01 SCREN.
           05 SCRENC OCCURS 25 TIMES.
           10 SCRENCHR PIC A(2).
           10 SCRENR OCCURS 80 TIMES.
               15 SCRENCHRT PIC X(1) VALUE '-'.
       01 x PIC 9(2) VALUE 1.
       01 y PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           COMPUTE x = 20.
           COMPUTE y = 20.
           PERFORM PLOTPIXEL-PROCEDURE.
           PERFORM DISPLAY-PROCEDURE.
            STOP RUN.
       DISPLAY-PROCEDURE.
           PERFORM UNTIL i > 25
               PERFORM UNTIL j > 80
                   DISPLAY SCRENCHRT(i,j) WITH NO ADVANCING
                   ADD 1 TO j GIVING j
               END-PERFORM
               DISPLAY ' '
               COMPUTE j = 1
               ADD 1 TO i GIVING i
           END-PERFORM.
       PLOTPIXEL-PROCEDURE.
           INSPECT SCRENCHRT(x, y) REPLACING CHARACTERS BY '#'.
       END PROGRAM YOUR-PROGRAM-NAME.
