      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 8025test.
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
               15 SCRENCHRT PIC X(1) VALUE ' '.
       01 x PIC 9(2) VALUE 1.
       01 y PIC 9(2) VALUE 1.
       01 linelength PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           COMPUTE x = 20.
           COMPUTE y = 20.
           PERFORM PLOTPIXEL-PROCEDURE.
           COMPUTE x = 5.
           COMPUTE y = 5.
           COMPUTE linelength = 10.
           PERFORM DRAWVERTICALLINE.
           PERFORM DRAWHORIZONTALLINE.
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
           COMPUTE i = 1.
       PLOTPIXEL-PROCEDURE.
           INSPECT SCRENCHRT(x, y) REPLACING CHARACTERS BY '#'.
       REMOVEPIXEL-PROCEDURE.
           INSPECT SCRENCHRT(x, y) REPLACING CHARACTERS BY ' '.
       DRAWVERTICALLINE.
           COMPUTE i = x + linelength.
           PERFORM UNTIL x > i
               INSPECT SCRENCHRT(x, y) REPLACING CHARACTERS BY '#'
               ADD 1 to x GIVING x
           END-PERFORM
           COMPUTE i = 1.
       DRAWHORIZONTALLINE.
           COMPUTE i = y + linelength.
           PERFORM UNTIL y > i
               INSPECT SCRENCHRT(x, y) REPLACING CHARACTERS BY '#'
               ADD 1 to y GIVING y
           END-PERFORM
           COMPUTE i = 1.
       CLEARSCREEN.
           PERFORM UNTIL i > 25
               PERFORM UNTIL j > 80
                   INSPECT SCRENCHRT(i,j) REPLACING CHARACTERS BY '#'
                   ADD 1 TO j GIVING j
               END-PERFORM
               COMPUTE j = 1
               ADD 1 TO i GIVING i
           END-PERFORM.
           COMPUTE i = 1.
           COMPUTE x = 1.
           COMPUTE y = 1.
       END PROGRAM 8025test.
