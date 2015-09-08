      SUBROUTINE WRHELPT
C+
C    WRHELPT
C
C	called from EDITTAB
C	part of general table editing package
C	types help info for edittab
C
C
C    D. Tudhope/ROE/April 1983
C-

      CALL WRUSER(' ',I)
      CALL WRUSER('Program to edit a table of real numbers.',I)
      CALL WRUSER('No. of rows can be changed but not no. of columns',I)
      CALL WRUSER('Commands may prompt for row number etc.',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('R  n   REPLACE row n by typing in new row',I)
      CALL WRUSER('C  r,c CHANGE individual table value TAB(R,C)',I)
      CALL WRUSER('D  n   DELETE row n',I)
      CALL WRUSER('I  n   INSERT row n by typing in new row',I)
      CALL WRUSER('L  s,f LIST   rows s to f on terminal',I)
      CALL WRUSER('P      PRINT  whole table on terminal',I)
      CALL WRUSER('H      HELP   show this help info on terminal',I)
      CALL WRUSER('Q      QUIT   the program',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('The rows are renumbered immediately after D,I',I)
      CALL WRUSER('as a row will have been taken away/added.',I)
      CALL WRUSER('Do not type row number when actually entering a row'
     :            ,I)
      CALL WRUSER('See ROE LUN 17.1 for more information.',I)
      CALL WRUSER(' ',I)
      END
