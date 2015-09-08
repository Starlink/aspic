      SUBROUTINE UPPCAS (LINE)
*     [JAC]  16JUL81
*     convert character variable to upper case
*+
*   UPPCAS - converts character variable to upper case
 
*   given (argument):
*     LINE   character   (any length)
 
*   returned (argument):
*     LINE   character   (same length)
 
*   JAC/UOE/16JUL81
*-
 
      CHARACTER LINE*(*)
      INTEGER I,VALUE,LENGTH
 
      LENGTH=LEN(LINE)
 
      DO I=1,LENGTH
         VALUE=ICHAR(LINE(I:I))
         IF ( VALUE.GE.97 .AND. VALUE.LE.122 ) THEN
            VALUE=VALUE-32
            LINE(I:I)=CHAR(VALUE)
         ENDIF
      ENDDO
 
      END
