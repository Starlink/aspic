       CHARACTER FUNCTION UPCASE(WORD)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         FUNCTION UPCASE 
C
C
C         It converts any lower case characters in a character string  into 
C         upper  case  equivalents. This may be of value (eg) in converting 
C         "yes" into "YES". 
C
C         The result is returned as the value of the function. 
C
C         WORD        Char  I/O   This is the charater string which  is  to 
C                                 be converted to upper case. 
C
C
C         L L Bell                 RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
                                      
                                                                                
                                                                                
                                                                                
*  THIS FUNCTION CONVERTS ANY LOWER CASE LETTERS IN A
*  WORD TO UPPER CASE

*  AUTHOR L.L.BELL
*  DATE   29 JANUARY 1982

       CHARACTER *(*) WORD
       CHARACTER*1    LETTER
       INTEGER        LAST,VALUE,I


       LAST=LEN(WORD)

*  EXAMINE EACH CHARACTER WITHIN THE WORD


       DO I=1,LAST

          LETTER=WORD(I:I)
          IF ((LGE(LETTER,'a')).AND.(LLE(LETTER,'z'))) THEN

*  CONVERT FROM LOWER TO UPPER CASE
*  THE DIFFERENCE BETWEEN THE UPPER CASE CHARACTER AND ITS EQUIVALENT
*  LOWER CASE CHARACTER IS DECIMAL 32 IN ASCII REPRESENTATION.

             VALUE=ICHAR(LETTER)
             VALUE=VALUE-32
             WORD(I:I)=CHAR(VALUE)

          ENDIF

       ENDDO

       UPCASE(1:LAST)=WORD(1:LAST)


       END
