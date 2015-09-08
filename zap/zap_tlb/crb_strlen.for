*************************************
*  STRLEN: Calculates string length
*************************************
      SUBROUTINE CRB_STRLEN(STRING,LEN)

      CHARACTER*70 STRING

      LEN=70

      DO I=70,1,-1
         IF(STRING(I:I).EQ.' ') THEN
            LEN=LEN-1
         ELSE
            GO TO 1
         ENDIF
      ENDDO
1     CONTINUE
      END
