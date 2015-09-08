      SUBROUTINE PRMPT2I(PREFIX,NAME,I1,I2,ISTATUS)
      CHARACTER*(*) PREFIX,NAME
      DIMENSION IVALUE(2)
10    CALL WRUSER(PREFIX,ISTAT)
      CALL RDKEYI(NAME,.FALSE.,2,IVALUE,I,ISTATUS)
      IF (ISTATUS .EQ. 1 .OR. I .NE. 2)THEN
        CALL WRUSER('******ERROR****** MUST INPUT 2 INTEGERS',ISTAT)
        CALL CNPAR(NAME,ISTAT)
        GO TO 10
      END IF
      I1=IVALUE(1)
      I2=IVALUE(2)
      END
C *******************************************************************
