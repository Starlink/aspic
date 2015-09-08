      INTEGER FUNCTION IWAIT (DTSEC)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         FUNCTION IWAIT 
C
C
C         It requests a  delay  of  a  specified  number  of  seconds  from 
C         (nominally)  100*10E-9  up  to 5 minutes. It has the advantage of 
C         not using up processor time looping around. 
C
C         It returns a value of zero on successful completion.
C
C         DTSEC       Real  In    The number of seconds. Because it is real 
C                                 fractions of a second can be requested. 
C
C
C         C D Pike                 RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
                                    
                                                                                
                                                                                
                                                                                
*
*
*  REQUEST DELAY
*
*
*  GIVEN:
*
*     DTSEC      DELAY (REAL, SECONDS)
*
*
*  RETURNED:
*
*     IWAIT      0 = SUCCESSFUL
*
*
*  THE MINIMUM DELAY IS (NOMINALLY) 100NS;
*  THE MAXIMUM IS 5 MINUTES.
*
*
*

      REAL DTSEC

      INTEGER LIB$GETEF,SYS$SETIMR,SYS$WAITFR,LIB$FREE_EF

      INTEGER IDT(2)
      DATA IDT(2)/-1/


*  PRESET STATUS
      JSTAT=-1

*  ENCODE DELTA TIME
      IDT(1)=-NINT(MAX(1.0,1E7*MIN(300.0,DTSEC)))


*  ASK SYSTEM TO ALLOCATE EVENT FLAG
      JSTAT=LIB$GET_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000

*  ASK FOR EVENT FLAG TO BE SET AFTER DELAY
      JSTAT=SYS$SETIMR(%VAL(NEF),IDT,,)
      IF (.NOT.JSTAT) GO TO 9000

*  WAIT FOR EVENT FLAG
      JSTAT=SYS$WAITFR(%VAL(NEF))
      IF (.NOT.JSTAT) GO TO 9000

*  FREE THE EVENT FLAG
      JSTAT=LIB$FREE_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000

*  RETURN STATUS
      JSTAT=0

 9000 CONTINUE
      IWAIT=JSTAT

      END
