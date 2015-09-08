      SUBROUTINE WAIT (DTSEC,STAT)
C+
C     WAIT.
C
C     Subroutine to request a delay time.
C     The minimum delay is (nominally) 100ns.
C     The maximum is 5 minutes.
C
C  Given;
C   DTSEC   (R)  Delay time required (seconds).
C
C  Returned;
C   STAT    (I)  Return status.
C                = 0 - Successful return.
C                = 1 - Unsuccessful return.
C
C  Subroutines called;
C   System functions; SYS$SETIMR, SYS$WAITFR,
C                     LIB$FREE_EF.
C
C  Original;  C. D. Pike./RGO/                May 1982.
C  Modified;  A C Davenhall./ROE/             29/6/82.
C-
      REAL DTSEC
      INTEGER STAT
C
      INTEGER JSTAT,IDT2
      INTEGER LIB$GETEF,SYS$SETIMR,SYS$WAITFR,LIB$FREE_EF
      INTEGER IDT(2)
      PARAMETER (IDT2=-1)
      IDT(2)=IDT2
C
C  Preset status.
C
      JSTAT=-1
C
C  Encode delta time.
C
      IDT(1)=-NINT(MAX(1.0,1E7*MIN(300.0,DTSEC)))
C
C  Ask the system to allocate an event flag.
C
      JSTAT=LIB$GET_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000
C
C  Ask for the event flag to be set after delay.
C
      JSTAT=SYS$SETIMR(%VAL(NEF),IDT,,)
      IF (.NOT.JSTAT) GO TO 9000
C
C  Wait for the event flag.
C
      JSTAT=SYS$WAITFR(%VAL(NEF))
      IF (.NOT.JSTAT) GO TO 9000
C
C  Free the event flag.
C
      JSTAT=LIB$FREE_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000
C
C  Set the return status following succesful
C  completion.
C
      JSTAT=0
C
 9000 CONTINUE
      STAT=JSTAT
      END
