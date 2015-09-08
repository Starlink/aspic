      PROGRAM TYPETAB

C+
C    TYPETAB
C
C	part of general 2-d table package
C	types table on terminal
C
C    Given (program parameter)
C	TABLE (RA)	array of reals
C
C    D. Tudhope/ROE/April 1983
C-

      INTEGER NPTAB,NAXTAB(2),ISTATUS

      ISTATUS=0
      CALL INPICR('TABLE','ENTER NAME OF TABLE TO BE TYPED',
     :            2,NAXTAB,NPTAB,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
       CALL TYPETABS(NAXTAB(1),NAXTAB(2),%VAL(NPTAB))
      ENDIF
      END
