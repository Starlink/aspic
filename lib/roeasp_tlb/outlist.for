C
C
C    
      SUBROUTINE OUTLIST (NAME,PROMPT,NITEM,LSTLEN,POINTER,
     :                    STATUS)
C+
C     OUTLIST.
C
C     Returns a pointer to a new output frame with the EDRS
C     XYlist format.
C
C  Given;
C   NAME     (C)  Name of the output file.
C   PROMPT   (C)  prompt string.
C   NITEM    (I)  Total number of words in each record.
C   LSTLEN   (I)  No. of records in the output file.
C
C  Returned;
C   POINTER  (I)  Pointer to the output file.
C   STATUS   (I)  Return status, = 0 for success.
C
C  Subroutines called;
C   Interfaces:-        BATCH, OUTPUT, READC.
C   Interim envirn.:-   WRDATA, WRDSCR.
C
C  A C Davenhall./ROE/                                         5/10/83.
C-
      IMPLICIT NONE
C
      CHARACTER*(*) NAME,PROMPT
      INTEGER NITEM,LSTLEN,POINTER,STATUS
C
      LOGICAL BATFL
      INTEGER IOSTAT,INTSTAT,COPSTAT1,COPSTAT2
      INTEGER SIZE
      CHARACTER BUFFER*30,TITLE*72
C
      CHARACTER FRTYP*6
      PARAMETER (FRTYP='XYLIST')
C
C
      IF (STATUS.EQ.0) THEN
        IOSTAT=0
C
        CALL BATCH (BATFL)
        IF (.NOT.BATFL) CALL OUTPUT (PROMPT,IOSTAT)
C
        SIZE=NITEM*LSTLEN
        INTSTAT=0
        CALL WRDATA (NAME,104,FRTYP,SIZE,POINTER,INTSTAT)
        IF (INTSTAT.EQ.0) THEN
C
          WRITE(BUFFER,'(I30)',IOSTAT=COPSTAT1) NITEM
          CALL WRDSCR (NAME,'NITEM',BUFFER,1,INTSTAT)
          WRITE(BUFFER,'(I30)',IOSTAT=COPSTAT2) LSTLEN
          CALL WRDSCR (NAME,'LSTLEN',BUFFER,1,INTSTAT)
C
          CALL READC ('REPLY','Give image title;',
     :                'IAM dataset.',' ','{',TITLE,IOSTAT)
          CALL WRDSCR (NAME,'TITLE',TITLE,1,INTSTAT)
        END IF
C
        STATUS=MAX(STATUS,INTSTAT,COPSTAT1,COPSTAT2)
C
      END IF
C
      END
