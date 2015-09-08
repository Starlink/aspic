      SUBROUTINE PEEP(PIC,NX,NY,IX0,IY0,FACTOR)
*
*  PRINT 9 X 9 REGION OF ARRAY PIC, CENTRED ON IX0,IY0
*
 
      REAL PIC(NX,NY)
      INTEGER NX,NY,IX0,IY0
 
*  WORK FIELD AND REPORT LINE
      CHARACTER CWK*9,CLINE*72
 
*  X,Y COORDINATES FOR LEFT, RIGHT, TOP, BOTTOM
 
*  REPORT X,Y FOR TOP LEFT HAND CORNER
      IXL=IX0-4
      IYT=IY0+4
      WRITE (CLINE,'(''('',I4,'','',I4,'')'')') IXL,IYT
 1    FORMAT ('(',I4,',',I4,')',50X,'(',I4,',',I4,')')
      CALL WRUSER(' ',JSTAT)
      CALL WRUSER(CLINE,JSTAT)
      CALL WRUSER(' ',JSTAT)
 
*  MAIN REPORT
 
      DO J=1,9
         IY=IY0-J+6
         DO I=1,9
            IX=IX0+I-4
            IF (IX.LT.1.OR.
     &          IX.GT.NX.OR.
     &          IY.LT.1.OR.
     &          IY.GT.NY) THEN
               CWK=' ........ '
            ELSE
               V=NINT(PIC(IX,IY)*FACTOR)
               IF (V.GT.9999999.0) THEN
                  CWK=' +++++++ '
               ELSE IF (V.LT.-999999.0) THEN
                  CWK=' ------- '
               ELSE
                  WRITE (CWK,'(F9.0)') V
               END IF
            END IF
            NPTR=(I-1)*8+1
            CLINE(NPTR:NPTR+7)=CWK(1:8)
         END DO
         CALL WRUSER(CLINE,JSTAT)
      END DO
 
*  REPORT BOTTOM RIGHT HAND CORNER
      IXR=IX0+4
      IYB=IY0-4
      WRITE (CLINE,'(61X,''('',I4,'','',I4,'')'')') IXR,IYB
         CALL WRUSER(' ',JSTAT)
      CALL WRUSER(CLINE,JSTAT)
      CALL WRUSER(' ',JSTAT)
 
      END
