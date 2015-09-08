      SUBROUTINE ILIST(A,NX,NY,FNAME,FACTOR)
 
*
*  LIST AN ARRAY
*
*
*  GIVEN:
*     A        ARRAY TO BE LISTED
*     NX,NY    DIMENSIONS OF ARRAY A
*     FNAME    FILENAME FOR REPORT
*     FACTOR   SCALING FACTOR FOR PIXEL VALUES
*
*  FOR EACH PIXEL A 3-DIGIT REPRESENTATION OF FACTOR TIMES ITS
*  VALUE IS LISTED.
*
 
      INTEGER NX,NY
      REAL A(NX,NY),FACTOR
      CHARACTER*(*) FNAME
 
 
      CHARACTER LINE*128
 
 
*  OPEN REPORT
      OPEN (UNIT=1,FILE=FNAME,STATUS='NEW')
 
*  INITIALISE STRIP COUNT
      NSTRIP=0
 
*  LIST IN VERTICAL STRIPS EACH 40 PIXELS WIDE
      DO IXS=1,NX,40
 
*     INCREMENT & REPORT STRIP COUNT
         NSTRIP=NSTRIP+1
*   TO SAVE PAPER WE ONLY THROW A NEW PAGE FOR EACH STRIP
*   IF THERE ARE MORE THAN 8 STRIPS
*   REALLY DONE TO HANDLE 1-D CASE!
 
         IF (NY.GT.8) THEN
            WRITE (1,'(''1STRIP'',I3,30X,
     :             ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR
         ELSE
            WRITE(1,'(''0STRIP'',I3,30X,
     :              ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR
         END IF
 
*     REPORT FIRST & LAST X
         IXE=MIN(IXS+39,NX)
         LINE=' '
         WRITE (LINE(:11),'(I11)') IXS-1
         K=4+3*(IXE-IXS)
         WRITE (LINE(K:K+7),'(I8)') IXE-1
         WRITE (1,'(A/)') LINE
 
*     LINE BY LINE
         DO IY=NY,1,-1
 
*        RESET LINE BUFFER
            LINE=' '
 
*        FORMAT Y-COORDINATE
            WRITE (LINE(:5),'(I5)') IY-1
 
*        PIXEL BY PIXEL
            DO IX=IXS,IXE
 
*           BUFFER POINTER
               K=9+3*(IX-IXS)
 
*           FORMAT PIXEL
               Z=FACTOR*A(IX,IY)
               IF (ABS(Z).GT.1000.0) Z=SIGN(Z,1000.0)
               IZ=NINT(Z)
               IF (IZ.LT.-99) THEN
                  LINE(K:K+2)='---'
               ELSE IF (IZ.GT.999) THEN
                  LINE(K:K+2)='+++'
               ELSE
                  WRITE (LINE(K:K+2),'(I3)') IZ
               END IF
 
*           NEXT PIXEL
            END DO
 
*        OUTPUT THE LINE
            WRITE (1,'(A)') LINE
 
*        NEXT LINE
         END DO
 
*     NEXT STRIP
      END DO
 
*  CLOSE REPORT
      WRITE (1,'(''1'')')
      CLOSE (UNIT=1)
 
*  EXIT
 
      END
