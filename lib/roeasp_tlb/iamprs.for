      SUBROUTINE IAMPRS(IXEXT,IYEXT,PARAMS,SKY,THRLD,AREA,SKYMAG)
C+
C   IAMPRS
C
C     OUTPUTS IMAGE PARAMETERS FROM IAM TO TEMP.DAT FOR LISTING
C    called from IAMPR
C
C   Given         (arguments)
C   IXEXT,IYEXT    (I)   dimensions of PARAMS
C   PARAMS         (RA)  array with images found by TVANAL
C   SKY      (R)    adopted sky background
C   THRLD    (R)    adopted threshold
C   AREA     (I)    adopted area cut
C   SKYMAG   (I)    adopted sky background magnitude in one pixel
C      
C   Output  :  FORTRAN unit 4
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-
      IMPLICIT NONE

      INTEGER AREA
      INTEGER IXEXT,IYEXT
      REAL PARAMS(IXEXT,IYEXT)
      REAL SKY,THRLD,SKYMAG
      INTEGER J, I
      INTEGER OSTAT1, OSTAT2
      CHARACTER BUFFER*130

      OPEN(UNIT=4,NAME='TEMP.DAT',TYPE='NEW',
     &     FORM='FORMATTED')

      WRITE(4,*)' IAM PARAMETERS'
170   FORMAT(////21X,'UNWEIGHTED PARAMETERS',21X,
     :       'INTENSITY WEIGHTED PARAMETERS'//3X,'NUM',5X,
     :       'XCEN',6X,'YCEN',6X,'AU',6X,'BU',3X,'THETAU',5X,
     :       'XCEN',6X,'YCEN',6X,'AI',6X,'BI',3X,'THETAI',5X,
     :       'AREA',3X,'IMAX',5X,'MAG',4X,'ELLIPT'//)
      WRITE(4,*)' '
      WRITE(4,*)' SKY BACKGROUND, THRESHOLD, SKY MAGNITUDE
     : AND AREACUT ARE'
      WRITE(4,190) SKY,THRLD,SKYMAG,AREA
190   FORMAT (4X,F8.3,6X,F8.3,4X,F8.3,8X,I5)
      WRITE(4,170)

      DO J=1,IYEXT
        BUFFER = ' '
        WRITE(BUFFER, 180, IOSTAT=OSTAT1) (J,(PARAMS(I,J),I=1,11),
     :    PARAMS(13,J),PARAMS(12,J),PARAMS(18,J))
180     FORMAT(1X,I4,2(3X,F7.2,3X,F7.2,2X,F6.2,2X,F6.2,3X,F5.1),
     :    2X,F7.0,2X,F7.2,3X,F7.2,2X,F5.2)

        IF (OSTAT1 .EQ. 0) THEN
          WRITE(4, 200, IOSTAT=OSTAT2) BUFFER
200       FORMAT(1X, A130)
        ELSE
          WRITE(4, 210, IOSTAT=OSTAT2) (J,(PARAMS(I,J),I=1,11),
     :      PARAMS(13,J),PARAMS(12,J),PARAMS(18,J))
210       FORMAT(2X, '*** Image number ', I8, ' Contains wild values:' /
     :      2X, 1PE17.4, 1PE17.4, 1PE17.4, 1PE17.4, 1PE17.4,
     :      1PE17.4, 1PE17.4 /
     :      2X,  1PE17.4, 1PE17.4, 1PE17.4, 1PE17.4, 1PE17.4,
     :      1PE17.4, 1PE17.4 )
        END IF
      END DO

      CLOSE(UNIT=4,DISPOSE='PRINT/DELETE')

      END
