





      SUBROUTINE APER
*+
*   APER
*
*   Aperture photometry of images selected by cursor
*
*
*   Subroutines called :
*   ADDEL3, APERCOM, ARGCUR, BOXCUR, BOXSUM, MAG, SHAPEL, ZERO,
*   MULREP                                : E2DLIB
*   READC, READR, WRUSER                  : INTERFACE
*
*   B.D.Kelly/ROE/1981
*   C.Aspin/UOE/ FEB 1984        {Modified}
*   A C Davenhall./ROE/13.3.84   {Modified}
*-
      IMPLICIT NONE
*
      INTEGER WRUNIT
      PARAMETER (WRUNIT=15)
*
      CHARACTER*80 RBUF,FILENAME
      CHARACTER*72 OBUF,LBUF
      CHARACTER*4  PROMPT
      CHARACTER*10 IDENT,IDDEF,SHAPE
*
      INTEGER NAXIS(2),NPTR,IST,WRSTAT,LSTAT,COUNT,
     :        IFAIL,JX,JY,IXPOS,IYPOS,IBSIZE,
     :        IXCUR,IYCUR
*
      LOGICAL OPENFIL
*
      REAL SUMSKY,SUM,ARESTAR,AXMAJ,AXMIN,ECCEN,THETA,AREA,
     :     XCOORD,YCOORD,AJ,AN,ARESKY,RTEMP,TEMSKY,RMAG,
     :     ZERDEF,SUMSTAR,ZEROMAG
*
*
      WRSTAT=0
      IST=0
*
      COUNT=0
      LBUF='-----------------------------------------------------'
*
      CALL SRINIT(0,.FALSE.,IFAIL)
 
      CALL INPICR('INPIC1','Give input image;',2,NAXIS,NPTR,IST)
*
*    Set defaults.
*
      SHAPE='ELLIPSE'
      SUMSTAR=1.0E0
      SUMSKY=0.0E0
      ARESTAR=1.0E0
      ARESKY=1.0E0
      ZEROMAG=0.0E0
      JX=NAXIS(1)/2
      JY=NAXIS(2)/2
      AXMAJ=1.0E1
      ECCEN=0.0E0
      THETA=0.0E0
      IXPOS=0
      IYPOS=0
*
*    Give instructions.
*
      CALL APERCOM
*
*    Interactive node.
*
      OPENFIL=.FALSE.
      RBUF=' '
*
      DO WHILE(RBUF.NE.'E')
        CALL READC('APERCOM','Give command;',' ','A','ZZZZZ',RBUF,IST)

        IF(RBUF.EQ.'SIZE') THEN
          RTEMP=AXMAJ
          CALL READR('SIZE','Give new aperture size',RTEMP,
     :               1.0,128.0,AXMAJ,IST)

        ELSE IF(RBUF.EQ.'SHAPE') THEN
          CALL MULREP('State square or ellipse','ELLIPSE,SQUARE$',
     :                SHAPE,IST)

        ELSE IF(RBUF.EQ.'POSN')THEN
          CALL READI('XPOS','Give X position of cursor centre;',
     :                0,0,512,IXPOS,IST)
          CALL READI('YPOS','Give y position of cursor centre;',
     :                0,0,512,IYPOS,IST)
          JX=IXPOS
          JY=IYPOS

        ELSE IF (RBUF.EQ.'GIVE') THEN
          CALL WRUSER (LBUF,IST)
          CALL WRUSER (' ',IST)
          WRITE(OBUF,'(A,A)')'Aperture is ',SHAPE
          CALL WRUSER (OBUF,IST)
          CALL WRUSER (' ',IST)
          WRITE(OBUF,'(A,I4)') 'X Centre of aperture       = ',JX
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,I4)') 'Y Centre of aperture       = ',JY
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,0PF10.2)')
     :         'Major axis of aperture     = ',AXMAJ
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,0PF10.2)')
     :         'Eccentricity of aperture   = ',ECCEN
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,0PF10.2)')
     :         'Orientation of aperture    = ',THETA
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,1PE13.4)')
     :         'Current star aperture sum  = ',SUMSTAR
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,1PE13.4)')
     :         'Current sky aperture sum   = ',SUMSKY
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,1PE13.4)')
     :         'Current star aperture area = ',ARESTAR
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,1PE13.4)')
     :         'Current sky aperture area  = ',ARESKY
          CALL WRUSER (OBUF,IST)
          WRITE(OBUF,'(A,1PE13.4)')
     :         'Current mag zero point     = ',ZEROMAG
          CALL WRUSER (OBUF,IST)
          CALL WRUSER ('  ',IST)
          CALL WRUSER (LBUF,IST)

        ELSE IF((RBUF.EQ.'STAR').OR.(RBUF.EQ.'SKY')) THEN
          IF(SHAPE.EQ.'SQUARE') THEN
	    IBSIZE=IFIX(AXMAJ+1.0E0)
            CALL BOXCUR(JX,JY,IBSIZE)
            AXMAJ=REAL(IBSIZE)
            IXCUR=1+JX
            IYCUR=1+JY
            CALL BOXSUM(IXCUR,IYCUR,IBSIZE,NAXIS(1),NAXIS(2),
     :                  %VAL(NPTR),SUM)
            AREA=FLOAT(IBSIZE)**2
          ELSE
            CALL SHAPEL(AXMAJ,ECCEN,THETA,JX,JY)
            AXMIN=AXMAJ*SQRT(1.0-ECCEN**2)
            XCOORD=REAL(JX)
            YCOORD=REAL(JY)
            AJ=AXMAJ/2.0
            AN=AXMIN/2.0
            CALL ADDEL3(XCOORD,YCOORD,AJ,AN,THETA,%VAL(NPTR),
     :                  NAXIS(1),NAXIS(2),SUM)
            AREA=3.14159*AJ*AN
          ENDIF
          IF(RBUF.EQ.'STAR') THEN
            SUMSTAR=SUM
            ARESTAR=AREA
            WRITE(OBUF,'(''Star measure is: '',1PE16.4)') SUMSTAR
            CALL WRUSER(OBUF,IST)
          ELSE
            SUMSKY=SUM
            ARESKY=AREA
            WRITE(OBUF,'(''Sky measure is: '',1PE16.4)') SUMSKY
            CALL WRUSER(OBUF,IST)
          ENDIF

        ELSE IF(RBUF.EQ.'MAG') THEN
          TEMSKY=SUMSKY*ARESTAR/ARESKY
          CALL MAG(SUMSTAR,TEMSKY,ZEROMAG,RMAG,LSTAT)
          IF (LSTAT.EQ.0) THEN
            WRITE(OBUF,2004) RMAG
 2004       FORMAT(1X,'Magnitude = ',0PF10.3)
            CALL WRUSER (OBUF,IST)
          ELSE
            CALL WRUSER ('***ERROR Star measure smaller than '/
     :                  /'sky; magnitude undefined.',IST)
          END IF

        ELSE IF(RBUF.EQ.'ZERO') THEN
          ZERDEF=ZEROMAG
          CALL READR('ZEROPT','Give zero-point;',ZERDEF,
     :               -1.0E19,1.0E19,ZEROMAG,IST)

        ELSE IF (RBUF.EQ.'WRITE') THEN
*
*        Check if the output file is already written.
*
          IF (.NOT.OPENFIL) THEN
            CALL READC ('FILENAME','Give name of output file;',
     :                  'APERASP.LIS',' ','~',FILENAME,IST)
            OPEN(UNIT=WRUNIT,FILE=FILENAME,STATUS='NEW',
     :           IOSTAT=WRSTAT)
            IF (WRSTAT.EQ.0) THEN
              OPENFIL=.TRUE.
              WRITE(WRUNIT,2000)
 2000         FORMAT(2X,'Count',1X,'Identifier',4X,'Position',
     :         11X,'Star',17X,'Sky',13X,'Zero',6X,'Magnitude'/
     :         22X,'X',6X,'Y',4X,'Intensity',3X,'Area',5X,
     :         'Intensity',3X,'Area',7X,'Point'/)
            ELSE
              CALL WRUSER (
     :         '***ERROR Unable to open output file.',IST)
              WRITE(OBUF,2001) WRSTAT
 2001         FORMAT(10X,'Status raised = ',I4)
              CALL WRUSER (OBUF,IST)
            END IF
          END IF
*
*        Proceed to write a record if the ouput file is open.
*
          IF (OPENFIL) THEN
            COUNT=COUNT+1
            WRITE(IDDEF,2002) COUNT
 2002       FORMAT('Image ',I4)
            CALL READC ('IDENT','Give image identifier;',
     :                   IDDEF,' ','~',IDENT,IST)
            CALL MAG (SUMSTAR,SUMSKY,ZEROMAG,RMAG,LSTAT)
            IF (LSTAT.NE.0) 
     :        CALL WRUSER ('***ERROR Star measure smaller than '/
     :         /'sky; magnitude undefined.',IST)
            WRITE(WRUNIT,2003) COUNT,IDENT,JX,JY,SUMSTAR,ARESTAR,
     :                         SUMSKY,ARESKY,ZEROMAG,RMAG
 2003       FORMAT(I6,2X,A10,I6,1X,I6,2X,1PE10.3,1X,1PE8.1,
     :        2X,1PE10.3,1X,1PE8.1,2X,1PE10.3,2X,1PE10.3)
          END IF

        ELSE IF(RBUF.EQ.'E') THEN
          CONTINUE

        ELSE
          CALL APERCOM
        ENDIF
      ENDDO
*
*    Finished.
*
      IF (OPENFIL) CLOSE(UNIT=WRUNIT)
*
      CALL CLEARIM('INPIC1')
 
      END
