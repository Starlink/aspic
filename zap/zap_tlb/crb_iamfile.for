	subroutine crb_iamfile
C+
C   IAMPR
C
C     OUTPUTS IMAGE PARAMETERS FROM IAM TO .LP
C*      user prompted for file of params
C	only a fixed number of params for each image are output
C
C   Given         (program parameters)
C   PARAMS    (RA)    array of image parameters
C
C   Given (descriptors to params)
C   SKY      (R)    adopted sky background
C   THRLD    (R)    adopted threshold
C   AREA     (I)    adopted area cut
C   SKYMAG   (R)    adopted sky background magnitude in one pixel
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

      INTEGER AREA
      REAL SKY,THRLD,SKYMAG
	character*72 title
      INTEGER NAXPAR(2),NPPAR,ISTATUS
      INTEGER JDUM
C*  chars for reading descriptors
      CHARACTER*5 CAREA
      CHARACTER*20 CSKY,CTHRLD,CSKYMAG

      ISTATUS=0
      CALL crb_inpicr('PARAMS','ENTER PARAMS FILE',2,NAXPAR,NPPAR,
	1 istatus,title)
C*  input descriptors and later change chars-->nos
      CALL RDDSCR('PARAMS','SKY',1,CSKY,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','THRLD',1,CTHRLD,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','AREA',1,CAREA,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','SKYMAG',1,CSKYMAG,JDUM,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        READ(CSKY,'(BN,F20.0)') SKY
        READ(CTHRLD,'(BN,F20.0)') THRLD
        READ(CAREA,'(BN,I5)') AREA
        READ(CSKYMAG,'(BN,F20.0)') SKYMAG
        CALL crb_iamprs(title,NAXPAR(1),NAXPAR(2),%VAL(NPPAR),
     :              SKY,THRLD,AREA,SKYMAG)
      ENDIF
      END
      SUBROUTINE crb_iamprs(title,IXEXT,IYEXT,PARAMS,SKY,THRLD,
	1  area,skymag)
C+
C   crb_iamprs
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

      INTEGER AREA
      INTEGER IXEXT,IYEXT
      REAL PARAMS(IXEXT,IYEXT)
      REAL SKY,THRLD,SKYMAG
	character*72 title
	character*30 filename
      INTEGER J

	print *,'Filename for output?'
	read(*,196) filename
196	format(a30)
      OPEN(UNIT=4,NAME=filename,TYPE='NEW',
     &     FORM='FORMATTED')

	write(4,666) title
666	format(' Name of analysed image: ',a72,/)
      WRITE(4,*)' IAM PARAMETERS'
170   FORMAT(////21X,'UNWEIGHTED PARAMETERS',21X,
     :       'INTENSITY WEIGHTED PARAMETERS'//3X,'NUM',5X,
     :       'XCEN',6X,'YCEN',6X,'AU',6X,'BU',3X,'THETAU',5X,
     :       'XCEN',6X,'YCEN',6X,'AI',6X,'BI',3X,'THETAI',5X,
     :       'AREA',3X,'IMAX',5X,'MAG',4X,'ELLIPT'//)
180   FORMAT(2X,I4,2(3X,F7.2,3X,F7.2,2X,F6.2,2X,F6.2,3X,F5.1),
     :       2X,F7.0,2X,F10.2,3X,F7.2,2X,F5.2)
      WRITE(4,*)' '
      WRITE(4,*)' SKY BACKGROUND, THRESHOLD, SKY MAGNITUDE
     : AND AREACUT ARE'
      WRITE(4,190) SKY,THRLD,SKYMAG,AREA
190   FORMAT (4X,F8.3,6X,F8.3,4X,F8.3,8X,I5)
      WRITE(4,170)

      DO J=1,IYEXT
        WRITE(4,180) (J,(PARAMS(I,J),I=1,11),
     :                PARAMS(13,J),PARAMS(12,J),PARAMS(18,J))
      ENDDO

      END
      SUBROUTINE crb_inpicr(NAME,PROMPT,NDIMS,NAXIS,NPTR,
	1   istat,ident1)
C+
C   INPICR
C
C   Return a pointer to a real array
C
C   ASPIC interface version
C
C   Given      (arguments)
C   NAME        image name
C   PROMPT      prompt to user if Starlink disk-file needed
C   NDIMS       dimensionality of required array
C   ISTAT       input status
C
C   Returned   (arguments)
C   NPTR        pointer to first element of array
C   NAXIS       dimensions of array
C   ISTAT       status return = MAX(input status,output status)
C
C   Subroutine calls :
C   WRUSER,RDIMAG    : STARLINK
C   BATCH            : ASPFACE
C
C   B.D.Kelly/ROE/21.9.1981
C-

      CHARACTER*(*) NAME,PROMPT
      CHARACTER*72 IDENT1
      INTEGER NDIMS,NPTR,ISTAT,IST1,IST2,JDIMS
      INTEGER NAXIS(NDIMS)
      LOGICAL BATFL

      CALL BATCH(BATFL)
      IF(.NOT.BATFL)  CALL WRUSER(PROMPT,IST1)
      CALL RDIMAG(NAME,204,NDIMS,NAXIS,JDIMS,NPTR,IST2)
      IF(IST2.EQ.0) THEN
        CALL RDDSCR(NAME,'TITLE',1,IDENT1,JDUM,IST1)
        IF(IST1.EQ.0) THEN
          CALL WRUSER(IDENT1,IST1)
        ELSE
          IDENT1=' '
        ENDIF
      ENDIF

      ISTAT=MAX(ISTAT,IST2)

      END
