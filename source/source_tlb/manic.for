
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   MANIC *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               MANIC
C
C
C          FUNCTION:-
C               Conversion of STARLINK images from one format to another
C
C
C          USE:-
C               To write all or part of a 1, 2 or  3  dimensional  STARLINK
C               image  to an output image of 1, 2 or 3 dimensions.  Windows
C               may be set in any of the dimensions  of  the  input  image.
C               All  or  part of the input image may be projected on to any
C               of the rectangular planes or axes of the input before being
C               written to an  output  image; or a 1 or 2 dimensional image
C               may be grown to more dimensions to fill  an  output  image.
C               Many  output  images, each  of  a  different  configuration
C               if required, may be extracted from  a  single  input  image
C               with one call to the routine.
C
C
C
C         USER PARAMETERS:-
C
C     Any  character  strings  may  be  entered  in  upper  or  lower  case
C
C         INPUT                               Name of the STARLINK file (ie
C                                             .BDF) to be read as input
C
C         OUTPUT                              Name of the STARLINK file  to
C                                             which  the output image is to
C                                             be written
C
C         OUTDIMS                             The number of dimensions  for
C                                             the output image
C
C         XLIMITS         Whole of input      The X  window  of  the  input
C                                             image  to  be used in forming
C                                             the output image
C
C         YLIMITS         Whole of input      The Y  window  of  the  input
C                                             image  to  be used in forming
C                                             the output image
C
C         ZLIMITS         Whole of input      The Z  window  of  the  input
C                                             image  to  be used in forming
C                                             the output image
C
C         PLANE                               The rectangular plane  of the
C                                             input  image  to be extracted
C                                             or projected on to
C
C         LINE                                The rectangular axis  of  the
C                                             input  image  to be extracted
C                                             or projected on to
C
C         XSIZE                               The X size of an output image
C                                             grown   from  an  input  with
C                                             fewer dimensions
C
C         YSIZE                               The Y size of an output image
C                                             grown   from  an  input  with
C                                             fewer dimensions
C
C         ZSIZE                               The Z size of an output image
C                                             grown   from  an  input  with
C                                             fewer dimensions
C
C         LOOP                                Allows the user to loop  back
C                                             through    the   program   to
C                                             extract or  grow further out-
C                                             put  images   from  the  same
C                                             input  image.   Answer Y or N
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         QUIET           FALSE               Controls the issuing of  user
C                                             prompts   which   appear   by
C                                             default
C
C
C
C         C D Pike                 RGO                            15-AUG-81
C         Amended by Roger Wood    RGO                            30-MAR-83
C
C
C--------------------------------------------------------------------------




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PROGRAM MANIC
C
C
C
C
C CALLS  STARLINK:
C                 CNPAR,FRDATA,RDKEYC,RDKEYI,RDKEYL
C                 RDIMAG,WRIMAG,WRUSER
C
C
C NOTES
C        Uses the VAX %VAL facility
C        Uses the Run-Time Library procedure STR$UPCASE
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C Declare variables and arrays
C
      INTEGER PIN,POUT,INDIM,OUTDIM,STATUS
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      INTEGER XY(2),YZ(2),ZX(2)
      CHARACTER TEXT*72,PLANE*2,LINE*1,LOOP*1
      LOGICAL NQ,QUIET
C
C Include STARLINK error and format statements
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C Pick up QUIET parameter to control prompting
C
      CALL RDKEYL('QUIET',.TRUE.,1,QUIET,I,STATUS)
      IF(STATUS.GT.ERR_PARNUL)  GO TO 9999
      NQ = .NOT.QUIET
C
C Pick up INPUT image
C
      CALL RDIMAG('INPUT',FMT_R,3,AXIN,INDIM,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)   THEN
         CALL WRUSER('CANT FIND THAT IMAGE',STATUS)
         GO TO 9999
      ENDIF
C
C Jump in here for repeats
C
  100 CONTINUE
C
C Inform user of the number of input dimensions and their sizes
C
      WRITE(TEXT,111)  INDIM
  111 FORMAT(' NO OF DIMENSIONS IN INPUT IMAGE = ',I4)
      IF(NQ) CALL WRUSER(TEXT,STATUS)
      WRITE(TEXT,112) (AXIN(I),I=1,INDIM)
  112 FORMAT(' SIZE OF INPUT DIMENSIONS ..... ',3I5)
      IF(NQ) CALL WRUSER(TEXT,STATUS)
C
C Enquire number of dimensions for output image
C
  113 IF(NQ) CALL WRUSER('GIVE NO OF OUTPUT DIMENSIONS',STATUS)
      CALL RDKEYI('OUTDIMS',.FALSE.,1,OUTDIM,I,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Verify number of output dimensions
C
      IF(OUTDIM.LT.1.OR.OUTDIM.GT.3)  THEN
         CALL WRUSER('OUTPUT DIMENSION ERROR',STATUS)
	   CALL WRUSER('MUST BE 1,2 OR 3 - TRY AGAIN',STATUS)
	   CALL CNPAR('OUTDIMS',STATUS)
	   GO TO 113
      ENDIF
C
C
C
C Case of 3-D to 3-D transfer with windowing in all dimensions
C         **********
C
      IF(INDIM.EQ.3.AND.OUTDIM.EQ.3)  THEN
C
C Enquire limits in all 3 dimensions
C
         CALL XLIMITS(XLIMS,AXIN(1))
         CALL YLIMITS(YLIMS,AXIN(2))
         CALL ZLIMITS(ZLIMS,AXIN(3))
C
C Set output dimensions
C
         AXOUT(1) = XLIMS(2)-XLIMS(1)+1
         AXOUT(2) = YLIMS(2)-YLIMS(1)+1
         AXOUT(3) = ZLIMS(2)-ZLIMS(1)+1
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,3,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_3TO3(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,ZLIMS)
      ENDIF
C
C
C
C Case of 3-D to 2-D image
C         **********
C
      IF(INDIM.EQ.3.AND.OUTDIM.EQ.2)  THEN
C
C Enquire PLANE for extraction or projection
C
221      IF(NQ) CALL WRUSER('WHICH PLANE OF THE BRICK DO YOU ',STATUS)
         IF(NQ) CALL WRUSER('WISH TO EXTRACT OR PROJECT ON TO?',STATUS)
         IF(NQ) CALL WRUSER('SPECIFY: (XY,YZ,ZX)',STATUS)
         CALL RDKEYC('PLANE',.FALSE.,1,PLANE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(PLANE,PLANE)
C
C Check it is one of them
C
         IF(PLANE.EQ.'YX')PLANE='XY'
         IF(PLANE.EQ.'ZY')PLANE='YZ'
         IF(PLANE.EQ.'XZ')PLANE='ZX'
         IF(PLANE.NE.'XY'.AND.PLANE.NE.'YZ'.AND.PLANE.NE.'ZX')  THEN
            CALL WRUSER(' NOT AN ALLOWED RESPONSE',STATUS)
            CALL CNPAR('PLANE',STATUS)
            GO TO 221
         ENDIF
C
C Enquire limits in all 3 dimensions
C
      CALL XLIMITS(XLIMS,AXIN(1))
      CALL YLIMITS(YLIMS,AXIN(2))
      CALL ZLIMITS(ZLIMS,AXIN(3))
C
C Set output dimensions for each case
C
         IF(PLANE.EQ.'XY')  THEN
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
            AXOUT(2) = YLIMS(2)-YLIMS(1)+1
         ENDIF
         IF(PLANE.EQ.'YZ')  THEN
            AXOUT(1) = YLIMS(2)-YLIMS(1)+1
            AXOUT(2) = ZLIMS(2)-ZLIMS(1)+1
         ENDIF
         IF(PLANE.EQ.'ZX')  THEN
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
            AXOUT(2) = ZLIMS(2)-ZLIMS(1)+1
         ENDIF
C
C Set redundant output dimension
C
	      AXOUT(3) = 0
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,2,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_3TO2(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS
     +                                                ,ZLIMS,PLANE)
      ENDIF
C
C
C
C Case of 3-D to 1-D image
C         **********
C
      IF(INDIM.EQ.3.AND.OUTDIM.EQ.1)  THEN
C
C Enquire axis for extraction or projection
C
  555    IF(NQ) CALL WRUSER('WHICH AXIS OF THE BRICK DO YOU WISH TO',
     +                                                       STATUS)
         IF(NQ) CALL WRUSER('EXTRACT OR PROJECT ON TO? (X,Y,Z)',
     +                                                       STATUS)
         CALL RDKEYC('LINE',.FALSE.,1,LINE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(LINE,LINE)
C
C Check that it is one of them
C
         IF(LINE.NE.'X'.AND.LINE.NE.'Y'.AND.LINE.NE.'Z')  THEN
             CALL WRUSER('MUST BE X,Y OR Z',STATUS)
             CALL CNPAR('LINE',STATUS)
             GO TO 555
         ENDIF
C
C Enquire limits in all 3 dimensions
C
         CALL XLIMITS(XLIMS,AXIN(1))
         CALL YLIMITS(YLIMS,AXIN(2))
         CALL ZLIMITS(ZLIMS,AXIN(3))
C
C Set output dimensions for each case
C
         IF(LINE.EQ.'X')  THEN
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
         ENDIF
         IF(LINE.EQ.'Y')  THEN
            AXOUT(1) = YLIMS(2)-YLIMS(1)+1
         ENDIF
         IF(LINE.EQ.'Z')  THEN
            AXOUT(1) = ZLIMS(2)-ZLIMS(1)+1
         ENDIF
C
C Set redundant output dimensions
C
	  AXOUT(2) = 0
	  AXOUT(3) = 0
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,1,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_3TO1(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,ZLIMS,
     +                                                           LINE)
      ENDIF
C
C
C
C Case of 2-D to 3-D image
C         **********
C
      IF(INDIM.EQ.2.AND.OUTDIM.EQ.3)  THEN
C
C Enquire how input image is to be grown
C
  999    IF(NQ) CALL WRUSER('WHICH PLANE OF 3-D IMAGE IS INPUT',STATUS)
         IF(NQ) CALL WRUSER('TO BECOME? (XY,YZ,ZX)',STATUS)
         CALL RDKEYC('PLANE',.FALSE.,1,PLANE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(PLANE,PLANE)
C
C Check it is one of them
C
         IF(PLANE.EQ.'YX')PLANE='XY'
         IF(PLANE.EQ.'ZY')PLANE='YZ'
         IF(PLANE.EQ.'XZ')PLANE='ZX'
         IF(PLANE.NE.'XY'.AND.PLANE.NE.'YZ'.AND.PLANE.NE.'ZX')  THEN
             CALL WRUSER('NOT AN ALLOWED INPUT',STATUS)
             CALL CNPAR('PLANE',STATUS)
             GO TO 999
         ENDIF
C
C Enquire limits for each case
C
         IF(PLANE.EQ.'XY')  THEN
            CALL XLIMITS(XLIMS,AXIN(1))
            CALL YLIMITS(YLIMS,AXIN(2))
            IF(NQ) CALL WRUSER('GIVE SIZE OF Z OUTPUT DIMENSION',STATUS)
            CALL RDKEYI('ZSIZE',.FALSE.,1,NZ,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            ZLIMS(1) = 0
            ZLIMS(2) = NZ-1
	      LINE = 'Z'
         ENDIF
         IF(PLANE.EQ.'ZX')  THEN
            CALL XLIMITS(XLIMS,AXIN(1))
            CALL ZLIMITS(ZLIMS,AXIN(2))
            IF(NQ) CALL WRUSER('GIVE SIZE OF Y OUTPUT DIMENSION',STATUS)
            CALL RDKEYI('YSIZE',.FALSE.,1,NY,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            YLIMS(1) = 0
            YLIMS(2) = NY-1
	      LINE = 'Y'
         ENDIF
         IF(PLANE.EQ.'YZ')  THEN
            CALL YLIMITS(YLIMS,AXIN(1))
            CALL ZLIMITS(ZLIMS,AXIN(2))
            IF(NQ) CALL WRUSER('GIVE SIZE OF X OUTPUT DIMENSION',STATUS)
            CALL RDKEYI('XSIZE',.FALSE.,1,NX,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            XLIMS(1) = 0
            XLIMS(2) = NX-1
	      LINE = 'X'
         ENDIF
C
C Set output dimensions
C
	      AXOUT(1) = XLIMS(2) - XLIMS(1) + 1
	      AXOUT(2) = YLIMS(2) - YLIMS(1) + 1
	      AXOUT(3) = ZLIMS(2) - ZLIMS(1) + 1
C
C Prepare output image and write data to it
C
            CALL WRIMAG('OUTPUT',FMT_R,AXOUT,3,POUT,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            CALL DO_2TO3(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,
     +                                                  ZLIMS,LINE)
      ENDIF
C
C
C
C Case of 2-D to 2-D image with limits in both dimensions
C         **********
C
      IF(INDIM.EQ.2.AND.OUTDIM.EQ.2)  THEN
C
C Enquire limits in both dimensions
C
         CALL XLIMITS(XLIMS,AXIN(1))
         CALL YLIMITS(YLIMS,AXIN(2))
C
C Set output dimensions
C
         AXOUT(1) = XLIMS(2)-XLIMS(1)+1
         AXOUT(2) = YLIMS(2)-YLIMS(1)+1
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,2,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_2TO2(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS)
      ENDIF
C
C
C
C Case of 2-D to 1-D image
C         **********
C
      IF(INDIM.EQ.2.AND.OUTDIM.EQ.1)  THEN
C
C Enquire axis for extraction or projection
C
 1111    IF(NQ) CALL WRUSER('WHICH AXIS OF THE FRAME DO YOU WISH',
     +                                                    STATUS)
         IF(NQ) CALL WRUSER('TO EXTRACT OR PROJECT ON TO? (X,Y)',
     +                                                    STATUS)
         CALL RDKEYC('LINE',.FALSE.,1,LINE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(LINE,LINE)
C
C Check it is one of them
C
         IF(LINE.NE.'X'.AND.LINE.NE.'Y')  THEN
              CALL WRUSER('INPUT MUST BE X OR Y',STATUS)
             CALL CNPAR('LINE',STATUS)
             GO TO 1111
         ENDIF
C
C Enquire limits for each case
C
         IF(LINE.EQ.'Y')   THEN
            CALL XLIMITS(XLIMS,AXIN(1))
            CALL YLIMITS(YLIMS,AXIN(2))
            AXOUT(1) = YLIMS(2)-YLIMS(1)+1
         ENDIF
         IF(LINE.EQ.'X')   THEN
            CALL YLIMITS(YLIMS,AXIN(2))
            CALL XLIMITS(XLIMS,AXIN(1))
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
         ENDIF
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,1,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_2TO1(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,LINE)
      ENDIF
C
C
C
C Case of 1-D to 3-D image
C         **********
C
      IF(INDIM.EQ.1.AND.OUTDIM.EQ.3)  THEN
C
C Enquire how image is to be grown
C
 1112    IF(NQ) CALL WRUSER('WHICH AXIS OF 3-D IMAGE IS INPUT'
     +                       ,STATUS)
         IF(NQ) CALL WRUSER('IMAGE TO BECOME? (X,Y,Z)',STATUS)
         CALL RDKEYC('LINE',.FALSE.,1,LINE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(LINE,LINE)
C
C Check that it is one of them
C
         IF(LINE.NE.'X'.AND.LINE.NE.'Y'.AND.LINE.NE.'Z')  THEN
             CALL WRUSER('MUST BE X,Y OR Z',STATUS)
             CALL CNPAR('LINE',STATUS)
             GO TO 1112
         ENDIF
C
C Enquire limits and output dimensions for each case
C
         IF(LINE.EQ.'X')  THEN
            CALL XLIMITS(XLIMS,AXIN(1))
            IF(NQ) CALL WRUSER('GIVE Y OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('YSIZE',.FALSE.,1,NY,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            IF(NQ) CALL WRUSER('GIVE Z OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('ZSIZE',.FALSE.,1,NZ,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            YLIMS(1) = NY
            YLIMS(2) = NY
            ZLIMS(1) = NZ
            ZLIMS(2) = NZ
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
            AXOUT(2) = NY
            AXOUT(3) = NZ
         ENDIF
         IF(LINE.EQ.'Y')  THEN
            CALL YLIMITS(YLIMS,AXIN(1))
            IF(NQ) CALL WRUSER('GIVE X OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('XSIZE',.FALSE.,1,NX,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            IF(NQ) CALL WRUSER('GIVE Z OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('ZSIZE',.FALSE.,1,NZ,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            XLIMS(1) = NX
            XLIMS(2) = NX
            ZLIMS(1) = NZ
            ZLIMS(2) = NZ
            AXOUT(1) = NX
            AXOUT(2) = YLIMS(2)-YLIMS(1)+1
            AXOUT(3) = NZ
         ENDIF
         IF(LINE.EQ.'Z')  THEN
            CALL ZLIMITS(ZLIMS,AXIN(1))
            IF(NQ) CALL WRUSER('GIVE X OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('XSIZE',.FALSE.,1,NX,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            IF(NQ) CALL WRUSER('GIVE Y OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('YSIZE',.FALSE.,1,NY,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            XLIMS(1) = NX
            XLIMS(2) = NX
            YLIMS(1) = NY
            YLIMS(2) = NY
            AXOUT(1) = NX
            AXOUT(2) = NY
            AXOUT(3) = ZLIMS(2)-ZLIMS(1)+1
         ENDIF
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,3,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_1TO3(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,ZLIMS)
      ENDIF
C
C
C
C Case of 1-D to 2-D image
C         **********
C
      IF(INDIM.EQ.1.AND.OUTDIM.EQ.2)  THEN
C
C Enquire how the input image is to be grown
C
 123    IF(NQ) CALL WRUSER('WHICH AXIS DOES THE INPUT BECOME? (X,Y)'
     +                                                     ,STATUS)
         CALL RDKEYC('LINE',.FALSE.,1,LINE,I,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(LINE,LINE)
C
C Check it is one of them
C
         IF(LINE.NE.'X'.AND.LINE.NE.'Y')  THEN
            CALL WRUSER(' NOT ALLOWED INPUT',STATUS)
            CALL CNPAR('LINE',STATUS)
            GO TO 123
         ENDIF
C
C Enquire limits and set output dimensions for each case
C
         IF(LINE.EQ.'X')  THEN
            CALL XLIMITS(XLIMS,AXIN(1))
            IF(NQ) CALL WRUSER('GIVE Y OUTPUT DIMENSION',STATUS)
            CALL RDKEYI('YSIZE',.FALSE.,1,NY,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL) GO TO 9999
            YLIMS(1) = NY
            YLIMS(2) = NY
            AXOUT(1) = XLIMS(2)-XLIMS(1)+1
            AXOUT(2) = NY
         ENDIF
         IF(LINE.EQ.'Y')  THEN
            CALL YLIMITS(YLIMS,AXIN(1))
            IF(NQ) CALL WRUSER('GIVE X OUTPUT DIMENSIONS',STATUS)
            CALL RDKEYI('XSIZE',.FALSE.,1,NX,I,STATUS)
            IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
            XLIMS(1) = NX
            XLIMS(2) = NX
            AXOUT(1) = NX
            AXOUT(2) = YLIMS(2)-YLIMS(1)+1
         ENDIF
C
C Prepare output image and write data to it
C
       CALL WRIMAG('OUTPUT',FMT_R,AXOUT,2,POUT,STATUS)
       IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
       CALL DO_1TO2(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS,YLIMS,LINE)
      ENDIF
C
C
C
C Case of 1-D to 1-D image
C         **********
C
      IF(INDIM.EQ.1.AND.OUTDIM.EQ.1)  THEN
C
C Enquire limits and set output dimension
C
         CALL XLIMITS(XLIMS,AXIN(1))
         AXOUT(1) = XLIMS(2)-XLIMS(1)+1
C
C Prepare output image and write data to it
C
         CALL WRIMAG('OUTPUT',FMT_R,AXOUT,1,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
         CALL DO_1TO1(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,XLIMS)
      ENDIF
C
C
C
C
C
C Enquire if user wishes to make another image from the same brick
C
 9998 CALL WRUSER('DO YOU WANT TO EXTRACT ANOTHER WINDOW? (Y/N)',
     +                                                   STATUS)
      CALL RDKEYC('LOOP',.FALSE.,1,LOOP,I,STATUS)
      IF(STATUS.NE.ERR_PARNUL)  THEN
          IF(STATUS.NE.ERR_NORMAL)GO TO 9998
      ENDIF
C
C Convert to upper case if necessary
C
         CALL STR$UPCASE(LOOP,LOOP)
C
C Check validity of response
C
      IF(LOOP.NE.'Y'.AND.LOOP.NE.'N'.AND.LOOP.NE.'YES'.AND.LOOP.NE.'NO')
     +                                                         THEN
          CALL WRUSER('PLEASE ENTER Y OR N',STATUS)
          CALL CNPAR('LOOP',STATUS)
          GO TO 9998
      ENDIF
C
C Cancel all parameters except INPUT
C
      IF(LOOP.EQ.'Y'.OR.LOOP.EQ.'YES')   THEN
          CALL CNPAR('LOOP',STATUS)
          CALL CNPAR('OUTPUT',STATUS)
          CALL CNPAR('OUTDIMS',STATUS)
          CALL CNPAR('XLIMITS',STATUS)
          CALL CNPAR('YLIMITS',STATUS)
          CALL CNPAR('ZLIMITS',STATUS)
          CALL CNPAR('PLANE',STATUS)
          CALL CNPAR('LINE',STATUS)
          CALL CNPAR('XSIZE',STATUS)
          CALL CNPAR('YSIZE',STATUS)
          CALL CNPAR('ZSIZE',STATUS)
          GO TO 100
      ENDIF
C
C
C
C
C
C Close down and free images
C
 9999 CONTINUE
      CALL FRDATA(' ',STATUS)
      END



* *******************************************************************
*
* Subroutine to set window limits in X
*
*
      SUBROUTINE XLIMITS(XLIMS,NAX)
C
C Declare variables and array
C
      INTEGER NAX,STATUS,XLIMS(2)
C
C Pick up STARLINK error and format statements
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   SET DEFAULTS
C
      XLIMS(1) = 0
      XLIMS(2) = NAX-1
C
C Enquire X limits
C
   10 IF(NQ) CALL WRUSER('INPUT LIMITS IN X DIMENSION',STATUS)
      CALL RDKEYI('XLIMITS',.TRUE.,2,XLIMS,I,STATUS)
      IF(STATUS.NE.ERR_PARNUL)  THEN
         IF(STATUS.NE.ERR_NORMAL)  GO TO 10
      ENDIF
C
C Check input within range
C
      IF(XLIMS(1).LT.0.OR.XLIMS(2).GT.NAX-1)  THEN
         CALL WRUSER('NOT WITHIN LIMITS OF DATA',STATUS)
         CALL CNPAR('XLIMITS',STATUS)
         GO TO 10
      ENDIF
C

      END




* *******************************************************************
*
* Subroutine to set window limits in Y
*
*
      SUBROUTINE YLIMITS(YLIMS,NAY)
C
C Declare variables and array
C
      INTEGER NAY,STATUS,YLIMS(2)
C
C Pick up STARLINK error and format statements
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   SET DEFAULTS
C
      YLIMS(1) = 0
      YLIMS(2) = NAY-1
C
C Enquire Y limits
C
   10 IF(NQ) CALL WRUSER('INPUT LIMITS IN Y DIMENSION',STATUS)
      CALL RDKEYI('YLIMITS',.TRUE.,2,YLIMS,I,STATUS)
      IF(STATUS.NE.ERR_PARNUL)  THEN
         IF(STATUS.NE.ERR_NORMAL)  GO TO 10
      ENDIF
C
C Check input within range
C
      IF(YLIMS(1).LT.0.OR.YLIMS(2).GT.NAY-1)  THEN
         CALL WRUSER(' NOT WITHIN LIMITS OF DATA',STATUS)
         CALL CNPAR('YLIMITS',STATUS)
         GO TO 10
      ENDIF

      END




* *******************************************************************
*
* Subroutine to set window limits in Z
*
*
      SUBROUTINE ZLIMITS(ZLIMS,NAZ)
C
C Declare variables and array
C
      INTEGER NAZ,STATUS,ZLIMS(2)
C
C Pick up STARLINK error and format statements
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C Set defaults
C
      ZLIMS(1) = 0
      ZLIMS(2) = NAZ-1
C
C Enquire Z limits
C
   10 IF(NQ) CALL WRUSER('INPUT LIMITS IN Z DIMENSION',STATUS)
      CALL RDKEYI('ZLIMITS',.TRUE.,2,ZLIMS,I,STATUS)
      IF(STATUS.NE.ERR_PARNUL)  THEN
         IF(STATUS.NE.ERR_NORMAL)  GO TO 10
      ENDIF
C
C Check input within range
C
      IF(ZLIMS(1).LT.0.OR.ZLIMS(2).GT.NAZ-1)  THEN
          CALL WRUSER(' NOT WITHIN LIMITS OF DATA',STATUS)
          CALL CNPAR('ZLIMITS',STATUS)
          GO TO 10
      ENDIF

      END





* *******************************************************************
*
* Subroutine to extract a 3-D image from another 3-D image
*
*
      SUBROUTINE DO_3TO3(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,ZLIMS)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1,0:AXIN(3)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1,0:AXOUT(3)-1)
C
C Do the switch
C
      KK = -1
      DO 100 K=ZLIMS(1),ZLIMS(2)
      KK = KK + 1
      JJ = -1
      DO 100 J=YLIMS(1),YLIMS(2)
      JJ = JJ + 1
      II = -1
      DO 100 I=XLIMS(1),XLIMS(2)
      II = II + 1
      OUT(II,JJ,KK) = IN(I,J,K)
  100 CONTINUE


      END




* *******************************************************************
*
* Subroutine to extract a 2-D image from a 3-D image
*
*
      SUBROUTINE DO_3TO2(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,ZLIMS,PLANE)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1,0:AXIN(3)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1)
      CHARACTER PLANE*2
C
C Zero the output image
C
      DO 50 I=0,AXOUT(1)-1
      DO 50 J=0,AXOUT(2)-1
      OUT(I,J)=0.0
   50 CONTINUE
C
C Extract output values for each case
C
      IF(PLANE.EQ.'YZ')  THEN
         KK = -1
         DO 100 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 100 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         DO 100 I=XLIMS(1),XLIMS(2)

         OUT(JJ,KK) = OUT(JJ,KK) + IN(I,J,K)
  100    CONTINUE
      ENDIF
      IF(PLANE.EQ.'ZX')     THEN
         KK = -1
         DO 200 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         DO 200 J=YLIMS(1),YLIMS(2)
         II = -1
         DO 200 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,KK) = OUT(II,KK) + IN(I,J,K)
  200    CONTINUE
      ENDIF
      IF(PLANE.EQ.'XY')    THEN
         DO 300 K=ZLIMS(1),ZLIMS(2)
         JJ = -1
         DO 300 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 300 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ) = OUT(II,JJ) + IN(I,J,K)
  300     CONTINUE
      ENDIF


      END




* *******************************************************************
*
* Subroutine to extract a 1-D image from a 3-D image
*
*
      SUBROUTINE DO_3TO1(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,ZLIMS,LINE)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1,0:AXIN(3)-1)
      REAL OUT(0:AXOUT(1)-1)
      CHARACTER LINE*1
C
C Zero the output image
C
      DO 50 I=0,AXOUT(1)-1
      OUT(I)=0.0
   50 CONTINUE
C
C Extract output values for each case
C
      IF(LINE.EQ.'X')  THEN
         DO 100 K=ZLIMS(1),ZLIMS(2)
         DO 100 J=YLIMS(1),YLIMS(2)
         II = -1
         DO 100 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II) = OUT(II) +IN(I,J,K)
  100    CONTINUE
      ENDIF
      IF(LINE.EQ.'Y')   THEN
         DO 200 K=ZLIMS(1),ZLIMS(2)
         JJ = -1
         DO 200 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         DO 200 I=XLIMS(1),XLIMS(2)
         OUT(JJ) = OUT(JJ) + IN(I,J,K)
  200    CONTINUE
      ENDIF
      IF(LINE.EQ.'Z')  THEN
         KK = -1
         DO  300 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         DO 300 J=YLIMS(1),YLIMS(2)
         DO 300 I=XLIMS(1),XLIMS(2)
         OUT(KK) = OUT(KK) + IN(I,J,K)
  300    CONTINUE
      ENDIF

      END





* *******************************************************************
*
* Subroutine to grow a 2-D image into a 3-D image
*
*
      SUBROUTINE DO_2TO3(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,ZLIMS,MODE)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1,0:AXOUT(3)-1)
      CHARACTER*1  MODE
C
C Extract output values and build up third dimension for each case
C
      IF(MODE.EQ.'Z')   THEN
         KK = -1
         DO 100 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 100 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 100 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(I,J)
  100    CONTINUE
      ENDIF
      IF(MODE.EQ.'Y')   THEN
         KK = -1
         DO 200 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 200 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 200 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(I,K)
  200    CONTINUE
      ENDIF
      IF(MODE.EQ.'X')   THEN
         KK = -1
         DO 300 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 300 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 300 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(J,K)
  300    CONTINUE
      ENDIF

      END




* *******************************************************************
*
* Subroutine to extract a 2-D image from another 2-D image
*
*
      SUBROUTINE DO_2TO2(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1)
C
C Do the switch
C
      JJ = -1
      DO 100 J=YLIMS(1),YLIMS(2)
      JJ = JJ + 1
      II = -1
      DO 100 I=XLIMS(1),XLIMS(2)
      II = II + 1
      OUT(II,JJ) = IN(I,J)
  100 CONTINUE
      END





* *******************************************************************
*
* Subroutine to extract a 1-D image from  2-D image
*
*
      SUBROUTINE DO_2TO1(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,LINE)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2)
      REAL IN(0:AXIN(1)-1,0:AXIN(2)-1)
      REAL OUT(0:AXOUT(1)-1)
      CHARACTER LINE*1
C
C Zero the output image
C
      DO 50 I=0,AXOUT(1)-1
      OUT(I) = 0.0
   50 CONTINUE
C
C Extract the output values for each case
C
      IF(LINE.EQ.'Y')  THEN
         JJ = -1
         DO 100 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         DO 100 I=XLIMS(1),XLIMS(2)
         OUT(JJ) = OUT(JJ) + IN(I,J)
  100    CONTINUE
      ENDIF
      IF(LINE.EQ.'X')   THEN
         DO 200 J=YLIMS(1),YLIMS(2)
         II = -1
         DO 200 I=XLIMS(1),XLIMS(2)
         II = II+1
         OUT(II) = OUT(II) + IN(I,J)
  200    CONTINUE
      ENDIF


      END




* *******************************************************************
*
* Subroutine to grow a 1-D image into a 3-D image
*
*
      SUBROUTINE DO_1TO3(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS,ZLIMS)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2),ZLIMS(2)
      REAL IN(0:AXIN(1)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1,0:AXOUT(3)-1)
C
C Extract the output values and grow the image in 2 dimensions for each case
C
      IF(XLIMS(1).NE.XLIMS(2))   THEN
         KK = -1
         DO 100 K=1,ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 100 J=1,YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 100 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(I)
  100    CONTINUE
      ENDIF
      IF(YLIMS(1).NE.YLIMS(2))   THEN
         KK = -1
         DO 200 K=1,ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 200 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 200 I=1,XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(J)
  200    CONTINUE
      ENDIF
      IF(ZLIMS(1).NE.ZLIMS(2))   THEN
         KK = -1
         DO 300 K=ZLIMS(1),ZLIMS(2)
         KK = KK + 1
         JJ = -1
         DO 300 J=1,YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 300 I=1,XLIMS(2)
         II = II + 1
         OUT(II,JJ,KK) = IN(K)
  300    CONTINUE
      ENDIF

      END





* *******************************************************************
*
* Subroutine to grow a 1-D image into a 2-D image
*
*
      SUBROUTINE DO_1TO2(IN,AXIN,OUT,AXOUT,XLIMS,YLIMS)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2),YLIMS(2)
      REAL IN(0:AXIN(1)-1)
      REAL OUT(0:AXOUT(1)-1,0:AXOUT(2)-1)
C
C Extract the output values and grow to 2 dimensions for both cases
C
      IF(XLIMS(1).NE.XLIMS(2))  THEN
         JJ = -1
         DO 100 J=1,YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 100 I=XLIMS(1),XLIMS(2)
         II = II + 1
         OUT(II,JJ) = IN(I)
  100    CONTINUE
      ENDIF
      IF(YLIMS(1).NE.YLIMS(2))   THEN
         JJ = -1
         DO 200 J=YLIMS(1),YLIMS(2)
         JJ = JJ + 1
         II = -1
         DO 200 I=1,XLIMS(2)
         II = II + 1
         OUT(II,JJ) = IN(J)
  200    CONTINUE
      ENDIF
      END




* *******************************************************************
*
* Subroutine to extract a 1-D image from another 1-D image
*
*
      SUBROUTINE DO_1TO1(IN,AXIN,OUT,AXOUT,XLIMS)
C
C Declare arrays
C
      INTEGER AXIN(3),AXOUT(3),XLIMS(2)
      REAL IN(0:AXIN(1)-1),OUT(0:AXOUT(1)-1)
C
C Do the switch
C
      II = -1
      DO 100 I=XLIMS(1),XLIMS(2)
      II = II + 1
      OUT(II) = IN(I)
  100 CONTINUE

      END
