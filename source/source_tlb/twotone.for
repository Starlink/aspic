      PROGRAM TWOTONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C                           *************
C                           *  TWOTONE  *
C                           *************
C
C
C	CALLING SEQUENCE:-
C		TWOTONE
C
C
C	FUNCTION:-
C		Takes two 2-D starlink images and forms a single coded
C		"pseudo-colour" image of the same size.  The coded image
C               is saved as a new disk file, as well as displayed on the
C		ARGS.
C
C	Use:-
C		For creating a pseudo colour image of an object from
C               two monochromatic images taken through different filters
C               (or different polarizations, etc.).  Each pixel of the
C               output image is assigned one of 16 different intensity levels
C		depending on the maximum scaled intensity of the two input
C		images.  Then within each intensity level the pixel is assigned
C		one of up to 31 different colours, (ranging from red,
C		through yellow, white, and cyan to blue) depending on the
C               intensity ratio of the input red and blue images.
C
C		N.B: TO DISPLAY THE OUTPUT IMAGE PROPERLY, THE LOOK UP TABLE
C		     CONTAINED IN THE FILE "LUT2TONE.BDF" MUST FIRST BE LOADED
C		     INTO THE ARGS. TWOTONE will attempt to load the LUT
C                    (using the connection file default file name).
C
C	USER PARAMETERS:-
C
C	RED					This is the RED starlink
C						2D image.
C
C	RLIMITS					The lower and upper scaling
C						limits for the RED array.
C						The lower limit represents
C						the zero intensity, or
C						sky background level; the
C						upper level gives the level
C						to be mapped onto the maximum
C						ARGS brightness level. By
C						varying the upper limit, the
C						contrast and colour balance
C						of the output array can be
C						altered.
C
C	BLUE					This is the blue starlink
C						2D image.
C
C	BLIMITS					The lower and upper scaling
C						limits for the BLUE array.
C
C
C	LOG		false			If true, then logarithmic
C						scaling will be used for the
C						output image; otherwise linear
C						scaling is used.
C
C	ENHANCE		1.0			The calculated colour saturation
C						of each pixel will be multiplied
C						by this factor.
C
C	OUT					The name for the output 2D
C						starlink array containing
C						the encoded colours. This
C						image can be displayed on the
C						ARGS using the program AFLASH,
C						after first loading the
C						pseudo colour look up table
C						contained in LUT2TONE.BDF.
C
C	NORMALLY DEFAULTED PARAMETER:-
C
C	PSEUD			LUT2TONE	THE STARLINK 2D FILE CONTAINING
C						THE PSEUDO COLOUR LUT
C
C
C	Author
C
C		Written by WDP  at the AAO
C		Based in part on the FCPACK program written by KFH at RGO
C
C	Date
C		Nov 1982
C
C------------------------------------------------------------------------------
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   IXR  and IXB contain the dimensions of Red and Blue.
C   IPR ,  and IPB are pointers to Red and Blue.
C   ISTAT is a general status reply.
C   IPOUT is a pointer to the output image.
C
      LOGICAL LLOG
      INTEGER IXR(2),IXB(2)
      INTEGER IPR,IPB
      INTEGER ISTAT,IPOUT
      REAL RLIMITS(2),BLIMITS(2)
C
C   First get the 2 input images.
C   Note that only one try is allowed, the images must all be 2-D
C   and of the same size.
C
      CALL WRUSER('ENTER NAME OF THE RED IMAGE:',ISTAT)
      CALL RDIMAG('RED',FMT_R,2,IXR,NDIM,IPR,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL WRUSER('ENTER THE LOWER AND UPPER PIXEL SCALING LIMITS;',
     1 ISTAT)
      CALL RDKEYR('RLIMITS',.FALSE.,2,RLIMITS,NVALS,JSTAT)
      CALL WRUSER('ENTER NAME OF THE BLUE IMAGE:',ISTAT)
      CALL RDIMAG('BLUE',FMT_R,2,IXB,NDIM,IPB,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL WRUSER('ENTER THE LOWER AND UPPER PIXEL SCALING LIMITS;',
     1 ISTAT)
      CALL RDKEYR('BLIMITS',.FALSE.,2,BLIMITS,NVALS,JSTAT)
C
C   Now check that the arrays have the same dimensions - exit if not.
C
      IF ( IXR(1).NE.IXB(1) ) THEN
         CALL WRERR('ERRXDIM')
         GO TO 800
      END IF
      IF ( IXR(2).NE.IXB(2) ) THEN
         CALL WRERR('ERRYDIM')
         GO TO 800
      END IF
C
      CALL WRUSER('USE LOG. INTENSITY SCALE?',ISTAT)
      LLOG=.FALSE.
      CALL RDKEYL('LOG',.TRUE.,1,LLOG,NVALS,ISTAT)
C
      CALL WRUSER('COLOUR ENHANSEMENT FACTOR=?',ISTAT)
      ENHANSE=1.
      CALL RDKEYR('ENHANSE',.TRUE.,1,ENHANSE,NVALS,ISTAT)
C
C   Now set up the output frame - of the same size as both
C   the input frames - so use IXR for it's dimensions.
C
      CALL WRUSER('NAME FOR OUTPUT IMAGE=?',ISTAT)
      CALL WRIMAG('OUT',FMT_SW,IXR,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
C
C   It is now possible to do the packing.
C
      CALL TONE2(%VAL(IPR),%VAL(IPB),%VAL(IPOUT),
     :          IXR(1),IXR(2),RLIMITS,BLIMITS,LLOG,ENHANSE)
C
  800 CONTINUE
      END
C************************************************************************
      SUBROUTINE TONE2 (RED,BLUE,OUT,N1,N2,RLIMITS,BLIMITS,LLOG,
     1  ENHANSE)
C
C      This routine takes 2 input images and packs
C      them into a single output image , using a coding which,
C      when displayed with the right look-up-table, gives
C      a falsecolour display.
C
C      RED  and BLUE are the TWO input arrays.
C      OUT is the output array.
C
      PARAMETER NGREY=16
      PARAMETER PIE2=1.570796,PIE2D=1.5708, PIE4=.785398
      LOGICAL LLOG
      INTEGER MDIMLUT(2)
      REAL RED(N1*N2),BLUE(N1*N2)
      INTEGER*2 OUT(N1*N2)
      REAL RLIMITS(2),BLIMITS(2)
      INTEGER NCOLOUR(NGREY),NPOINT(NGREY)
      DATA NCOLOUR/1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31/
      DATA NPOINT/0,1,4,9,16,25,36,49,64,81,100,121,144,169,
     1            196,225/
C
C     NGREY IS THE NUMBER OF GREYSCALE LEVELS IN THE ARGS LUT
C     NCOLOUR IS THE NUMBER OF DIFFERENT COLOURS WITHIN EACH
C          SHADE OF GREY
C     NPOINT IS A POINTER TO THE LOCATION IN THE LUT OF THE FIRST
C          COLOUR ENTRY FOR THAT SHADE OF GREY
C
      RL=RLIMITS(1)
      BL=BLIMITS(1)
      RSCALE=255./(RLIMITS(2)-RLIMITS(1))
      BSCALE=255./(BLIMITS(2)-BLIMITS(1))
      IF (LLOG)THEN
        RSCALOG=255./LOG(RLIMITS(2)/RLIMITS(1))
        BSCALOG=255./LOG(BLIMITS(2)/BLIMITS(1))
      END IF
C
      DO I=1,N1*N2
        RV=MAX(MIN(RSCALE*(RED(I)-RL),255.),0.)
        BV=MAX(MIN(BSCALE*(BLUE(I)-BL),255.),0.)
C
C       FIND COLOUR ANGLE
C         0 DEGREES = PURE RED
C        45 DEGREES = WHITE
C        90 DEGREES = PURE BLUE
C
        IF (RV .GT. 0.)THEN
          ANGLE=ATAN(BV/RV)
          IF (ENHANSE .NE. 1.)THEN
            ANGLE=(ANGLE-PIE4)*ENHANSE+PIE4
          END IF
          ANGLE=MAX(MIN(ANGLE,PIE2),0.)
        ELSE
          ANGLE=0.
        END IF
C
C       FIND GREYSCALE LEVEL
C
        IF (LLOG)THEN
          IF (RED(I).GT. RL)THEN
            RV=RSCALOG*LOG(RED(I)/RL)
          ELSE
            RV=0.
          END IF
          IF (BLUE(I) .GT. BL)THEN
            BV=BSCALOG*LOG(BLUE(I)/BL)
          ELSE
            BV=0.
          END IF
        END IF
        INTEN=INT(MAX(RV,BV)/255.*NGREY)+1
        INTEN=MIN(MAX(INTEN,1),NGREY)
C
C       FIND QUANTIZED COLOUR VALUE
C
        ICOLOUR=INT(ANGLE/PIE2D*NCOLOUR(INTEN))
C
C       FINALLY DETERMINE LOOK UP TABLE VALUE OF OUTPUT IMAGE
C
        OUT(I)=NPOINT(INTEN)+ICOLOUR
      END DO
C
C     DISPLAY ON ARGS
C
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('ARGS NOT AVAILABLE',ISTAT)
        RETURN
      END IF
C
C    LOAD THE PSEUDO COLOUR LOOK UP TABLE
C
      MDIMLUT(1)=3
      MDIMLUT(2)=256
      CALL RDIMAG('PSEUD',104,2,MDIMLUT,JUNK,IPOINT,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('FAILED TO LOAD THE PSEUDO COLOUR LUT',ISTAT)
      ELSE
        CALL SRCOLS(0,256,%VAL(IPOINT))
        CALL WRUSER('THE PSEUDO COLOUR LUT HAS BEEN LOADED',ISTAT)
      END IF
C
C     DISPLAY THE IMAGE
C
      CALL SRPXI2(OUT,N1,N1,N2,256-N1/2,256-N2/2,16,.FALSE.,
     1   IDUMMY,1)
C
C     UPDATE THE ARGS DATABASE
C
      CALL ARGS_WRIM (256,256,N1,N2,N1,N2,JSTAT)
      IF(JSTAT.NE.0)THEN
         CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)
      ENDIF
      END
