      SUBROUTINE PROMAR
     :      (ADDR,ANALYSER,ARECUT,COMPL,CS,F,F0,I,ICAT,IMAGE,INCOMP,
     :       IYPOS,MARKER,MXLEN,NEWMAR,NONIM,NOTSET,NUMLEN,NUMPAR,
     :       PS,S,S0,STORE)
 
*+
*   PROMAR
*
*   Processes connectivity markers for the cosmos analyser.
*
*   Given      (arguments)
*   ADDR    IA  address for object data in STORE
*   ARECUT  I   area cut in pixels
*   COMPL   I   fixed value, a possible state of PS
*   CS      I   flag for whether current pixel is image or nonimage
*   F       I   fixed value, connectivity marker for finish of object
*   F0      I   fixed value, connectivity marker for
*               finish of object segment
*   I       I   pixel number along current scan
*   ICAT    I   count of completed objects
*   IMAGE   I   fixed value, a possible state of CS and PS
*   INCOMP  I   fixed value, a possible state of PS
*   IYPOS   I   Y-coordinate of current pixel
*   MXLEN   I   X-dimension of image frame
*   NEWMAR  I   connectivity marker for current pixel
*   NONIM   I   fixed value, a possible state of CS
*   NOTSET  I   fixed value, indicating undefined start-address
*               entry in object stack
*   NUMLEN  I   maximum possible number of objects
*   NUMPAR  I   maximum possible number of object parameters
*   S       I   fixed value, connectivity marker for start of object
*   S0      I   fixed value, connectivity marker for start of
*               object segment
*   STORE   DA  store for accumulating object parameters
*
*   Given      (stacks)
*   OBSTAK      objects currently being traversed
*   PSSTAK      previous-scan connectivity flags
*   FREE        unused indices to data storage area
*
*   Returned   (arguments)
*   ADDR    IA  addresses for object data in STORE
*   ANALYSER RA final object parameters
*   ICAT    I   count of completed objects
*   MARKER  IA  array of connectivity markers
*   PS      I   flag for image connectivity on previous scan
*   STORE   DA  store for accumulating object parameters
*
*   Returned   (stacks)
*   OBSTAK      objects currently being traversed
*   PSSTAK      previous-scan connectivity flags
*   FREE        unused indices to data storage area
*
*   Subroutines called :
*   PUSH,POP,GETSTK,PUTSTK,UPDATE,PARAD,MOMCAL  : E2DLIB
*
*   B.D.Kelly/ROE/15.3.1982
*   A C Davenhall./ROE/12.6.84 {Modified to include X & Y image min.
*                               and max. in fields.}
*-
      INTEGER ARECUT,COMPL,CS,F,F0,I,ICAT,IMAGE,INCOMP,
     &        IYPOS,MXLEN,NEWMAR,NONIM,NOTSET,PS,S,S0,YMAX
      INTEGER ADDR(MXLEN),MARKER(MXLEN+1),OBJECT(3)
      REAL ANALYSER(NUMPAR,NUMLEN)
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2)

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)

      PARAMETER (EPSLON=0.001,RADDEG=57.2958)
*
*
*
*
*   If, on previous scan, are at start of an image, then
*
      IF (NEWMAR.EQ.S) THEN
*
*     push PS onto PSSTAK
*
        CALL PUSH('PSSTAK',PS)
*
*       if current pixel is an image pixel, then
*
        IF (CS.EQ.IMAGE)  THEN
*
*        if current pixel is at start position of current image, then
*          set top of stack pointing to data storage area
*               to value stored at current position
*        else
*          combine data pointed to by top of stack with
*               data pointed to from current position
*        endif
*
          CALL GETSTK('OBSTAK',OBJECT)
          IF(OBJECT(1).EQ.I) THEN
            OBJECT(3)=ADDR(I)
            CALL PUTSTK('OBSTAK',OBJECT)
          ELSE
            KK1=OBJECT(3)
            KK2=ADDR(I)
            IF(STORE(YEXT,KK1).GE.STORE(YEXT,KK2)) THEN
              KL=KK1
              KS=KK2
            ELSE
              KL=KK2
              KS=KK1
            ENDIF
            CALL UPDATE(KK1,KL,KS,MXLEN,STORE)
*
*          Push discarded data area onto free data stack
*
            CALL PUSH('FREE',KK2)
          ENDIF
*
*      else
*        push COMPLete onto PSSTAK
*        push new data address and 'undefined' start address
*             onto object stack
*      endif
*
        ELSE
          CALL PUSH('PSSTAK',COMPL)
          OBJECT(1)=NOTSET
          OBJECT(2)=0
          OBJECT(3)=ADDR(I)
          CALL PUSH('OBSTAK',OBJECT)
        ENDIF
*
*      set previous scan connectivity flag to IMAGE
*      finished this case
*   endif
*
        PS=IMAGE
        RETURN
      ENDIF
 
 
*
*   If on previous scan, are at start of an image segment, then
*      if current pixel is image, 
*      and previous scan not within image
*      and no known segments to come, then
*         combine the parameters of the top two objects on the
*                   data stack
*         pop PSSTAK
*      endif
*      set connectivity flag for previous scan to IMAGE
*      finished this case
*   endif
*
      IF (NEWMAR.EQ.S0) THEN
        IF((CS.EQ.IMAGE).AND.(PS.EQ.COMPL)) THEN
          CALL PARAD(MARKER,MXLEN,NOTSET,S0,STORE)
          CALL POP('PSSTAK',PS)
        ENDIF
        PS=IMAGE
        RETURN
      ENDIF
 
 
*
*   If, on previous scan, are at end of an image segment, then
*      set connectivity flag for previous scan to INCOMPlete
*      finished this case
*   endif
*
      IF (NEWMAR.EQ.F0) THEN
        PS=INCOMP
        RETURN
      ENDIF
 
 
*
*     MARKER IS F, THEREFORE IMAGE FINISHED ON PREVIOUS SCAN
*
*
*   pop PS from PSSTAK
*
      CALL POP('PSSTAK',PS)
*
*   If current pixel is image
*   or previous scan is within an object
*   or previous scan contains segments yet to come which are
*   connected to current object, then
*      finished this case
*   endif
*
      IF ((CS.EQ.IMAGE).OR.(PS.NE.COMPL)) RETURN
 
 
*
*   If have started the image on this scan, then
*      overwrite last end-of-segment marker written for this
*      image by an end of image marker
*      put top of address stack at address corresponding to
*      start-of-image marker
*      increment Y-extent for top of data area stack
*      pop object data stack
*      pop PS from PSSTAK
*      finished this case
*   endif
*
      CALL POP('OBSTAK',OBJECT)
      IF (OBJECT(1).NE.NOTSET) THEN
        MARKER(OBJECT(2))=F
        ADDR(OBJECT(1))=OBJECT(3)
        STORE(YEXT,OBJECT(3))=STORE(YEXT,OBJECT(3))+1.0
        CALL POP('PSSTAK',PS)
        RETURN
      ENDIF
 
 
*
*   Read top of address stack
*
      KK1=OBJECT(3)
*
*   If image too small in area, then
*      push data area onto free data stack
*      pop object data stack
*      pop PS from PSSTAK
*      finished this case
*   endif
*
      IF(NINT(STORE(AREA,KK1)).LE.ARECUT) THEN
        CALL PUSH('FREE',KK1)
        CALL POP('PSSTAK',PS)
        RETURN
      ENDIF
 
 
*
*     HAVE COMPLETED A NEW IMAGE
*
      ICAT=ICAT+1
      YMAX=IYPOS-1
*
*
*  CALCULATE NORMALISED AND CENTRALISED SECOND MOMENTS
* -----------------------------------------------------
*
      CALL MOMCAL(ANALYSER,ICAT,KK1,MXLEN,NUMLEN,NUMPAR,STORE,YMAX)
*
*  STORE THE X & Y MIN. AND MAX. VALUES FOR THIS OBJECT.
* -------------------------------------------------------
*
      ANALYSER(14,ICAT)=STORE(XMIN,KK1)
      ANALYSER(15,ICAT)=STORE(XMAX,KK1)
      ANALYSER(16,ICAT)=ANALYSER(7,ICAT)-(STORE(YEXT,KK1)/2.0E0)
      ANALYSER(17,ICAT)=ANALYSER(7,ICAT)+(STORE(YEXT,KK1)/2.0E0)
*
*
*
*  TIDY UP
* ---------
*
*
*   Put top of data storage stack back on the free stack
*
      CALL PUSH('FREE',KK1)
*
*   pop PS from PSSTAK
*
      CALL POP('PSSTAK',PS)

      RETURN
      END
