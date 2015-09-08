      SUBROUTINE CVINFO (SCAL,THADD,TITLE,NCON,BASE,CINT,SMOOTH,
     :                   NSIZE,SIGMA,DEVICE)
*+
*
*     CVINFO
*
*     This routine gets all the information from the user 
*     concerning the contour and vector plots .
*
*     Parameters :
*
*     Returned  Type     Usage
*     SCAL       R       polarization vector scale factor
*     THADD      R       angle to be added to the polarization angle
*     TITLE      C       title for the plot
*     NCON       I       number of contours for the plot
*     BASE       R       level of the first contour
*     CINT       R       interval between contours
*     SMOOTH     C       the type of smoothing (if any)
*     NSIZE      I       the size of the smoothing box
*     SIGMA      R       the sigma for the gaussian smooth
*     DEVICE     C       the device to be used for plotting
*
*     Subroutines called :
*     WRUSER , READI , READR , READC , MULREP   : ASPFACE
*     UPPCAS                                    : E2DASP
*
*     D.W.T.Baines/ROE/Feb 1983/
*
*-
      INTEGER IST , NCON , NSIZE
      REAL PI , SCAL , THADD , BASE , CINT , SIGMA 
      CHARACTER*(*) TITLE , SMOOTH , DEVICE
      CHARACTER*40 OBUFF
      PARAMETER (PI = 3.141593)
*
*     set the status return variable to 0
*
      IST = 0
*
*     request the scale factor for the polarization vectors
*
      CALL READR ('SCALF','Input Polarization Vector scale factor',
     :            1.,0.1,10.,SCAL,IST)
*
*     request a constant angle to be added to the polarization angle
*
      CALL READR ('THETP','Angle to be added to Polarization angle ?',
     :            0.,-180.,180.,THADD,IST)
*
*     convert THADD to radians
*
      THADD = THADD * PI / 180.
*
*     request a title for the plot
*
      CALL READC ('TITLE','Give a title for the plot',
     :             ' ',' ','}',TITLE,IST)
*
*      make sure title is in upper case
*
      CALL UPPCAS ( TITLE )
*
*     get the number of contours to be used 
*
      CALL READI ('NCONT','Number of contours for the plot ?',
     :            10,1,20,NCON,IST)
*
*     get level of first contour
*
      CALL READR ('CBASE','Base contour level ?',
     :            0.,-1.0E10,1.0E10,BASE,IST)
*
*     get the interval between contours
*
      CALL READR ('CINTER','Interval between contours ?',
     :            100.,-1.0E10,1.0E10,CINT,IST)
*
*     find out what smoothing option is to be used ( if any )
*
      CALL WRUSER ('The smoothing options are :',IST)
      CALL WRUSER ('Box average , Gaussian or No smoothing',IST)
      CALL MULREP ('Which smoothing option ?',
     :             'NONE,BOX,GAUSSIAN$',SMOOTH,IST)
      IF ( SMOOTH .NE. 'NONE' ) THEN
*
*        get the size of the box aver which smoothing is to be done
*
         CALL WRUSER ('Input the size (in pixels) of the area',IST)
         CALL WRUSER ('over which smoothing is to be performed .',IST)
         CALL WRUSER ('This must be an odd number of pixels .',IST)
         CALL READI ('ISIZE','Size of smoothing box ?',
     :               5,3,15,NSIZE,IST)
*
*        check that this is an odd number of pixels
*
         IF ( MOD(NSIZE,2) .EQ. 0 ) THEN
*
*           add one if its an even number
*
            NSIZE = NSIZE + 1
*
*           tell the user the new value
*
            WRITE (OBUFF,'(''Size of smoothing box taken as'',I3)')NSIZE
            CALL WRUSER (OBUFF,IST)
         ENDIF
*
*        if a gaussian smooth requested , get the value of SIGMA
*
         IF ( SMOOTH .EQ. 'GAUSSIAN' ) THEN
            CALL READR ('SIGMA','Input sigma for gaussian (in pixels)',
     :                  2.,1.,10.,SIGMA,IST)
         ENDIF
      ENDIF
*
*     request the graphics device for the plot
*
      CALL MULREP ('Which output device ?',
     :             'T4014,T4010,ARGS,VERSATEK$',DEVICE,IST)
	END
