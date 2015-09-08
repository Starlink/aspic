C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  ICDISP *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ICDISP [MDIS=TRUE AVERAG=TRUE LOG=TRUE WRAP=TRUE
C                       XC=? YC=? TRIM=FALSE DISINV=?]
C
C             A call to see all the image with contrast designed to give
C             best display of faint features would be
C                    ICDISP 'file' = = MDIS=T
C             A call to see all the image with it scaled for display
C             from its min to max values would be
C                    ICDISP 'file' = = = =
C
C
C          FUNCTION:-
C               It displays an image on the  ARGS. The image is scaled so
C               that a chosen range of values corresponds to the range from
C               0(black) to 255(white). The range selection can be done
C               automatically so that faint features are well seen.
C                  An area of the image can be selected to be displayed
C               (the default is the whole image)
C                 If either of the sides of the area that has been
C               chosen to be displayed is larger than 512 (the max size
C               of the ARGS display) then the area is displayed
C               by binning the data into squares to make a suitable
C               smaller image (the default binning is just by taking one
C               value in each square, but the user can choose to have it
C               done by averaging all the values in each square)
C
C
C          USE:-
C               This routine is used to look at I*2 data, but can look
C               any data, treating it as I*2 integers.
C                 Note  the  LOG  parameter  and remember that by choosing
C               suitable values for PVLO,PVHI  a  negative  display  is
C               generated.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image to  be
C                                             displayed.  It  may be of any
C                                             size.
C         XRANGE          Entire              X limits of area to be
C                                             displayed
C
C         YRANGE          Entire              Y limits of area to be
C                                             displayed
C
C         PVLO            Min                 Data with Min value are scaled
C                                             to 0 (black) for display
C
C         PVHI            Max                 Data with Max value are scaled
C                                             to 255 (white) for display
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C         XC              256                 The x co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         YC              256                 The y co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         MDIS            FALSE               Flag for automatic selection
C                                             of display levels as
C                                             (mean+3sigma) to (mean-3sigma)
C                                             for best ARGS viewing of
C                                             faint objects
C
C         DISINV          PVHI                The value INVALID pixels are
C                                             to be displayed as.
C
C         TRIM            TRUE                If TRUE  values  outside  the
C                                             range PVLO to PVHI are set to
C                                             those values.
C                                             If FALSE then the values are
C                                             not scaled at PVLO,PVHI, but
C                                             0 to 255 and wrapped around,
C                                             so this disables MDIS,LOG,WRAP.
C
C         WRAP            FALSE               If FALSE then values outside
C                                             PVLO to PVHI are set to those
C                                             values.
C                                             If TRUE then the values outside
C                                             are instead wrapped round at
C                                             (PVHI-PVLO) cycles.
C
C
C         LOG             FALSE               If  TRUE   then   logarithmic
C                                             scaling  is  used between the
C                                             defined   limits,   otherwise
C                                             linear scaling is used.
C
C         AVERAG          TRUE                If TRUE then the binning down
C                                             to get below 512X512 is done
C                                             by averaging the values in
C                                             each box. (If an 'invalid'
C                                             pixel is included, the whole
C                                             box is displayed as invalid.)
C                                             If FALSE it is done
C                                             simply by taking the first
C                                             value in each box (this saves
C                                             time).
C
C
C      STARLINK OUTPUT PARAMTERS
C
C         COMFAC                              Output as ICDISP_COMFAC, the
C                                             compression factor used for
C                                             the display
C
C         DX                                  Output as ICDISP_DX, the X
C                                             coordinate in the input image
C                                             of the first pixel in the
C                                             displayed image.
C
C         DY                                  Output as ICDISP_DY, the Y
C                                             coordinate in the input image
C                                             of the first pixel in the
C                                             displayed image.
C
C
C
C         Various (mainly AJP)     RGO                             82-6-25
C
C
C--------------------------------------------------------------------------



*
*  ICDISP IMAGE [XRANGE] [YRANGE] [FLOOR] [CEILING] [ARGS X] [ARGS Y]
*               [DISINV] [TRIM] [LOG] [AVERAG] [MDIS]
*
*  PURPOSE
*       To display on the ARGS a section of s Starlink I*2 image,
*       suitably scaled
*
*  METHOD
*       This is a version of the DISP program
*       It allows:-
*        1) Use of I*2 images
*        2) Display of compressed images
*        3) Windowing of the image
*        4) Fixing of position displayed on the ARGS
*        5) Scaling by log as well as linear and triming or not of
*           values outside display range
*        6) Updating of the ARGS database file
*        7) Automatic selection of best display levels
*        8) Wrapping around at the display range cycle
*
*        The image is obtained. The windowed area is obtained and
*        the compression factor needed to display that on the 512x512
*        pixels of the ARGS calculated. The position on the ARGS display
*        is obtained. The array display position factors are sent to the
*        environment. The value displayed range is obtained.
*        If it is not input, it is given by the total range in the area.
*        The value 'invalid' values are to be displayed is obtained.
*        A scaled shrunk array is calculated and displayed on the ARGS.
*        The ARGS database file is updated. The mean value at the centre
*        of the displayed area is calculated, and if it was not done for
*        the range, the minimum and maximum values in the displayed area
*        are calculated.
*
*
*        EXPERIMENTAL VERSION USING 'DATA STRUCTURE'
*        ORIG PTW MODIF FOR DATABASE BY WFL JULY 1981
*        MODIF AUG 81 WFL - EXTRA TRIM AND LOG PARAMETERS
*        Modified from ADISP to IDISP by KFH on 19/2/82
*        FURTHER MODIFIED TO ICDISP BY AJP ON 82/6/21
*
*
* ----------------------------------------------------------
C
C
C
      PROGRAM ICDISP
C
C
C
      INTEGER NSIZE(2)
      CHARACTER*72 TITLE
C
C
C
      CALL DISIMG(IPIN,NSIZE,BSCALE,BZERO,INVAL,TITLE,IERR)
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



