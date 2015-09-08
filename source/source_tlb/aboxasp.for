      PROGRAM ABOXASP
*+
*     Program:- ABOXASP.
*
*
*                      +------------+
*                      |            |
*                      |  ABOXASP.  |
*                      |            |
*                      +------------+
*
*
*  Purpose:-
*
*      Define a series of boxes around user selected features
*   on an image and save the coordinates of the defined areas
*   as a file.
*
*
*  Usage:-
*
*     The image to be studied is displayed on an Args, eg. by
*   using ADISP.  ABOXASP is then run.  The cross-hair cursor
*   is  then  used to  define the  required rectangles on the
*   image.  The  user  is  given  the  option  to  reject  an
*   unsatisfactory box.  Up to 30 boxes may be selected. When
*   all  the  required boxes  have  been chosen  the  user is
*   prompted for the  filename of the Starlink image to  hold
*   their coordinates.  Once  the  file  has  been  saved the
*   program terminates.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called:
*
*			ABOX
*
*  A C Davenhall./ROE/                                      20/8/82.
*-

      CALL ABOX
      END
