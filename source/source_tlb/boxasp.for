      PROGRAM BOXASP
*+
*     Program:- BOXASP.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     Use the cross-hair cursor to define a rectangular
*   region of an image displayed on the Args and return
*   the coords. of this region to the environment.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     The image to be examined should have been displayed on an
*   Args eg. by using ADISP. BOXASP is then run. The cross-hair
*   cursor is used to pick of either pair of opposite corners
*   of the required rectangle. The user is asked if the defined
*   rectangle is acceptable. If it is not the process is
*   repeated until an acceptable rectangle is found. When
*   an acceptable rectangle has been defined its coords. are
*   written to the environment for use by subsequent programs.
*   BOXASP then terminates.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   BOXS.
*
*  A C Davenhall/ROE/                                   28/9/82.
*-
      CALL BOXS
      END
