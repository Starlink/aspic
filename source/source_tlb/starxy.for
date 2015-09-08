      PROGRAM STARXY
*+
*
* Program :- STARXY
*
* Purpose :-
*            To measure astrometric X,Y positions of user selected stars
*            in an image and to save the list of positions in a file .
*
* Usage   :-
*            The image containing the stars to be measured is displayed on
*            an ARGS , by using ADISP for example . STARXY is then run .
*            A box cursor is centred on a star to be measured . The size
*            of the box , which determines the area over which the fit is
*            performed , can be altered using the trackerball keys .
*            The box can have a size in the range 20 to 512 pixels with a
*            default value of 40 . Measurements are taken by using the
*            MEASURE key or the program terminated by using the ABORT key .
*            The functions of the trackerball keys are displayed at the
*            bottom of the ARGS screen . The measured positions are
*            writen to both the terminal and the file OBJECTS.LIS .
*
*
*----------------------------------------------------------------------------
*
* Subroutines called : STXY
*
* D.W.T.Baines / ROE /                                                12/1/83
*
*-
      CALL STXY
      END
