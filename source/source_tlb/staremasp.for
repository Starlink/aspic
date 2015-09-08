      PROGRAM STAREMASP
*+
*     Program:- STAREMASP.
*
*
*
*                      +--------------+
*                      |              |
*                      |  STAREMASP.  |
*                      |              |
*                      +--------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     Interactive removal of foreground stars or plate blemishes
*   from a galaxy image.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     The image from which unwanted objects are to be removed
*   should be displayed on an Args, eg. by using ADISP. STAREMASP
*   can then be run. Firstly the user will be prompted for the
*   name of the file containing the original image and the name
*   of the file which is to contain the cleaned image.
*     The user will then be presented with a menu of several
*   commands which can be used to remove unwanted blemishes
*   from the image. The following commands are available;
*
*  SKY  (SK)  Use the cross-hair cursor to define a patch of
*             blank (clear) sky. The mean value of this patch
*             will be computed.
*
*  SET  (S)   Use the cross-hair cursor to define a rectangular
*             box around an object to be removed. Note SET
*             only defines the box and does not remove the
*             object. Thus if a mistake is made simply
*             repeat SET.
*
*  ZAP  (Z)   Replace the points in the box defined by SET
*             with the mean determined from the last use of SKY.
*             Note any number of objects can be replaced with
*             the value from a single use of sky. However
*             SKY must be used at least once initially to
*             define a value for the sky.
*
*  REPLACE (R) Replace the points inside a cursor defined
*             region with a polynomial fitted to the
*             periphery of the region. Whilst ZAP is suited
*             to regions beyond the galaxy where the
*             background is essentially flat REPLACE can
*             be used to remove stars embedded in the body
*             of the galaxy. Care, however, must be taken with
*             its use to ensure that a realistic replacement
*             is being made. In particular the choice of
*             aperture size can be critical to the success
*             of the replacement.
*
*  CURCOL (C) Select new colours for the cursor.
*
*  HELP  (H)  List the commands available.
*
*  EXIT  (E)  Terminate the program.
*
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   E2D:-   STAREM.
*
*  A C Davenhall./ROE/                                  28/9/82.
*
*  Loosly based on ideas by P. S. Bunclark/St. And/ 1976-79.
*-
      CALL STAREM
      END
