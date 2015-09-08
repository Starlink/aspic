      PROGRAM ANNOTASP
*+
*     Program:- ANNOTASP.
*
*
*                     +------------+
*                     |            |
*                     |  ANNOTASP  |
*                     |            |
*                     +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*      Annotate an image displayed on an Args with text strings,
*   arrows or scale length bars.
*
*
*  Description:-
*      The purpose of ANNOTASP is to enable the annotation of an
*   image displayed on the Args with user supplied text strings,
*   arrows or scale length bars. A typical use would be prior to
*   photographing an image displayed on the screen.
*     The routine has two modes of operation; either all graphics
*   will be written to the same overlay plane in the Args or
*   successive graphical operations will be cyclyed through
*   the overlay planes, this later mode being the default. The
*   advantage of this mode of operation is that it enables the
*   most recently displayed graphics to be cleared if a mistake
*   is made, without obliterating any previous display. The
*   disadvantage is that since all eight available overlay
*   planes are used the annotation could interfere with other
*   graphics that might be present. It was to avoid this
*   that the option of putting all annotation in a single
*   specified plane was included.
*     The following commands are available;
*
*  WRITE (W) A text string to be displayed on the screen and its
*            position are obtained from the user. The string
*            is then displayed in the indicated position.
*
*  SCALE (S) A scale length bar of user chosen length is
*            displayed at a user chosen position. This would
*            typically be used to indicate the scale (arcsec
*            or kpc for example) of a direct image. After
*            drawing the bar WRITE would then be used to
*            display a string indicating its length.
*
*  Note;     SCALE prompts the user to input the pixel size of the
*            frame and the length of the bar in "astronomical
*            units". This means the units with which the image
*            is to be annotated, eg. Kpc, seconds of arc etc.
*            and has nothing to do with the distance between the
*            Earth and the Sun!!
*
*  ARROW (A) A user defined arrow is added to the display, to
*            point out some feature.
*
*  MODE (M)  This command allows the user to control the way
*            ANNOTASP works in two ways;
*       (i)  He can determine whether the positions required
*            by subsequent usages of WRITE, SCALE and ARROW
*            are to be obtained from the Args cursor or
*            input from the keyboard.
*       (ii) The size of characters displayed by WRITE can also
*            be controlled.
*
*  CLEAR (C) Clear a single overlay plane. If the graphics
*            are being rotated through the overlay planes
*            the last displayed graphics will be cleared.
*            If the graphics are not being so rotated all
*            the graphics will be cleared.
*
*  CLEARALL (CA) Clear all annotation. If the graphics are being
*            rotated through the overlay planes all planes will
*            be cleared. If a single plane is being used that
*            plane only will be cleared.
*
*  COLOUR (CO) Select a colour for the graphics displayed.
*
*  ROTATE (R) Determine whether or not graphics are to be
*            rotated through the overlay planes. The default
*            is that they are. If the user selects to put all
*            annotation in a single plane the plane required
*            will be prompted for.
*
*  HELP (H)  List the commands available.
*
*  EXIT (E)  Terminate annotation.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*
*	CHARG
*
*  A C Davenhall./ROE/                                  28/7/82.
*-
      CALL CHARG
      END
