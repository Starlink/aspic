      PROGRAM TARGET
*+
*     Program:- TARGET.
*
*     (Part of the STARSIM suite).
*
*
*                    +----------+
*                    |          |
*                    |  TARGET. |
*                    |          |
*                    +----------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*      Overlay markers showing the brightest stars in an artificial
*   image frame on top of the frame displayed on an Args.
*
*
*  General Description:-
*      When an artificial image frame is generated using STARSIM
*   a "star catalogue" file is created holding the positions and
*   apparent magnitudes of all the stars in the field. As an
*   aid in identifying particular stars TARGET has been provided
*   to overlay "gun-sight" markers around the positions of all
*   stars brighter than a given threshold, when the original
*   image frame is displayed on an Args.
*
*
*  Usage:-
*      The simulated image frame should be displayed on an Args
*   (eg. by using APLOT). The command TARGET is then typed
*   and the name of the "star catlogue" file is prompted
*   for. Next the magnitude of the faintest star to be plotted
*   is prompted for.
*      A marker is plotted for every star in the catalogue
*   brighter than the threshold. The marker consists of a
*   "gun-sight" centred on the position of the star. To the right
*   of the gun-sight is the identification number of the star
*   which corresponds to the running count in the output listing
*   produced by STARSIM.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   TARGETS.
*
*  A C Davenhall./ROE/                                     23/1/83.
*-
      CALL TARGETS
      END
