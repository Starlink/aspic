      PROGRAM PRFPRTASP
*+
*     Program:- PRFPRTASP.
*
*
*                     +----------------+
*                     |                |
*                     |   PRFPRTASP.   |
*                     |                |
*                     +----------------+
*
*
*  Purpose:-
*      Program to produce a lineprinter listing of an extracted
*      profile.
*
*
*  Description:-
*      The purpose of PRFPRTASP is to list on the lineprinter a
*   profile (major or minor axis or equivalent profile) previously
*   extracted from a galaxy image and saved as a disk file.
*   Generally the values of the profile will be printed out
*   precisely as they are held in the file. However an option
*   is available, which is only meaningful for profiles extracted
*   from an image converted to intensity relative to a sky of 1.0
*   (the sky not being subtracted), to output the profile in
*   magnitudes per square arcsec. If this option is chosen the
*   sky brightness in magnitudes per square arcsec must be
*   supplied.
*
*   Note; Points with an intensity of less than 1.0, for which
*         the surface brightness is undefined, will arbitarily
*         be assigned the value of 30.0 magnitudes per square
*         arcsec.
*
*
*  Usage:-
*     The user is first prompted for the filename of the profile
*   to be listed and then for the title to appear at the head of
*   the listing.
*     Next he has to choose whether the file is to be output
*   exactly as it is stored or converted to mag./square arcsec
*   prior to output. If the former option is chosen the profile
*   is listed on the printer and the program terminates. If the
*   user chooses to convert the profile to mag./square arcsec
*   (see above for the necessary type of input profile for
*   this to be sensible) he will be prompted for the sky
*   brightness, which should be supplied in mag./sq. arcsec.
*   Then the file will be sent to the printer and the program
*   terminates.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   PRFPRT.
*
*  A C Davenhall./ROE/                                         1/8/82.
*-
      CALL PRFPRT
      END
