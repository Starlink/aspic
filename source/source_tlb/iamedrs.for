      PROGRAM IAMEDRS
*+
*     Program:-  IAMEDRS.
*
*
*                        +-----------+
*                        |           |
*                        |  IAMEDRS. |
*                        |           |
*                        +-----------+
*
*
*  Purpose:-
*    Convert a file of IAM parameterised data to the EDRS XYlist
*    format.
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
* 1 IAMEDRS
*
*   IAMEDRS converts a frame of parameterised data produced by the IAM
* suite into the XYlist format of the EDRS package. The various
* table handling utilities in EDRS and GRASP can then be used on
* the converted dataset.
*   The user is first prompted for the name of the file generated
* by the IAM suite. Next he is prompted for the name of the file
* to hold the XYlist. The conversion then proceeds.
*
* A C Davenhall./ROE/                                          6/10/83.
*
* 2 Parameters
*
*   The version of IAM installed in ASPIC generates 15 parameters for
* each image detected. For compatibility with EDRS and GRASP the
* first three parameters stored in each record are XY positions
* and a magnitude. IAM produces two positions for each image, an
* unweighted and an intensity weighted centroid. It is the
* unweighted position which is stored at the start of the record.
*   A title for each parameter is written as a descriptor to the
* XYlist. For each parameter its position in the record, title,
* a brief description and its units are given below;
*
*    Position  Title     Description.                      Units.
*        1     UXCEN   Unweighted X centroid.              pixels.
*        2     UYCEN       "      Y    "    .                "   .
*        3     MAG     Total magnitude.                    magnitudes.
*        4     UMAJAX  Unweighted semi-major axis.         pixels.
*        5     UMINAX      "       "  -minor  "  .           "   .
*        6     UTHETA      "      orientation.             degrees.
*        7     IXCEN   Intensity weighted X centroid.      pixels.
*        8     IYCEN       "        "     Y    "    .        "   .
*        9     IMAJAX  Intensity weighted semi-major axis.   "   .
*       10     IMINAX      "        "      "  -minor  "  .   "   .
*       11     ITHETA      "        "     orientation.     degrees.
*       12     AREA    Image area.                         pixels.
*       13     MAXINT  Maximum intensity in image.
*       14     UELLIP  Unweighted ellipticity.
*       15     IELLIP  Intensity weighted ellipticity.
*
* Note;     ellipticity = 1.0 - (b/a)
*
* where     a = semi-major axis and b = semi-minor axis.
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*  Subroutines called;
*   IAMEDRSS.
*
*  A C Davenhall./ROE/                                          6/10/83.
*-
      CALL IAMEDRSS
      END
