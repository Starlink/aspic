      PROGRAM GAUFIT
*+
*
*    GAUFIT
*
*    Purpose : To produce  astrometric positions  and  photometric
*              parameters,  determined  from a  gaussian fit,  for
*              a number of stars for which  approximate positions,
*              in the input  STARLINK  data frame,  are given in a
*              second STARLINK frame.  This frame could be created
*              by program COORDS for example.
*
*    Summary : For each image the X and Y positions, and errors on
*              the positions  will be  calculated.  The number  of
*              photometric  parameters  which  are  fitted to  the
*              image is decided by the user. Three parameters, the
*              magnitude index, background level and 1 sigma image
*              width will always be calculated.  The remaining two
*              parameters, the  saturation density and  saturation
*              exponent,  which determine the shape of the profile
*              as the image saturates, can either be calculated or
*              used as  constants. This   allows  these  last  two
*              parameters to be determined from a number of bright
*              images and then applied as constants to all images
*              to be measured.
*
*    Usage   : When GAUFIT is run the following parameters will be
*              prompted for.
*              i  ) MODE  - this is the number of parameters to be
*                           fitted. The range is  from 3 to 5 with
*                           a default of 3.
*              ii ) IAREA - this  gives the size of the area  over
*                           which  the fit  will be performed  for
*                           each star. The range is from 20 to 512
*                           pixels with a default of 40.
*              iii) EXP   - the saturation exponent. if  MODE is 5
*                           this value  will be the initial  value
*                           used by the fitting routine, otherwise
*                           it will remain fixed.
*              iv ) DSAT  - the saturation density. if  MODE  is 3
*                           this value will be fixed, otherwise it
*                           will be the starting value used by the
*                           fitting routine.
*              The user  will be asked to supply a  STARLINK frame
*              containing  the image data  followed by a  STARLINK
*              frame  containing  the  approximate  positions  for
*              the objects  for which the fit is to  be performed.
*              The results will be output to the terminal and also
*              written  to the  file  OBJECTS.LIS.  If such a file
*              already exists then a  new version will be created.
*
*    Subroutine called :
*    GAUFIT1           : E2DASP
*
*    D.W.T.Baines/ROE/April 1983
*
*-
      CALL GAUFIT1
      END
