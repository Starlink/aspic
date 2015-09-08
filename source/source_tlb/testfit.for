      PROGRAM TESTFIT
*+
*
*    TESTFIT
*
*    Purpose : To calculate the astrometric position and photometric
*              parameters of a  stellar image for which  approximate
*              coordinates are input by the user.
*
*    Summary : For  the chosen  image  the  X and Y  positions,  and
*              errors  on  the  positions  will  be  calculated. The
*              number of photometric parameters  which are fitted to
*              the image is decided  by the user.  Three parameters,
*              the  magnitude index,  background level  and  1 sigma
*              image width will always be  calculated. The remaining
*              two parameters, the saturation density and saturation
*              exponent, which determine the shape of the profile as
*              the image saturates, can either be calculated or used
*              as constants.  This allows these last  two parameters
*              to be  used as constants  if the values  for the data
*              being  measured have  been  calculated  from  fits to
*              bright images.
*
*    Usage   : TESTFIT  will prompt  for the  following  parameters.
*              i  ) MODE  - the number  of parameters to  be fitted.
*                           The range is from  3 to 5 with a default
*                           of 3.
*              ii ) IAREA - the size of the  area over which the fit
*                           is to be performed. The range is from 20
*                           to  512  pixels  with a  default of  40.
*              iii) EXP   - the  saturation exponent, if  MODE  is 5
*                           this  value will be  the starting  value
*                           used by the fitting  procedure otherwise
*                           it will be fixed.
*              iv ) DSAT  - the  saturation  density, if  MODE  is 3
*                           this value  will remain fixed  otherwise
*                           it will  be the  starting value  used by
*                           the fitting procedure.
*              The user  will then be asked to provide the  STARLINK
*              frame containing the  data on which the  fit is to be
*              performed and then the last two parameters requested.
*              v  ) IXPOS - the   approximate   X-position   of  the
*                           stellar image on which  the fit is to be
*                           performed.
*              vi ) IYPOS - the   approximate   Y-position   of  the
*                           stellar image on which  the fit is to be
*                           performed.
*              The  results will be output to the file  OBJECTS.LIS,
*              if  no such  file exists  then one  will be  created.
*
*    Subroutine called :
*    TESTFIT1          : E2DASP
*
*    D.W.T.Baines/ROE/April 1983
*
*-
      CALL TESTFIT1
      END
