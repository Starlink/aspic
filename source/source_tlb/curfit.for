      PROGRAM CURFIT
*+
*
*    CURFIT
*
*    Purpose : To calculate the astrometric position and photometric
*              parameters for  a stellar image  identified using the
*              box-cursor on the ARGS.
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
*    Usage   : The STARLINK frame containing the data to be analysed
*              should  be displayed  on the  ARGS,  using  ADISP for
*              example.  CURFIT  is  then  run.  The  user  will  be
*              prompted for the following parameters.
*              i  ) MODE  - this is the  number of parameters  to be
*                           fitted.  The range is from 3 to 5 with a
*                           default of 3.
*              ii ) IAREA - this  determines the  size of  the  area
*                           over which  the fit  will be  performed.
*                           The  range of values  is from  20 to 512
*                           with a default of 40.
*              iii) EXP   - the saturation  exponent.  If  MODE is 5
*                           this will be the  starting value used by
*                           the fitting procedure, otherwise it will
*                           remain fixed.
*              iv ) DSAT  - the saturation  density.  If  MODE  is 3
*                           this value will remain fixed,  otherwise
*                           it will be the initial value used by the
*                           fitting procedure.
*              The user will  then be prompted  for the name  of the
*              STARLINK image containing  the data and will be asked
*              to centre the box-cursor on  the object for which the
*              fit is to  be performed. The results of the  fit will
*              be appended to the file  OBJECTS.LIS, if no such file
*              exists then it will be created.
*
*    Subroutine called :
*    CURFIT1           : E2DASP
*
*    D.W.T.Baines/ROE/1983
*
*-
      CALL CURFIT1
      END
