      SUBROUTINE MOVLEV( JLOC, ILOC, STATUS )
*+
*  Name:
*     MOVLEV

*  Purpose:
*     Move all HDS objects within a structure to another structure

*SOURCE
*     MOVLEV.FOR in UTILITIES.TLB

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MOVLEV( JLOC, ILOC, STATUS )

*  Description:
*     All the objects within the object located by JLOC are moved to
*     identically named objects within the object located by ILOC. The
*     original objects are deleted, as is the object which contained
*     them.

*  Arguments:
*     JLOC = CHARACTER (Given)
*        The HDS locator to the object whose contents are to be moved.
*     ILOC = CHARACTER (Given)
*        The HDS locator to the object which is to receive the contents
*        of the object located by JLOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1990 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(DAT__SZLOC) JLOC
      CHARACTER*(DAT__SZLOC) ILOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   COMP              ! Index to current component.
      CHARACTER KLOC*(DAT__SZLOC) ! Locator to a component within source
                                 ! object.
      CHARACTER NAME*(DAT__SZNAM) ! Name of current object.
      INTEGER   NCOMP             ! No. of components within the
                                  ! source object.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out how many components are contained within the source object.
      CALL DAT_NCOMP( JLOC, NCOMP, STATUS )

*  Loop round moving each component.
      DO COMP = 1, NCOMP

*  Get a locator to this component.
         CALL DAT_INDEX( JLOC, COMP, KLOC, STATUS )

*  Get the name of the component.
         CALL DAT_NAME( KLOC, NAME, STATUS )

*  Copy the component and all sub-components.
         CALL DAT_COPY( KLOC, ILOC, NAME, STATUS )

*  Anull the locator to the component.
         CALL DAT_ANNUL( KLOC, STATUS )

      END DO

* Erase the original object.
      CALL DAT_NAME( JLOC, NAME, STATUS )
      CALL DAT_PAREN( JLOC, KLOC, STATUS )
      CALL DAT_ERASE( KLOC, NAME, STATUS )


      END
