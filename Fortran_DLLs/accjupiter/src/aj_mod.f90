MODULE AJ_MOD
  !   Module AJ_MOD contains subprograms called from subprograms that are part 
  !   of the Access-Jupiter (AJ) library.  These subprograms are intended to be
  !   called only from Fortran.
  !
CONTAINS
  !
  !#############################################################################
  !
  SUBROUTINE CHAR2DSTRING(CHARVAR,DSTRING,IERR)
    ! Convert a Fortran character variable to a Delphi string, which
    ! uses first byte to store string length
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN)  :: CHARVAR  ! Fortran character variable
    CHARACTER(LEN=*), INTENT(OUT) :: DSTRING  ! Delphi string
    INTEGER,          INTENT(OUT) :: IERR
    !
    !   Local variables
    INTEGER :: LENTRIMCH, LENTRIMCHP1, LENDSTR, itmp
    !
    IERR = 0
    DSTRING = ' '
    DSTRING(1:1) = CHAR(0)
    LENTRIMCH = LEN_TRIM(CHARVAR)
    LENTRIMCHP1 = LENTRIMCH+1
    LENDSTR = LEN(DSTRING)
    IF (LENTRIMCHP1 > LENDSTR) THEN
      IERR = 1
      RETURN
    ENDIF
    DSTRING(1:1) = CHAR(LENTRIMCH)
    DSTRING(2:LENTRIMCHP1) = CHARVAR(1:LENTRIMCH)
    RETURN
  END SUBROUTINE CHAR2DSTRING
  !
  !#############################################################################
  !
  SUBROUTINE DSTRING2CHAR(DSTRING,CHARVAR,IERR)
    !   Convert a Delphi string, which uses first byte to store string length, 
    !   to a Fortran character variable
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN)  :: DSTRING  ! Delphi string
    CHARACTER(LEN=*), INTENT(OUT) :: CHARVAR  ! Fortran character variable
    INTEGER,          INTENT(OUT) :: IERR
    !
    !   Local variables
    INTEGER :: LENCH, LENTRIMDSTR, LENTRIMDSTRM1, LENSTR
    CHARACTER(LEN=1) :: CHAR1
    INTEGER*1 :: INT1
    EQUIVALENCE (CHAR1,INT1)
    !
    IERR = 0
    CHAR1 = DSTRING(1:1)
    CHARVAR = ' '
    LENSTR = INT1
    LENCH = LEN(CHARVAR)
    IF (LENSTR > LENCH) THEN
      IERR = 1
      RETURN
    ENDIF
    CHARVAR = DSTRING(2:LENSTR+1)
    RETURN
  END SUBROUTINE DSTRING2CHAR
  !
  !#############################################################################
  !
  INTEGER FUNCTION LOG2INT(LOGIC)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: LOGIC
    IF (LOGIC) THEN
      LOG2INT = 1
    ELSE
      LOG2INT = 0
    ENDIF
    RETURN
  END FUNCTION LOG2INT
  !
  !#############################################################################
  !
END MODULE AJ_MOD