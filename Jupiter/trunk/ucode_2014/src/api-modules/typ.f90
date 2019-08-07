! This file is part of the JUPITER API, documented in:
!
! Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, 
! JUPITER: Joint Universal Parameter IdenTification and Evaluation 
! of Reliability--An application programming interface (API) for 
! model analysis: U.S. Geological Survey Techniques and Methods, 
! book 6, chap. E1, 268 p.  
! (Also available at http://pubs.usgs.gov/tm/2006/tm6e1/.)
!
! For the latest updates to source code and documentation, go to:
! http://water.usgs.gov/software/JupiterApi/
!
MODULE DATATYPES
  IMPLICIT NONE
  PRIVATE
  !
  !   Public data types
  PUBLIC :: LNODE, LLIST, DMATRIX, CDMATRIX, CHAR_1D_30
  !
  !   Public interfaces
  PUBLIC :: TYP_DEALLOC, TYP_NULL
  !
  !--------------------------------------------------
  !   Data types used in construction of linked lists
  !--------------------------------------------------
  !
  TYPE :: LNODE
    !   One structure of this type is one node in a linked list
    CHARACTER(LEN=40)                       :: KEYWORD
    CHARACTER(LEN=40)                       :: VALUE
    INTEGER                                 :: NCHAR
    CHARACTER(LEN=1), POINTER, DIMENSION(:) :: STRING
    TYPE (LNODE), POINTER                   :: NEXTNODE
  END TYPE LNODE
  !
  TYPE :: LLIST
    !   One structure of this type allows access to a linked list of
    !   information associated with one KEYITEM in an information block.
    !   The pointer NEXTLIST will point to the next LLIST structure, which
    !   allows access to the linked list associated with the next KEYITEM.
    CHARACTER(LEN=12)     :: GROUP
    TYPE (LNODE), POINTER :: LHEAD, LTAIL
    TYPE (LLIST), POINTER :: NEXTLIST
  END TYPE LLIST
  !
  !----------------------------------
  !   Data types for storing matrices
  !----------------------------------
  !
  TYPE :: DMATRIX
    !   One structure of this type can hold a 2-D DOUBLE PRECISION array
    !   dimensioned (NR,NC)
    CHARACTER(LEN=12)                         :: ARRAYNAME
    INTEGER                                   :: NR
    INTEGER                                   :: NC
    DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: DVAL
  END TYPE DMATRIX
  !
  TYPE :: CDMATRIX
    !   One structure of this type can hold a compressed, 2-D DOUBLE-PRECISION
    !   array dimensioned (NR,NC)
    CHARACTER(LEN=12)                       :: ARRAYNAME
    INTEGER                                 :: IDIM
    INTEGER                                 :: NNZ
    INTEGER                                 :: NR
    INTEGER                                 :: NC
    DOUBLE PRECISION, POINTER, DIMENSION(:) :: DVAL
    INTEGER(KIND=8),  POINTER, DIMENSION(:) :: IPOS
    INTEGER,          POINTER, DIMENSION(:) :: ICOL
    !   IDIM is the dimension of DVAL and IPOS
    !   NNZ is number of non-zero entries in true matrix and in DVAL; it must
    !       not exceed IDIM
    !   NR is number of rows in true matrix
    !   NC is number of columns in true matrix, and dimension of ICOL
    !   DVAL contains non-zero elements of true matrix
    !   IPOS contains position in true matrix of corresponding element
    !       in DVAL (assuming column-major storage order)
    !   ICOL contains position in DVAL and IPOS of first stored (usually
    !       non-zero) element in each column of true matrix; equals 0 if no
    !       values in column are stored
  END TYPE CDMATRIX
  !
  !-----------------------------------------------------
  !   Data types for storing arrays of character strings
  !-----------------------------------------------------
  !
  TYPE :: CHAR_1D_30
    !   One structure of this type can hold a 1-D CHARACTER(LEN=30) array
    !   dimensioned (NDIM)
    INTEGER                                  :: NDIM
    CHARACTER(LEN=30), POINTER, DIMENSION(:) :: CVAL
  END TYPE CHAR_1D_30
  !
  INTERFACE TYP_DEALLOC
    !   A generic interface for deallocating all arrays and nullifying all
    !   pointers in a derived-type structure
    MODULE PROCEDURE TYP_DEALLOC_CDM
    MODULE PROCEDURE TYP_DEALLOC_DM
    MODULE PROCEDURE TYP_DEALLOC_LNODE
    MODULE PROCEDURE TYP_DEALLOC_LLIST
    MODULE PROCEDURE TYP_DEALLOC_C1D30
  END INTERFACE
  !
  INTERFACE TYP_NULL
    !   Generic interface to nullify pointer components of a structure
    !   of a derived data type
    MODULE PROCEDURE TYP_NULL_DM
    MODULE PROCEDURE TYP_NULL_CDM
    MODULE PROCEDURE TYP_NULL_LLIST
    MODULE PROCEDURE TYP_NULL_LNODE
    MODULE PROCEDURE TYP_NULL_C1D30
  END INTERFACE
  !
CONTAINS
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_DEALLOC_C1D30(C1D)
    !   Deallocate any arrays defined by pointers
    IMPLICIT NONE
    !
    !   Argument-list variable
    TYPE (CHAR_1D_30), INTENT(INOUT) :: C1D
    !
    !   Local variables
    !
    IF (ASSOCIATED(C1D%CVAL)) DEALLOCATE(C1D%CVAL)
    NULLIFY(C1D%CVAL)
    RETURN
  END SUBROUTINE TYP_DEALLOC_C1D30
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_DEALLOC_CDM(CDM)
    !   Deallocate any arrays defined by pointers
    IMPLICIT NONE
    !
    !   Argument-list variable
    TYPE (CDMATRIX), INTENT(INOUT) :: CDM
    !
    !   Local variables
    !
    IF (ASSOCIATED(CDM%DVAL)) DEALLOCATE(CDM%DVAL)
    IF (ASSOCIATED(CDM%IPOS)) DEALLOCATE(CDM%IPOS)
    IF (ASSOCIATED(CDM%ICOL)) DEALLOCATE(CDM%ICOL)
    NULLIFY(CDM%DVAL, CDM%IPOS, CDM%ICOL)
    RETURN
  END SUBROUTINE TYP_DEALLOC_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_DEALLOC_DM(DM)
    !   Deallocate any arrays defined by pointers
    IMPLICIT NONE
    !
    !   Argument-list variable
    TYPE (DMATRIX), INTENT(INOUT) :: DM
    !
    !   Local variables
    !
    IF (ASSOCIATED(DM%DVAL)) DEALLOCATE(DM%DVAL)
    NULLIFY(DM%DVAL)
    RETURN
  END SUBROUTINE TYP_DEALLOC_DM
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_DEALLOC_LNODE(LNO)
    !   Deallocate a linked list
    IMPLICIT NONE
    !
    !   Argument-list variable
    TYPE (LNODE), POINTER :: LNO
    !
    !   Local variables
    INTEGER :: ISTAT
    TYPE (LNODE), POINTER :: CURRENT, PREVIOUS
    !
    CURRENT => LNO
    DO WHILE (ASSOCIATED(CURRENT))
      PREVIOUS => CURRENT
      CURRENT => CURRENT%NEXTNODE
      IF (ASSOCIATED(PREVIOUS%STRING)) DEALLOCATE(PREVIOUS%STRING,STAT=ISTAT)
      DEALLOCATE(PREVIOUS,STAT=ISTAT)
    ENDDO
    NULLIFY(LNO)
    RETURN
  END SUBROUTINE TYP_DEALLOC_LNODE
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_DEALLOC_LLIST(LLI)
    !   Deallocate a nest of linked lists
    IMPLICIT NONE
    !
    !   Argument-list variable
    TYPE (LLIST), POINTER :: LLI
    !
    !   Local variables
    INTEGER :: ISTAT
    TYPE (LLIST), POINTER :: CURRENT, PREVIOUS
    !
    CURRENT => LLI
    DO WHILE (ASSOCIATED(CURRENT))
      PREVIOUS => CURRENT
      CURRENT => CURRENT%NEXTLIST
      IF (ASSOCIATED(PREVIOUS%LHEAD)) CALL TYP_DEALLOC_LNODE(PREVIOUS%LHEAD)
      DEALLOCATE(PREVIOUS,STAT=ISTAT)
    ENDDO
    NULLIFY(LLI)
    RETURN
  END SUBROUTINE TYP_DEALLOC_LLIST
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_NULL_DM(DM)
    !   Nullify pointer components of a DMATRIX structure
    !
    !   Argument-list variable
    TYPE (DMATRIX), INTENT(INOUT) :: DM
    !
    NULLIFY(DM%DVAL)
    RETURN
  END SUBROUTINE TYP_NULL_DM
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_NULL_CDM(CDM)
    !   Nullify pointer components of a CDMATRIX structure
    !
    !   Argument-list variable
    TYPE (CDMATRIX), INTENT(INOUT) :: CDM
    !
    NULLIFY(CDM%DVAL,CDM%IPOS,CDM%ICOL)
    RETURN
  END SUBROUTINE TYP_NULL_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_NULL_LLIST(LL)
    !   Nullify pointer components of an LLIST structure
    !
    !   Argument-list variable
    TYPE (LLIST), INTENT(INOUT) :: LL
    !
    NULLIFY(LL%LHEAD,LL%LTAIL,LL%NEXTLIST)
    RETURN
  END SUBROUTINE TYP_NULL_LLIST
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_NULL_LNODE(LN)
    !   Nullify pointer components of an LNODE structure
    !
    !   Argument-list variable
    TYPE (LNODE), INTENT(INOUT) :: LN
   !
    NULLIFY(LN%NEXTNODE)
    RETURN
  END SUBROUTINE TYP_NULL_LNODE
  !-----------------------------------------------------------------------------
  SUBROUTINE TYP_NULL_C1D30(C1D30)
    !   Nullify pointer components of a C1D30 structure
    !
    !   Argument-list variable
    TYPE (CHAR_1D_30), INTENT(INOUT) :: C1D30
    !
    NULLIFY(C1D30%CVAL)
    RETURN
  END SUBROUTINE TYP_NULL_C1D30
  !
END MODULE DATATYPES
