program testaj
use ucode_data, only: inunit
use utilities, only: utl_getarg, utl_getunit
implicit none
external aj_ini_parameter_data
!DEC$ ATTRIBUTES DLLIMPORT :: AJ_INI_PARAMETER_DATA
integer :: np, npars
character(len=200) :: infile

inunit = utl_getunit(7,100)
infile = utl_getarg(1)
if (infile /= '') then
  open(inunit,file=infile,status="OLD")
else
  stop
endif

call aj_ini_parameter_data(npars)
np = npars

end program testaj