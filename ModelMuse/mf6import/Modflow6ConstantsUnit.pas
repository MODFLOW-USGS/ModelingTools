unit Modflow6ConstantsUnit;

interface

uses
  System.Math;

const
  // from Utilities\Constants.f90.
  IUSERFORMATSTRIP = -99 ; //< default user format strip
  IUSERFORMATWRAP = 99 ; //< default user format wrap
  LENBIGLINE = 5000 ; //< maximum length of a big lineimplementation
  LENHUGELINE = 50000 ; //< maximum length of a huge line
  LENVARNAME = 16 ; //< maximum length of a variable nameend.
  LENCOMPONENTNAME = 16 ; //< maximum length of a component name
  LENCONTEXTNAME = 16 ; //< maximum length of a memory manager context
  LENSOLUTIONNAME = LENCOMPONENTNAME ; //< maximum length of the solution name
  LENMODELNAME = LENCOMPONENTNAME ; //< maximum length of the model name
  LENPACKAGENAME = LENCOMPONENTNAME ; //< maximum length of the package name
  LENEXCHANGENAME = LENCOMPONENTNAME ; //< maximum length of the exchange name
  LENBUDROWLABEL = 2 * LENPACKAGENAME + 1 ; //< maximum length of the rowlabel string used in the budget table
  LENMEMSEPARATOR = 1 ; //< maximum length of the memory path separator used, currently a '/'
  LENMEMPATH =
  LENCONTEXTNAME +
  2 * LENCOMPONENTNAME +
  2 * LENMEMSEPARATOR ; //< maximum length of the memory path
  LENMEMADDRESS =
  LENMEMPATH +
  LENMEMSEPARATOR +
  LENVARNAME ; //< maximum length of the full memory address, including variable name
  LENAUXNAME = 16 ; //< maximum length of a aux variable
  LENBOUNDNAME = 40 ; //< maximum length of a bound name
  LENBUDTXT = 16 ; //< maximum length of a budget component names
  LENPACKAGETYPE = 7 ; //< maximum length of a package type (DIS6, SFR6, CSUB6, etc.)
  LENFTYPE = 5 ; //< maximum length of a package type (DIS, WEL, OC, etc.)
  LENOBSNAME = 40 ; //< maximum length of a observation name
  LENOBSTYPE = 30 ; //< maximum length of a observation type (CONTINUOUS)
  LENTIMESERIESNAME = LENOBSNAME ; //< maximum length of a time series name
  LENTIMESERIESTEXT = 16 ; //< maximum length of a time series text
  LENDATETIME = 30 ; //< maximum length of a date time string
  LINELENGTH = 300 ; //< maximum length of a standard line
  LENLISTLABEL = 500 ; //< maximum length of a llist label
  MAXCHARLEN = LENBIGLINE;
  MAXOBSTYPES = 100 ; //< maximum number of observation types
  NAMEDBOUNDFLAG = -9 ; //< named bound flag
  LENPAKLOC = 34 ; //< maximum length of a package location
  IZERO = 0 ; //< integer constant zero
  IWETLAKE = 10000 ; //< integer constant for a dry lake
  MAXADPIT = 100 ; //< maximum advanced package Newton-Raphson iterations
  MAXMEMRANK = 3; //< maximum memory manager length (up to 3-dimensional arrays)
  LENMEMTYPE = 50; //< maximum length of a memory manager type

  LENERRMESSAGE = 1024; //< max length for the error message
  LENGRIDTYPE = 16; //< max length for Fortran grid type string

  // from srcbmi\mf6bmiUtil.f90
  BMI_LENCOMPONENTNAME = 256; // max. length for the variable's address C-string
  BMI_LENVARADDRESS = LENMEMADDRESS + 1; // component name length, i.e. 'MODFLOW 6'
  BMI_LENERRMESSAGE = LENERRMESSAGE + 1; // max. length for the (exported) C-style error message
  BMI_LENGRIDTYPE =  LENGRIDTYPE + 1; //< max. length for grid type C-strings
  BMI_LENVARTYPE = LENMEMTYPE + 1; //< max. length for variable type C-strings
  BMI_LENVERSION = 256; //< length of version string, e.g. '6.3.1' or '6.4.1-dev'

  Mf6Undefined = -842150451;

implementation

end.