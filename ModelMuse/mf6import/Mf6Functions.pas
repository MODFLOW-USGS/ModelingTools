unit Mf6Functions;

interface

uses
  Modflow6ConstantsUnit;

type
  cint32                 = LongInt;
  cint                   = cint32;
  pcint                  = ^cint;
  TGridShape = array[0..2] of cint;
  PGridShape = ^TGridShape;
  TMfName = array[0..BMI_LENVERSION-1] of AnsiChar;
  TMfComponentName = array[0..BMI_LENCOMPONENTNAME-1] of AnsiChar;
  PMfName = ^TMfName;
  TMfNames = array of TMfName;
  PMfNames = ^TMfNames;
  cintArray = array of cint;
  PcintArray = ^cintArray;
  TErrorMessage = Array[0..BMI_LENERRMESSAGE-1] of AnsiChar;
  PErrorMessage = ^TErrorMessage;
  TGridType = Array[0..BMI_LENGRIDTYPE-1] of AnsiChar;
  TVanName = array[0..BMI_LENVARADDRESS-1] of AnsiChar;
  TVarNames = array of TVanName;
  TMf6GridType = (gtDISU, gtDISV, gtDIS);

function initialize () : cint;cdecl;external 'libmf6.dll';
function update () : cint;cdecl;external 'libmf6.dll';
function finalize () : cint;cdecl;external 'libmf6.dll';
function get_version (var mf_version : TMfName) : cint;cdecl;external 'libmf6.dll';
function get_component_name(var name : TMfComponentName) : cint;cdecl;external 'libmf6.dll';
function get_output_item_count(var count : cint) : cint;cdecl;external 'libmf6.dll';

// grid_id start at 1.
//function get_grid_type(grid_id : pcint; GridType: PAnsiChar) : cint;cdecl;external 'libmf6.dll';
function get_grid_type(grid_id: pcint; var GridType: TGridType) : cint;cdecl;external 'libmf6.dll';

function get_grid_shape(grid_id : pcint; var GridShape: TGridShape): cint;cdecl;external 'libmf6.dll';
function get_output_var_names(Names: PAnsiChar): cint;cdecl;external 'libmf6.dll';
//function get_output_var_names(var Names: TVarNames): cint;cdecl;external 'libmf6.dll';
function get_value_int(Variablename: PAnsiChar; AnArray: pcint): cint;cdecl;external 'libmf6.dll';
function get_value_double(Variablename: PAnsiChar; AnArray: pdouble): cint;cdecl;external 'libmf6.dll';
function get_last_bmi_error(Error: PErrorMessage): cint;cdecl;external 'libmf6.dll';

function get_current_time(var current_time: double): cint;cdecl;external 'libmf6.dll';
function get_end_time(var end_time: double): cint;cdecl;external 'libmf6.dll';

// get_grid_face_count only succeeds for DISU grids.
function get_grid_face_count(grid_id: pcint; var count: cint) : cint;cdecl;external 'libmf6.dll';
// get_grid_face_nodes only succeeds for DISU grids.
function get_grid_face_nodes(grid_id: pcint; var count: cint) : cint;cdecl;external 'libmf6.dll';
// get_grid_node_count only succeeds for DISU grids.
// get_grid_nodes_per_face only succeeds for DISU grids.

// get_grid_rank only works for DIS grids.
function get_grid_rank(grid_id: pcint; var grid_rank: cint) : cint;cdecl;external 'libmf6.dll';

// get_grid_size has a bug for DISV grids.
function get_grid_size(grid_id: pcint; var count: cint) : cint;cdecl;external 'libmf6.dll';
function get_grid_x(grid_id: pcint; x: pdouble) : cint;cdecl;external 'libmf6.dll';
function get_grid_y(grid_id: pcint; y: pdouble) : cint;cdecl;external 'libmf6.dll';

function get_var_shape(c_var_address: PAnsiChar; c_var_shape: pcint) : cint;cdecl;external 'libmf6.dll';

function set_value_int(Variablename: PAnsiChar; AnArray: pcint): cint;cdecl;external 'libmf6.dll';


function get_var_type(c_var_address: PAnsiChar; c_var_type: PAnsiChar): cint; cdecl; external 'libmf6.dll';
function get_var_rank(c_var_address: PAnsiChar; Var c_var_rank: Integer): cint; cdecl; external 'libmf6.dll';
function get_value_string(c_var_address: PAnsiChar; c_arr_ptr: PAnsiChar): cint; cdecl; external 'libmf6.dll';
function get_value_bool(c_var_address: PAnsiChar; c_arr_ptr: PLongBool): cint; cdecl; external 'libmf6.dll';
function get_var_nbytes(c_var_address: PAnsiChar; var var_nbytes: Integer): cint; cdecl; external 'libmf6.dll';
//function get_var_shape(c_var_address: PAnsiChar; var c_var_shape: TGridShape) result(bmi_status)

function get_var_itemsize(c_var_address: PAnsiChar; var var_size: integer): cint; cdecl; external 'libmf6.dll';

// other functions
{
function bmi_initialize() result(bmi_status)
function bmi_update() result(bmi_status)
function bmi_finalize() result(bmi_status)
function get_start_time(var start_time: double) result(bmi_status)
function get_end_time(var end_time: double) result(bmi_status)
function get_current_time(var current_time: double) result(bmi_status)

// time-step represents the length of the current time step not the time step number.
function get_time_step(var time_step: double) result(bmi_status)

function get_input_item_count(var count: Integer) result(bmi_status)
function get_output_item_count(var count: Integer) result(bmi_status)
// Returns all input variables in the simulation
function get_input_var_names(c_names) result(bmi_status)
function get_output_var_names(c_names) result(bmi_status)
//  the size (in bytes) of a single element of a variable
function get_var_itemsize(c_var_address, var_size) result(bmi_status)
function get_var_nbytes(c_var_address, var_nbytes) result(bmi_status)
function get_var_type(c_var_address, c_var_type) result(bmi_status)
function get_var_rank(c_var_address, c_var_rank) result(bmi_status)
function get_var_shape(c_var_address, c_var_shape) result(bmi_status)

function get_value_bool(c_var_address, c_arr_ptr) result(bmi_status)
function get_value_string(c_var_address, c_arr_ptr) result(bmi_status)
function get_value(c_var_address, c_arr_ptr) result(bmi_status)
function get_value_ptr(c_var_address, c_arr_ptr) result(bmi_status)
function get_value_ptr_double(c_var_address, c_arr_ptr) result(bmi_status)
function get_value_ptr_int(c_var_address, c_arr_ptr) result(bmi_status)
function get_value_ptr_bool(c_var_address, c_arr_ptr) result(bmi_status)
}

implementation

end.
