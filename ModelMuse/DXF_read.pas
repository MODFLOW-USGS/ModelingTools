{
  April 4, 2004,
    Modified by Richard Winston to eliminate memory leaks
    and to eliminate compiler hints and warnings.
    Updated email for John Biddiscombe: jbiddiscombe@skippingmouse.co.uk
}
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//                         DXF File reader object/code                       //
//                             ©John Biddiscombe                             //
//                      Rutherford Appleton Laboratory, UK                   //
//                           j.biddiscombe@rl.ac.uk                          //
//                       DXF code release 3.0 - July 1997                    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Thanks very much to John F Herbster for the original DXF reader class     //
// that got this started --- extract from his header follows...              //
//                                                                           //
// Pgm. 07/14/95 by John F Herbster, CIS:72714,3445, Houston, TX.            //
// for Rick Rogers (CIS:74323,3573).                                         //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

unit DXF_read;  

interface

uses
{$IFDEF CLX}
  SysUtils, QControls, QComCtrls,QDialogs, Classes,DXF_Structs,DXF_Utils,Math;
{$ELSE}
  Windows,SysUtils,Controls, StdCtrls,ComCtrls,Dialogs,Classes,Graphics,
  DXF_structs,DXF_Utils,Math;
{$ENDIF}

const
  message_delay_ms = 1500;
  EOL: AnsiString = #13#10;
  StrEOL: string = #13#10;

// Thanks to Ian L. Kaplan, whose code contained these ID's
// I've changed a few names here and there
const
  DXF_start            = 0;
  DXF_text_def         = 1;
  DXF_name             = 2;
  DXF_text_prompt      = 3;
  DXF_othername2       = 4;
  DXF_entity_handle    = 5;
  DXF_line_type        = 6;
  DXF_text_style       = 7;
  DXF_layer_name       = 8;
  DXF_var_name         = 9;
  DXF_primary_X        = 10;  DXF_primary_Y     = 20;
  DXF_primary_Z        = 30;
  DXF_other_X_1        = 11;  DXF_other_Y_1     = 21;
  DXF_other_Z_1        = 31;
  DXF_other_X_2        = 12;  DXF_other_Y_2     = 22;
  DXF_other_Z_2        = 32;
  DXF_other_X_3        = 13;  DXF_other_Y_3     = 23;
  DXF_other_Z_3        = 33;
  DXF_elevation        = 38;
  DXF_thickness        = 39;
  DXF_floatval         = 40;
  DXF_floatvals1       = 41;
  DXF_floatvals2       = 42;
  DXF_floatvals3       = 43;
  DXF_repeat           = 49;
  DXF_angle1           = 50;  DXF_angle2        = 51;
  DXF_angle3           = 52;  DXF_angle4        = 53;
  DXF_angle5           = 54;  DXF_angle6        = 55;
  DXF_angle7           = 56;  DXF_angle8        = 57;
  DXF_angle9           = 58;
  DXF_visible          = 60;
  DXF_colornum         = 62;
  DXF_entities_flg     = 66;
  DXF_ent_ident        = 67;
  DXF_view_state       = 69;
  DXF_70Flag           = 70;
  DXF_71Flag           = 71;  DXF_72Flag        = 72;
  DXF_73Flag           = 73;  DXF_74Flag        = 74;
  DXF_extrusionx       = 210;
  DXF_extrusiony       = 220;
  DXF_extrusionz       = 230;
  DXF_comment          = 999;

///////////////////////////////////////////////////////////////////////////////
// DXF_Reader class definition
///////////////////////////////////////////////////////////////////////////////
Const
  MaxSizeOfBuf = 4096;

type
  tCharArray = array [0..MaxSizeOfBuf-1] of AnsiChar;

  abstract_entity = class;

  DXF_Reader = class
  private
    // used when reading data from the file
    IO_chan     : file;
    SizeOfBuf   : integer;
    num_in_buf  : integer;
    ii          : integer;
    EC,fCode    : integer;
    pBuf        : ^tCharArray;
    Line_num    : longint;
    fLine       : AnsiString;
    FProgress    : TProgressBar;
    // useful bits to make parsing easier...
    file_pos   : integer;
    marked_pos : integer;
    backflag   : boolean;
    FOnThinking: TOnThinking;
    FOnStoppedThinking: TNotifyEvent;
    TempInsertList: TList;
    ReadingBlocks: boolean;
    procedure   go_back_to_last(code:integer; str:AnsiString);
    procedure   mark_position;
    procedure   goto_marked_position;
    //
//    procedure   go_back_to_start;
    function    NextGroupCode: integer;
    function    ValStr: AnsiString;
    function    ValDbl: double;
    function    ValInt: integer;
    function    code_and_string(var group:integer; var s:AnsiString) : boolean;
    function    code_and_double(var group:integer; var d:double) : boolean;
    function    read_2Dpoint(var p1:Point3D)                     : boolean;
    function    skip_upto_section(name:AnsiString)                   : boolean;
                // lowest level read function
    function    read_entity_data(ent:abstract_entity)            : boolean;
    function    read_generic(var layer:integer)                  : abstract_entity;
                // we can read most entities with this one
    function    general_purpose_read(obj_type:TClass; var entity:DXF_Entity; var layer:integer) : boolean;
                // inserts/polylines need a little more complexity
    function    read_insert(var entity:DXF_Entity; var layer:integer)   : boolean;
    function    read_polyline(var entity:DXF_Entity; var layer:integer) : boolean;
                // this calls the others above
    function    read_entity(s,endstr:AnsiString; var entity:DXF_Entity; var layer:integer) : boolean;
    procedure Thinking(message: String);
    procedure Stopped_Thinking;
    procedure SetProgress(const Value: TProgressBar);
  public
    // Extents in (x,y) of the dataset
    min_extents    : Point3D;
    max_extents    : Point3D;
    // We will read the Entities in the layers into this list
    DXF_Layers: TDXF_LayerList;
    colour_BYLAYER : boolean;
    skipped        : TStrings;
    // Constructors and destructors
    Constructor Create (const aName: string);
    Destructor  Destroy;                           override;
    // Header section
    function    move_to_header_section : boolean;
    function    read_header            : boolean;
    function    get_min_extent         : Point3D;
    function    get_max_extent         : Point3D;
    // Blocks section
    function    move_to_blocks_section : boolean;
    function    read_blocks            : boolean;
    function    read_block             : boolean;
    function    block_list             : Entity_List;
    // Tables section
    function    move_to_tables_section : boolean;
    function    read_tables : boolean;
    function    read_layer_information : boolean;
    function    read_vport_information : boolean;
    function    layer_num(layername:AnsiString) : integer;
    // Entities section
    function    move_to_entity_section : boolean;
    function    read_entities          : boolean;
    // These are the main routines to use
    function    read_file                 : boolean;
    function    remove_empty_layers       : boolean;
    function    release_control_of_layers : TDXF_LayerList;
    procedure   set_skipped_list(s:TStrings);
    // new
    property OnThinking: TOnThinking read FOnThinking write FOnThinking;
    property OnStoppedThinking: TNotifyEvent read FOnStoppedThinking write FOnStoppedThinking;
    property Progress : TProgressBar read FProgress write SetProgress;

  end;

///////////////////////////////////////////////////////////////////////////////
// This is a simple class used only during file reads, it should not be used
// as a base for any objects.
// It is to allow all entities to be read using the same basic structure
// even though they all use different group codes
// Add extra group codes if you need to recognize them
///////////////////////////////////////////////////////////////////////////////
  abstract_entity = class
    p1,p2,p3,p4                             : Point3D;
    rad_hgt                                 : double;
    angle1,angle2                           : double;
    fv1,fv2,fv3                             : double;
    thickness                               : double;
    colour                                  : integer;
    flag_70,flag_71,flag_72,flag_73,flag_74 : integer;
    attflag                                 : integer;
    namestr,tagstr,promptstr                : AnsiString;
    layer                                   : AnsiString;
    elev                                    : double;
    OCS_Z                                   : Point3D;
    procedure clear;
  end;
///////////////////////////////////////////////////////////////////////////////
// DXF file read exceptions will be this type
///////////////////////////////////////////////////////////////////////////////
type
  DXF_read_exception = class(Exception)
    line_number : integer;
    constructor create(err_msg:String; line:integer);
  end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// implementation
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
implementation

//////////

resourcestring
  StrInvalidFloatingPoi = 'Invalid Floating point conversion';
  StrInvalidIntegerConv = 'Invalid Integer conversion';
  StrCannotReferenceAn = 'Cannot reference an undefined BLOCK';
  StrUserAborted = 'User aborted';
  StrDXFReadErrorWar = ': DXF read error warning.  Press OK to ignore error.';
  StrReadingDXFFile = 'Reading DXF file...';
  StrPolylineContainedM = 'Polyline contained more than %d vertices';
  StrErrorReadingPOINT = 'Error reading POINT entity';
  StrErrorReadingINSERT = 'Error reading INSERT entity';
  StrErrorReadingTEXTE = 'Error reading TEXT entity';
  StrErrorReadingLINEE = 'Error reading LINE entity';
  StrErrorReadingPOLYLI = 'Error reading POLYLINE entity';
  StrErrorReading3DFACE = 'Error reading 3DFACE entity';
  StrErrorReadingSOLID = 'Error reading SOLID entity';
  StrErrorReadingARCEn = 'Error reading ARC entity';
  StrErrorReadingCIRCLE = 'Error reading CIRCLE entity';
  StrErrorReadingATTDEF = 'Error reading ATTDEF entity';
  StrErrorReadingATTRIB = 'Error reading ATTRIB entity';
  StrNoEntitiesOrInval = 'No Entities or invalid Entities section in DXF fil' +
  'e';
  StrNoBlocksOrInvalid = 'No Blocks or invalid Blocks section in DXF file';
  StrNoHeaderOrInvalid = 'No Header or invalid Header section in DXF file';
  StrNoLayersOrInvalid = 'No Layers or invalid Tables section in DXF file';
  StrFileContainedNoMa = 'File contained no Max/Min extents. Scanning...';
  StrDXFReadError = ': DXF Read Error';


/////////////////////////////////////////////////////////////////////
// abstract_entity implementation
// used when reading vertexes - just to make sure all flags are reset
// quicker than using create/destroy for each vertex.
///////////////////////////////////////////////////////////////////////////////
procedure abstract_entity.clear;
begin
  InitInstance(self);
end;
///////////////////////////////////////////////////////////////////////////////
// DXFReader implementation
///////////////////////////////////////////////////////////////////////////////
Constructor DXF_Reader.Create (const aName: string);
begin
  Inherited Create;
  TempInsertList:= TList.Create;
  AssignFile(IO_chan,aName);
  Reset(IO_chan,1);
  SizeOfBuf         := MaxSizeOfBuf;
  GetMem(pBuf,SizeOfBuf);
  DXF_Layers        := TDXF_LayerList.Create;
  colour_BYLAYER    := false;
  Line_num          := 0;
  backflag          := false;
//  progress          := Thinking_box.bar;
  progress := nil;
//  progress.position := 0;
//  progress.max      := FileSize(IO_chan) div MaxSizeOfBuf;
  min_extents       := origin3D;
  max_extents       := origin3D;
end;

destructor DXF_Reader.Destroy;
//var lp1 : integer;
begin
{  if (DXF_Layers<>nil) then
    for lp1 := 0 to DXF_Layers.count-1 do DXF_Layer(DXF_Layers[lp1]).Free;}
  DXF_Layers.Free;
  try
    CloseFile(IO_chan);
  except on E: EInOutError do
    // ErrorCode = 103 means the file was not opened.
    if E.ErrorCode <> 103 then
    begin
      raise
    end;
  end;
  if Assigned(pBuf) then
  begin
    FreeMem(pBuf,SizeOfBuf);
  end;
  TempInsertList.Free;
  Inherited Destroy;
end;
{ --------------------------------------------------------------------------- }
{ Routines for fetching codes and values
{ --------------------------------------------------------------------------- }
{procedure DXF_Reader.go_back_to_start;
begin
  Reset(IO_chan,1);
  num_in_buf := 0;
  ii         := 0;
end; }

procedure DXF_Reader.go_back_to_last(code:integer; str:AnsiString);
begin
  fCode    := code;
  fLine    := str;
  backflag := true;
end;

procedure DXF_Reader.mark_position;
begin
  marked_pos := File_pos + ii;
end;

procedure DXF_Reader.goto_marked_position;
begin
  Seek(IO_chan,marked_pos);
  File_pos := marked_pos;
  num_in_buf := 0;
  ii         := 0;
end;

function DXF_Reader.NextGroupCode: integer;
var
  Local_fLine: shortString;
  function GotMore: boolean;
  begin
    file_pos := FilePos(IO_chan);
    BlockRead(IO_chan,pBuf^,SizeOfBuf,num_in_buf); ec:=IoResult; ii:=0;
    If (ec=0) and (num_in_buf=0) then ec:=-1; GotMore:=(ec=0);
    if progress <> nil then progress.position := progress.position+1;
  end{GotMore};

  // Sometimes you get (download) a bad DXF file which has a couple of blank
  // lines in it. The commented retry code, can be used to skip blank lines, but you
  // should only use it as an emergency fix because you'll often find blank lines
  // in TEXT entities and other text strings.
  function GotLine: boolean;
  const CR=#13; LF=#10;
  var c: AnsiChar;
//    ReachedLineEnd: Boolean;
//  label retry;
  begin
//  retry:
    fLine := '';
//    ReachedLineEnd := False;
//    repeat
//    begin
      byte(Local_fLine[0]):=0;
      c := AnsiChar(0);
      While (ii<num_in_buf) or GotMore do begin
        c:=pBuf^[ii]; inc(ii);
        If (c<>CR) and (c<>LF) and (length(Local_fLine)<255) then
        begin
          inc(Local_fLine[0]);
          Local_fLine[length(Local_fLine)]:=c;
        end
        else begin      // Extra code added to handle C/Unix style LF not CR/LF
          if (c=CR) then
          begin
            if (ii<num_in_buf) or GotMore then
            begin
              if pBuf^[ii]=LF then
              begin
                inc(ii);
//                ReachedLineEnd := True;
                break;
              end;
            end;
          end
          else if (c=LF) then
          begin
//            ReachedLineEnd := True;
            break;
          end
          else if (length(Local_fLine) = 255) then
          begin
            fLine := fLine + Local_fLine;
            Local_fLine := '';
          end;
        end
      end;
      fLine := fLine + Local_fLine;
      Local_fLine := '';
//    end;
//    until ReachedLineEnd;
    GotLine:=(ec=0) and ((c=CR) or (c=LF));
    inc(Line_num);
//    if Local_fLine='' then goto retry;
  end;

begin {NextGroupCode}
  if backflag then begin
    result   := fCode;
    backflag := false;
  end
  else begin
    repeat
      if not GotLine then begin
        fCode:=-2;
        Result:=fCode;
        exit;
      end;
    until fLine<>'';
    Val(string(fLine),fCode,ec);
    If ec<>0 then fCode:=-2
    else if not GotLine then fCode:=-2;
    Result:=fCode;
  end;
end {NextGroupCode};

function DXF_Reader.ValStr: AnsiString;
begin Result:=fLine end;

function DXF_Reader.ValDbl: double;
begin
  Val(string(fLine),Result,ec);
  If ec<>0 then raise DXF_read_exception.Create(StrInvalidFloatingPoi,line_num);
end;

function DXF_Reader.ValInt: integer;
begin
  Val(string(fLine),Result,ec);
  If ec<>0 then raise DXF_read_exception.Create(StrInvalidIntegerConv,line_num);
end;

function DXF_Reader.code_and_string(var group:integer; var s:AnsiString) : boolean;
//var astr : AnsiString;
begin
  result := true;
  group  := NextGroupCode;
  if group>=0 then s := ValStr
  else result := false;
  // useful in debugging
  //  if (group=0) then begin astr := IntToStr(group)+' '+s; alb.Items.Add(astr); end;
end;

function DXF_Reader.code_and_double(var group:integer; var d:double) : boolean;
begin
  result := true;
  group  := NextGroupCode;
  if group>=0 then d := Valdbl
  else result := false;
end;

// This routine is just for the $EXT(max/min) and should be used with care....
function DXF_Reader.read_2Dpoint(var p1:Point3D) : boolean;
var Groupcode : integer;
begin
  repeat Groupcode:=NextGroupCode;
  until (Groupcode=DXF_primary_X) or (Groupcode<0);
  if Groupcode<0 then begin result:=false; exit; end;
  p1.x := Valdbl;
  result := code_and_double(Groupcode,p1.y);  { y next              }
end;

function DXF_Reader.skip_upto_section(name:AnsiString) : boolean;
var Group   : integer;
    s       : AnsiString;
begin
  result := false;
  repeat
    if not code_and_string(Group,s) then break;
    if (Group=0) then begin
      if (s='SECTION') then begin
        if not code_and_string(Group,s) then break;
        if (Group=DXF_name) then begin
          if (s=name) then result := true
//          else exit;
        end
        else if skipped<>nil then Skipped.Add(string(s));
      end else if skipped<>nil then Skipped.Add(string(s));
    end;
  until (result);
end;
{ --------------------------------------------------------------------------- }
{ Header section
{ --------------------------------------------------------------------------- }
function DXF_Reader.move_to_header_section : boolean;
begin
  result := skip_upto_section('HEADER');
end;

function DXF_Reader.read_header : boolean;
var Group : integer;
    s     : AnsiString;
begin
  result := false;
  repeat
    if not code_and_string(Group,s) then break;
    if (group=9) and (s='$EXTMAX') then begin
      if not read_2Dpoint(max_extents) then break;
    end;
    if (group=9) and (s='$EXTMIN') then begin
      if not read_2Dpoint(min_extents) then break;
    end;
    if (group=9) and (s='$CECOLOR') then begin
      if (NextGroupCode=DXF_colornum) and (ValInt=256) then colour_BYLAYER := true;
    end;
    result := (Group=0) and (s='ENDSEC');
  until result;
end;

function DXF_Reader.get_min_extent : Point3D;
begin
  result := min_extents;
end;

function DXF_Reader.get_max_extent : Point3D;
begin
  result := max_extents;
end;
{ --------------------------------------------------------------------------- }
{ Blocks section
{ --------------------------------------------------------------------------- }
function DXF_Reader.move_to_blocks_section : boolean;
begin
  result := skip_upto_section('BLOCKS');
end;

function DXF_Reader.read_blocks : boolean;
var Group : integer;
    s     : AnsiString;
    Index: integer;
    Insert: Insert_;
begin
  result := false;
  ReadingBlocks := True;
  repeat
    if not code_and_string(Group,s) then break;
    if (Group=0) and (s='BLOCK') then begin
      if not read_block then break;
    end;
    result := (Group=0) and (s='ENDSEC');
  until result;
  ReadingBlocks := False;
  try
  for Index := 0 to TempInsertList.Count -1 do
  begin
    Insert := TempInsertList[Index];
      try
        Insert.update_block_links(block_list);
      except
        raise DXF_read_exception.Create(StrCannotReferenceAn, -1);
      end;
  end;
  finally
    TempInsertList.Clear;
  end;

end;

function DXF_Reader.read_block : boolean;
var Groupcode  : integer;
    s          : AnsiString;
    ent        : abstract_entity;
    block      : Block_;
    layer      : integer;
    entity     : DXF_Entity;
//    base       : Point3D;
begin
  result := false;
  ent := read_generic(layer);
  try
    layer := layer_num('0'); // ALL BLOCKS GOING TO LAYER 0 (makes things easier)
    if layer<0 then layer := DXF_Layers.Add(DXF_Layer.create('0'));
    if ent<>nil then begin
      block := Block_.create(ent.namestr,ent.p1);
      DXF_Layer(DXF_Layers[layer]).add_entity_to_layer(block);
      repeat
        if not code_and_string(Groupcode,s) then break;
        if (Groupcode=0) then begin
          result := read_entity(s,'ENDBLK',entity,layer);
          if entity<>nil then block.entities.Add(entity);
        end;
      until result;
    end;
  finally
    ent.Free;
  end;
end;

// we need to know where the blocks are stored for lookup purposes
function DXF_Reader.block_list : Entity_List;
var lp1,lp2 : integer;
    layer   : DXF_Layer;
begin
  result := nil;
  for lp1:=0 to DXF_Layers.count -1 do begin
    layer := DXF_Layers[lp1];
    for lp2:=0 to layer.entity_lists.count-1 do begin
      if Entity_List(layer.entity_lists[lp2]).name='Block_' then begin
        result := Entity_List(layer.entity_lists[lp2]);
        exit;
      end;
    end;
  end;
end;
{ --------------------------------------------------------------------------- }
{ Tables (Layers - VPort) section
{ --------------------------------------------------------------------------- }
function DXF_Reader.move_to_tables_section : boolean;
begin
  result := skip_upto_section('TABLES');
end;

function DXF_Reader.read_tables : boolean;
var Group : integer;
    s     : AnsiString;
begin
  result := false;
  repeat
    if not code_and_string(Group,s) then break;
    if (Group=0) and (s='TABLE') then begin
      if not code_and_string(Group,s) then break;
      if (Group=DXF_name) then begin
        if (s='LAYER') then read_layer_information
        else if (s='VPORT') then read_vport_information
        else if skipped<>nil then Skipped.Add(string(s));
      end;
    end;
    result := (Group=0) and (s='ENDSEC');
  until result;
end;

function DXF_Reader.read_layer_information : boolean;
var Group,Lay_num : integer;
    s             : AnsiString;
begin
  lay_num := -1;
  result  := false;
  repeat
    if not code_and_string(Group,s) then break;
    if (Group=0) then begin
      if (s='LAYER') then begin
        if not code_and_string(Group,s) then break;
        if (Group=DXF_name) then lay_num := DXF_Layers.Add(DXF_Layer.create(s));
      end
      else if (s='ENDTAB') then result := true
      else if skipped<>nil then Skipped.Add(string(s));
    end
    else if (Group=DXF_colornum) and (lay_num<>-1) then
      DXF_Layer(DXF_Layers[lay_num]).Colour := ValInt;
  until result;
end;

// This no longer does anything !
function DXF_Reader.read_vport_information : boolean;
var Group : integer;
    s     : AnsiString;
begin
  result := false;
  repeat
    if not code_and_string(Group,s) then break;
    if (Group=0) then begin
      if (s='VPORT') then
      begin
        repeat
        if not code_and_string(Group,s) then break;
        if (Group=DXF_name) then
        begin
          if (UpperCase(string(s))='*ACTIVE') then
          begin
            repeat
              if not code_and_string(Group,s) then break;
  { removed Aspectratio stuff since it never seems to make any difference
    and sometimes buggers everything up
              if (Group=DXF_floatvals1) then Aspect := ValDbl;
  }
              result := (Group=0) and (s='ENDTAB');
            until (result)
          end
          else if skipped<>nil then Skipped.Add(string(s));
        end;
        until result;
      end
      else if skipped<>nil then Skipped.Add(string(s));
    end
  until (result);
end;

function DXF_Reader.layer_num(layername:AnsiString) : integer;
var lp1 : integer;
begin
  result := -1;
  for lp1:=0 to DXF_Layers.count-1 do begin
    if DXF_Layer(DXF_Layers[lp1]).name=layername then begin
      result := lp1;
      exit;
    end;
  end;
end;
{ --------------------------------------------------------------------------- }
{ Entities section
{ --------------------------------------------------------------------------- }
function DXF_Reader.move_to_entity_section : boolean;
begin
  result := skip_upto_section('ENTITIES');
end;

function DXF_Reader.read_entities : boolean;
var Groupcode,layer : integer;
    s               : AnsiString;
    entity          : DXF_Entity;
begin
  result := false;
  repeat
    try
      if not code_and_string(Groupcode,s) then break;
      if (Groupcode=0) then begin
        result := read_entity(s,'ENDSEC',entity,layer);
        // put the entity in the layer...
        if entity<>nil then DXF_Layer(DXF_Layers[layer]).add_entity_to_layer(entity);
      end;
    except
      on E:DXF_read_exception do begin
        stopped_thinking;
        if MessageDlg(E.message + StrDXFReadErrorWar,
          mtError, [mbOK, mbCancel], 0) = mrCancel then
          raise DXF_read_exception.Create(StrUserAborted,-1);
        thinking(StrReadingDXFFile);
      end;
      on E:Exception do Showmessage(E.Message);
    end;
  until result;
end;
{ --------------------------------------------------------------------------- }
{ Entity reading code
{ --------------------------------------------------------------------------- }
function DXF_Reader.read_entity_data(ent:abstract_entity) : boolean;
var Groupcode : integer;
begin
  ent.OCS_Z := WCS_Z;
  repeat
    Groupcode := NextGroupCode;
    case Groupcode of
      DXF_primary_X    : ent.p1.x      := Valdbl;
      DXF_primary_Y    : ent.p1.y      := Valdbl;
      DXF_primary_Z    : ent.p1.z      := Valdbl;
      DXF_other_X_1    : ent.p2.x      := Valdbl;
      DXF_other_Y_1    : ent.p2.y      := Valdbl;
      DXF_other_Z_1    : ent.p2.z      := Valdbl;
      DXF_other_X_2    : ent.p3.x      := Valdbl;
      DXF_other_Y_2    : ent.p3.y      := Valdbl;
      DXF_other_Z_2    : ent.p3.z      := Valdbl;
      DXF_other_X_3    : ent.p4.x      := Valdbl;
      DXF_other_Y_3    : ent.p4.y      := Valdbl;
      DXF_other_Z_3    : ent.p4.z      := Valdbl;
      DXF_floatval     : ent.rad_hgt   := Valdbl;
      DXF_floatvals1   : ent.fv1       := Valdbl;
      DXF_floatvals2   : ent.fv2       := Valdbl;
      DXF_floatvals3   : ent.fv3       := Valdbl;
      DXF_angle1       : ent.angle1    := Valdbl;
      DXF_angle2       : ent.angle2    := Valdbl;
      DXF_thickness    : ent.thickness := Valdbl;
      DXF_elevation    : ent.elev      := Valdbl;
      DXF_70Flag       : ent.flag_70   := ValInt;
      DXF_71Flag       : ent.flag_71   := ValInt;
      DXF_72Flag       : ent.flag_72   := ValInt;
      DXF_73Flag       : ent.flag_73   := ValInt;
      DXF_74Flag       : ent.flag_74   := ValInt;
      DXF_colornum     : ent.colour    := ValInt;
      DXF_entities_flg : ent.attflag   := ValInt;
      DXF_layer_name   : ent.layer     := ValStr;
      DXF_name         : ent.namestr   := ValStr;
      DXF_text_def     : ent.tagstr    := ValStr;
      DXF_text_prompt  : ent.promptstr := ValStr;
      DXF_extrusionx   : ent.OCS_Z.x   := Valdbl;
      DXF_extrusiony   : ent.OCS_Z.y   := Valdbl;
      DXF_extrusionz   : ent.OCS_Z.z   := Valdbl;
    end;
  until (Groupcode<=0); // end or fault;
  if Groupcode<0 then begin result:=false; exit; end;
  // we need to put the code=0, and valstr back, so the next entity starts
  // with the zero when necessary
  go_back_to_last(Groupcode,fline);
  ent.OCS_Z := normalize(ent.OCS_Z); // for safety
  result := true;
end;

function DXF_Reader.read_generic(var layer:integer) : abstract_entity;
var ent : abstract_entity;
//    s   : AnsiString;
begin
  result := nil;
  ent    := abstract_entity.create; // set everything to zero EVERY time
  if read_entity_data(ent) then begin
    layer := layer_num(ent.layer);
    if layer<0 then layer := DXF_Layers.Add(DXF_Layer.create(ent.layer));
    result := ent;
  end else ent.free;
end;

{ These ones are straightforward, so we'll use a crafty TClass parameter }
function DXF_Reader.general_purpose_read(obj_type:TClass; var entity:DXF_Entity; var layer:integer) : boolean;
var
  ent   : abstract_entity;
  points: pointlist;
begin
  result := False;
  entity := nil;
  ent := read_generic(layer);
  if ent<>nil then begin
    with ent do begin
      if      obj_type = Point_  then entity := Point_. create(OCS_Z,p1,colour)
      else if obj_type = Text_   then entity := Text_.  create(OCS_Z,p1,p2,tagstr,rad_hgt,colour,flag_72)
      else if obj_type = Line_   then entity := Line_.  create(p1,p2,colour)
      else if obj_type = Circle_ then entity := Circle_.create(OCS_Z,p1,rad_hgt,colour)
      else if obj_type = Arc_    then entity := Arc_.   create(OCS_Z,p1,rad_hgt,angle1,angle2,colour)
      // face3ds and solids can have 3 or 4 points, if 4=3, then 3 used
      else if obj_type = Face3D_ then begin
        SetLength(points, 4);
        points[0] := p1;
        points[1] := p2;
        points[2] := p3;
        points[3] := p4;
        if p1_eq_p2_3d(p3,p4) then entity := Face3D_.create(p1, 3, points, colour,true)
        else                       entity := Face3D_.create(p1, 4, points, colour,true)
      end
      else if obj_type = Solid_ then begin
        SetLength(points, 4);
        points[0] := p1;
        points[1] := p2;
        points[2] := p3;
        points[3] := p4;
        if p1_eq_p2_3d(p3,p4) then entity := Solid_.create(p1, OCS_Z,3, points, colour,thickness)
        else                       entity := Solid_.create(p1, OCS_Z,4, points, colour,thickness)
      end
      else if obj_type = Attdef_  then entity := Attdef_.create(OCS_Z,p1,p2,namestr,tagstr,promptstr,flag_70,flag_72,rad_hgt,colour)
      else if obj_type = Attrib_  then entity := Attrib_.create(OCS_Z,p1,p2,namestr,tagstr,flag_70,flag_72,rad_hgt,colour);
    end;
    ent.Free;
    result := true;
  end;
end;

{ INSERTs may have ATTRIBs + BLOCKs which makes it a little more complicated }
function DXF_Reader.read_insert(var entity:DXF_Entity; var layer:integer) : boolean;
var ent,ent2 : abstract_entity;
    {code,}num : integer;
    atts     : att_array;
begin
  result := true;
  SetLength(atts, 255);
  entity := nil;
  num := 0;
  ent := read_generic(layer);
  if ent<>nil then begin
    if ent.attflag=1 then begin
      repeat
        result := (Nextgroupcode=0);
        if result and (ValStr='ATTRIB') then begin
          ent2 := read_generic(layer);
          if ent2<>nil then with ent2 do begin
            atts[num] := Attrib_.create(OCS_Z,p1,p2,namestr,tagstr,flag_70,flag_72,rad_hgt,colour);
            ent2.Free;
            inc(num);
          end else result := false;
        end;
      until (not result) or (ValStr='SEQEND');
      if result then Nextgroupcode; // remove the SEQEND put back
    end;
    with ent do begin
      if fv1=0 then fv1 := 1;
      if fv2=0 then fv2 := 1;
      if fv3=0 then fv3 := 1;
      entity := Insert_.create(OCS_Z,p1,aPoint3D(fv1,fv2,fv3),angle1,colour,num,atts,namestr);
      if ReadingBlocks then
      begin
        TempInsertList.Add(entity);
      end
      else
      begin
        try
          Insert_(entity).update_block_links(block_list);
        except
          entity.Free;
          entity := nil;

          raise DXF_read_exception.Create(StrCannotReferenceAn+StrEOL+StrEOL+
          '(File may not have been saved with BLOCKs)'+ StrEOL,line_num);
        end;
      end;
    end;
    ent.Free;
  end
  else result := false;
end;

// POLYLINEs have variable number of points...
// Modified to accept polyface mesh variety of polyline ...
//   I've ignored the invisible flag for edges
// Modified to accept polygon MxN grid mesh ...
// It's a bit messy - you could simplify it a bit - but hey - what do you
// expect from free code.
function DXF_Reader.read_polyline(var entity:DXF_Entity; var layer:integer) : boolean;
var ent1,ent2, ent3    : abstract_entity;
    vertices{,lp1} : integer;
    faces        : integer;
    tempvert     : pointlist;//array[0..max_vertices_per_polyline-1] of Point3D;
    tempface     : Tfacelist;
    closed_poly  : boolean;
    M,N,mn       : integer;
label vertex_overflow;
begin
  SetLength(tempvert,max_vertices_per_polyline);
  SetLength(tempface,4096);
  result := false; closed_poly := false; entity := nil;
  ent1   := abstract_entity.create;
  // read initial polyline data
  if not read_entity_data(ent1) then begin ent1.Free; exit; end;
  layer    := layer_num(ent1.layer);
  if (layer=-1) then layer := DXF_Layers.Add(DXF_Layer.create(ent1.layer));
  vertices := 0; faces := 0;
  ent2     := abstract_entity.create;
  //////////////////////////////////////////
  //////////////////////////////////////////
  if (ent1.flag_70 and (64+16))=0 then begin
    // THIS IS A NORMAL POLYLINE
    repeat
      if (NextGroupCode=0) and (ValStr = 'VERTEX') then
      begin
        ent3 := abstract_entity.Create;
        try
          if read_entity_data(ent3) then begin
            tempvert[vertices] := ent3.p1; inc(vertices);
            if vertices>=max_vertices_per_polyline then
            begin
              ent3.Free;
              ent3 := nil;
              ent1.Free; ent2.Free;
              raise DXF_read_exception.Create(Format(StrPolylineContainedM,
                [max_vertices_per_polyline]),line_num);
            end;
          end
          else
          begin
            ent1.Free;
            ent2.Free;
            exit;
          end; // error
        finally
          ent3.Free;
        end;
      end;
    until fLine='SEQEND';
    // this should set result to true, because 0 SEQEND is next
    result := NextGroupCode=0;
    if ((ent1.flag_70) and 1)=1 then closed_poly := true;
    entity := Polyline_.create(ent1.p1, ent1.OCS_Z,vertices,tempvert,ent1.colour,closed_poly);
  end
  //////////////////////////////////////////
  //////////////////////////////////////////
  else if (ent1.flag_70 and 16)=16 then begin
    // THIS IS A POLYGON MESH - a grid of vertices joined along M & N
    M := ent1.flag_71; N := ent1.flag_72; mn := 0;
    repeat
      if (NextGroupCode=0) and (ValStr = 'VERTEX') then begin
        if read_entity_data(ent2) then begin
          inc(mn);
          if (ent2.Flag_70 and 64)=64 then begin
            tempvert[vertices] := ent2.p1; inc(vertices);
            if vertices>=max_vertices_per_polyline then goto vertex_overflow;
          end else begin ent1.Free; ent2.Free; exit; end; // error
        end else begin ent1.Free; ent2.Free; exit; end; // error
      end;
    until fLine='SEQEND';
    result := NextGroupCode=0;
    if mn<>M*N then begin ent1.Free; ent2.Free; exit; end; // error
    entity := Polygon_mesh_.create(ent1.p1, vertices,M,N,tempvert,ent1.flag_70,ent1.colour);
  end
  //////////////////////////////////////////
  //////////////////////////////////////////
  else if (ent1.flag_70 and 64)=64 then begin
    // THIS IS A POLYFACE MESH - a vertex array with facets
    repeat
      if (NextGroupCode=0) and (ValStr = 'VERTEX') then begin
        if read_entity_data(ent2) then begin
          if (ent2.Flag_70 and (128+64))=(128+64) then begin
            // this is a normal coordinate vertex
            tempvert[vertices] := ent2.p1; inc(vertices);
            if vertices>=max_vertices_per_polyline then goto vertex_overflow;
          end else if (ent2.Flag_70 and (128))=(128) then begin
            // this is a face definition vertex
            // negative indices indicate invisible edges (ignored for now)
            tempface[faces].nf[0] := Abs(ent2.flag_71)-1; // index 1..n -> 0..n-1
            tempface[faces].nf[1] := Abs(ent2.flag_72)-1;
            tempface[faces].nf[2] := Abs(ent2.flag_73)-1;
            tempface[faces].nf[3] := Abs(ent2.flag_74)-1;
            inc(faces);
          end else begin ent1.Free; ent2.Free; exit; end; // error
        end else begin ent1.Free; ent2.Free; exit; end; // error
      end;
    until fLine='SEQEND';
    result := NextGroupCode=0;
    entity := Polyface_mesh_.create(ent1.p1, vertices,faces,tempvert,tempface,ent1.colour);
  end;
  //////////////////////////////////////////
  //////////////////////////////////////////
  ent1.Free; ent2.Free;
  exit; // next bit only when vertices overflow
vertex_overflow:
  ent1.Free; ent2.Free;
  raise DXF_read_exception.Create(Format(StrPolylineContainedM,
    [max_vertices_per_polyline]),line_num);
end;

function DXF_Reader.read_entity(s,endstr:AnsiString; var entity:DXF_Entity; var layer:integer) : boolean;
begin
  entity := nil; result := false;
  if (s='POINT') then begin if not general_purpose_read(Point_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingPOINT,line_num); end
  else if (s='INSERT') then begin if not read_insert(entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingINSERT,line_num); end
  else if (s='TEXT') then begin if not general_purpose_read(Text_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingTEXTE,line_num); end
  else if (s='LINE') then begin if not general_purpose_read(Line_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingLINEE,line_num); end
  else if (s='POLYLINE') then begin if not read_polyline(entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingPOLYLI,line_num); end
  else if (s='3DFACE') then begin if not general_purpose_read(Face3D_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReading3DFACE,line_num); end
  else if (s='SOLID') then begin if not general_purpose_read(Solid_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingSOLID,line_num); end
  else if (s='CIRCLE') then begin if not general_purpose_read(Circle_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingCIRCLE,line_num); end
  else if (s='ARC') then begin if not general_purpose_read(Arc_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingARCEn,line_num); end
  else if (s='ATTDEF') then begin if not general_purpose_read(AttDef_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingATTDEF,line_num); end
  else if (s='ATTRIB') then begin if not general_purpose_read(Attrib_,entity,layer) then
    raise DXF_read_exception.Create(StrErrorReadingATTRIB,line_num); end
  else if (s=endstr) then result := true
  else if skipped<>nil then Skipped.Add(string(s));
end;
///////////////////////////////////////////////////////////////////////////////
// Main routines to use
///////////////////////////////////////////////////////////////////////////////
function DXF_Reader.read_file : boolean;
var lp1 : integer;
begin
  result := true;
  thinking(StrReadingDXFFile);
  try
    mark_position;
    if not (move_to_header_section and read_header) then begin
      Thinking(StrNoHeaderOrInvalid);
      Sleep(message_delay_ms);
      goto_marked_position;
    end;
    mark_position;
    if not (move_to_tables_section and read_tables) then begin
      Thinking(StrNoLayersOrInvalid);
      Sleep(message_delay_ms);
      goto_marked_position;
    end;
    mark_position;
    if not (move_to_blocks_section and read_blocks) then begin
      Thinking(StrNoBlocksOrInvalid);
      Sleep(message_delay_ms);
      goto_marked_position;
    end;
    mark_position;
    thinking(StrReadingDXFFile);
    if not (move_to_entity_section and read_entities) then
      raise DXF_read_exception.Create(StrNoEntitiesOrInval,-1);
  except
    on E:DXF_read_exception do begin
      stopped_thinking;
      MessageDlg(E.message + StrDXFReadError, mtWarning, [mbOK], 0);
    end;
    on E:EAccessViolation do begin
      stopped_thinking;
      MessageDlg(E.message, mtWarning, [mbOK], 0);
    end;
  end;
  if p1_eq_p2_3D(min_extents,origin3D) or p1_eq_p2_3D(max_extents,origin3D) then begin
    thinking(StrFileContainedNoMa);
    sleep(message_delay_ms); // just a delay to let the message be visible
    for lp1:=0 to DXF_layers.count-1 do
      DXF_Layer(DXF_Layers[lp1]).max_min_extents(max_extents,min_extents);
  end;
  stopped_thinking;
end;

function DXF_Reader.remove_empty_layers : boolean;
var lp1   : integer;
    layer : DXF_layer;
begin
   for lp1 := DXF_Layers.count-1 downto 0 do begin
     layer :=  DXF_Layers[lp1];
     if layer.num_lists=0 then begin
       DXF_Layers.Remove(layer);
       layer.Free;
     end;
  end;
  result := (DXF_Layers.count>0);
end;

// Hand over ownership of the layers, the owner of the entity lists
// is now responsible for their destruction
function DXF_Reader.release_control_of_layers : TDXF_LayerList;
begin
  result     := DXF_Layers;
  DXF_Layers := nil;
end;

// Since we're not reading all groupcodes, we offer the chance
// to dump the main titles into a list so we can see what
// we've missed
procedure DXF_Reader.set_skipped_list(s:TStrings);
begin
  skipped := s;
end;
///////////////////////////////////////////////////////////////////////////////
// DXF File exception
///////////////////////////////////////////////////////////////////////////////
constructor DXF_read_exception.create(err_msg:String; line:integer);
begin
  if line>-1 then
    message := err_msg + #13#10 + 'Error occurred at or near line number ' + IntToStr(line)
  else message := err_msg;
end;

procedure DXF_Reader.Thinking(message: String);
begin
  if Assigned(FOnThinking) then
  begin
    FOnThinking(self, message);
  end;
end;

procedure DXF_Reader.Stopped_Thinking;
begin
  if Assigned(FOnStoppedThinking) then
  begin
    FOnStoppedThinking(self);
  end;
end;

procedure DXF_Reader.SetProgress(const Value: TProgressBar);
begin
  FProgress := Value;
  if Value <> nil then
  begin
    progress.position := 0;
    progress.max      := FileSize(IO_chan) div MaxSizeOfBuf;
  end;

end;

initialization
end.


