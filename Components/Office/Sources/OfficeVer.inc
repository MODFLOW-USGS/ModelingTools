{ License include }

{$DEFINE OFFICEVCL}

{$IFDEF VER93}  { Borland C++Builder 1.0 }
  {$DEFINE OVCL_CBUILDER}
  {$DEFINE OVCL_CB1}
{$ENDIF}

{$IFDEF VER100} { Borland Delphi 3.0 }
  {$DEFINE OVCL_D3}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE OVCL_CBUILDER}
  {$DEFINE OVCL_CB3}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER120} { Borland Delphi 4.0 }
  {$DEFINE OVCL_ACTIONLINK}
  {$DEFINE OVCL_D4}
{$ENDIF}

{$IFDEF VER130} { Borland Delphi 5.0 }
  {$DEFINE OVCL_ACTIONLINK}
  {$DEFINE OVCL_D5}
  {$DEFINE OVCL_CB5}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125} { Borland C++Builder 4.0 }
  {$DEFINE OVCL_ACTIONLINK}
  {$DEFINE OVCL_CBUILDER}
  {$DEFINE OVCL_CB4}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130} { Borland Delphi 5.0 }
  {$IFDEF BCB}
    {$DEFINE OVCL_CBUILDER}
    {$DEFINE OVCL_CB5}
  {$ENDIF}
  {$DEFINE OVCL_ACTIONLINK}
  {$DEFINE OVCL_D5}
{$ENDIF}

{$IFNDEF VER80}              { Delphi 1.0     }
 {$IFNDEF VER90}             { Delphi 2.0     }
  {$IFNDEF VER93}            { C++Builder 1.0 }
    {$DEFINE OVCL_D3}          { Delphi 3.0 or higher }
    {$IFNDEF VER100}
      {$DEFINE OVCL_CB3}      { C++Builder 3.0 or higher }
      {$IFNDEF VER110}
        {$DEFINE OVCL_D4}      { Delphi 4.0 or higher }
        {$IFNDEF VER120}
          {$DEFINE OVCL_CB4}  { C++Builder 4.0 or higher }
          {$IFNDEF VER125}
            {$DEFINE OVCL_D5}  { Delphi 5.0 or higher }
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE OVCL_COMPILER6}
  {$IFDEF BCB}
    {$DEFINE OVCL_CB6}
    {$DEFINE OVCL_CBUILDER}
  {$ELSE}
    {$DEFINE OVCL_D6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE OVCL_COMPILER6}
  {$IFDEF BCB}
    {$DEFINE OVCL_CB6}
    {$DEFINE OVCL_CBUILDER}
  {$ELSE}
    {$DEFINE OVCL_D6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE OVCL_COMPILER6}
  { $DEFINE OVCL_COMPILER23}
  {$IFDEF BCB}
    {$DEFINE OVCL_CB6}
    {$DEFINE OVCL_CBUILDER}
  {$ELSE}
    {$DEFINE OVCL_D6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE OVCL_COMPILER6}
  { $DEFINE OVCL_COMPILER23}
  {$IFDEF BCB}
    {$DEFINE OVCL_CB6}
    {$DEFINE OVCL_CBUILDER}
  {$ELSE}
    {$DEFINE OVCL_D6}
  {$ENDIF}
{$ENDIF}

{$IFDEF CompilerVersion}
{$IF CompilerVersion >= 23}
  {$DEFINE OVCL_COMPILER6}
  {$IFDEF BCB}
    {$DEFINE OVCL_CB6}
    {$DEFINE OVCL_CBUILDER}
  {$ELSE}
    {$DEFINE OVCL_D6}
  {$ENDIF}
{$IFEND}
{$ENDIF}

{$IFDEF OVCL_CBUILDER}
  {$IFNDEF VER93}
    {$DEFINE OVCL_CB3}    { C++Builder 3.0 or higher }
    {$IFNDEF VER110}
      {$DEFINE OVCL_CB4}  { C++Builder 4.0 or higher }
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ALIGN ON}
{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$WRITEABLECONST ON}

{ ------------ !! DO NOT CHANGE DEFINES BELOW THIS LINE !! ------------ }

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$IFDEF WIN32}
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}
