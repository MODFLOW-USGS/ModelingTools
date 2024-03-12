unit XBase1PropEd;

interface

{$IFDEF MSWINDOWS}
  {$IF CompilerVersion >= 23}
uses SysUtils, Classes, DesignIntf, DesignEditors, VCL.Dialogs;
  {$ELSE}
uses SysUtils, Classes, DesignIntf, DesignEditors, Dialogs;
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
uses SysUtils, Classes, DesignIntf, DesignEditors, QDialogs;
{$ENDIF}

type
  TXBFilenameProperty = class(TStringProperty)
      public
         procedure Edit; override;
         function GetAttributes: TPropertyAttributes; override;
  end;


Procedure Register;

implementation

{#BACKUP *.ICO}

uses XBase1;

{******************************************************************************}
{* Property Editor Code                                                       *}
{******************************************************************************}

procedure TXBFilenameProperty.Edit;
var
  XBFileOpen: TOpenDialog;
begin
  XBFileOpen := TOpenDialog.Create(Nil);
  XBFileOpen.Filename := GetValue;
  XBFileOpen.Filter := 'xBase Files|*.DBF';
  XBFileOpen.Options := XBFileOpen.Options + [ofPathMustExist, ofFileMustExist];
  try
    if XBFileOpen.Execute then SetValue(XBFileOpen.Filename);
  finally
    XBFileOpen.Free;
  end;
end;

{*****************************************}
function TXBFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{--------------------------------------}
{ Register                             }
{--------------------------------------}
Procedure Register;
Begin
  RegisterComponents('CHABANT',[TXBase]);     // XBase
  RegisterPropertyEditor(TypeInfo(WString), TXBase, 'FileName',
    TXBFileNameProperty);
End;

end.
