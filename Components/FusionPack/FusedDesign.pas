unit FusedDesign;

interface

uses DesignIntf, DesignEditors;

type

  { Component editor - brings up Label editor when double clicking on
    Labels property }
  TLabelEditor = class(TDefaultEditor)
  protected
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;


implementation

uses FusedLabel;

{TLabelEditor}
procedure TLabelEditor.Edit;
begin
  TFusedLabel(Component).DoEdit;
end;

function TLabelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TLabelEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Edit EZLabel'
  else Result := '';
end;

procedure TLabelEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
   Begin
     TFusedLabel(Component).DoEdit;
   End;
end;

procedure Register;
begin
  RegisterComponentEditor(TFusedLabel, TLabelEditor);
end;


end.
