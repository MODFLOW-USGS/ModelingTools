
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit glyphdsgn;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses Classes, Menus, Dialogs, {$IFDEF OVCL_D6} DesignEditors, DesignIntf {$ELSE} DsgnIntf {$ENDIF};

procedure Register;

implementation {===============================================================}

uses GlyphCombo, GlyphForm;

type

  TGlyphItemsProperty = class(TPropertyEditor)
  private
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure TGlyphItemsProperty.Edit;
var
  Comp: TksoGlyphComboBox;
begin
  try
    Comp := TksoGlyphComboBox(GetComponent(0));
    GlyphForm.Execute(Comp);
  finally
  end;
end;

function TGlyphItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGlyphItemsProperty.GetValue: string;
begin
  Result := '(Items)';
end;

procedure TGlyphItemsProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStrings), TksoGlyphComboBox, '', TGlyphItemsProperty);
end;

end.
