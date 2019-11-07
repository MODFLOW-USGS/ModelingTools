
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit mrucombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboBox;

const

  DefaultKey = 'Software\Your company\Your product\Version\MRUList';

type

{ TksoMRUComboBox }

  TMRUAddEvent = procedure(Value: string; var CanAdd: boolean) of object;

  TksoMRUComboBox = class(TksoCustomComboBox)
  private
    FSaveKey: string;
    FSaveRoot: cardinal;
    FOnAddToMRU: TMRUAddEvent;
    FCaseSensitive: boolean;
    FAutoSave: boolean;
    procedure SetCaseSensitive(const Value: boolean);
    procedure SetSaveKey(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure ExitFocus; override;
    { Load/Save methods }
    procedure AddToMRU;
    function ItemExist(Value: string): boolean;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    property SaveRoot: cardinal read FSaveRoot write FSaveRoot;
  published
    property AutoSave: boolean read FAutoSave write FAutoSave;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property BorderStyle;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SaveKey: string read FSaveKey write SetSaveKey;
    property Sorted;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnAddToMRU: TMRUAddEvent read FOnAddToMRU write FOnAddToMRU;
    property OnClick;
    property OnChange;
    property OnDrawItem;
    property OnMeasureItem;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation {===============================================================}

uses Registry;

{ TksoMRUComboBox }

constructor TksoMRUComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := false;
  FSaveRoot := HKEY_CURRENT_USER;
  FSaveKey := DefaultKey;
  FAutoSave := true;
  Style := csDropDown;
end;

destructor TksoMRUComboBox.Destroy;
begin
  if not (csDesigning in ComponentState) then
    SaveToRegistry;
  inherited Destroy;
end;

procedure TksoMRUComboBox.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    LoadFromRegistry;
end;

function TksoMRUComboBox.ItemExist(Value: string): boolean;
var
  i: integer;
  S1, S2: string;
begin
  Result := true;
  for i := 0 to Items.Count-1 do
  begin
    S1 := Items[i];
    S2 := Value;
    if not FCaseSensitive then
    begin
      S1 := LowerCase(S1);
      S2 := LowerCase(S2);
    end;
    if S1 = S2 then Exit;
  end;
  Result := false;
end;

procedure TksoMRUComboBox.AddToMRU;
begin
  if Text <> '' then
  begin
    { Check exist item }
    if not ItemExist(Text) then
      { Add to list }
      Items.Add(Text);
  end;
end;

procedure TksoMRUComboBox.SetCaseSensitive(const Value: boolean);
begin
  FCaseSensitive := Value;
end;

procedure TksoMRUComboBox.LoadFromRegistry;
var
  Reg: TRegistry;
  i, Count: integer;
  Value: string;
begin
  if FSaveKey <> '' then
  begin
    Reg := TRegistry.Create;
    try
      if Reg.OpenKey(FSaveKey+Name, false) then
      begin
        Count := Reg.ReadInteger('Count');
        for i := 0 to Count-1 do
        begin
          Value := Reg.ReadString(IntToStr(i));
          if not ItemExist(Value) then Items.Add(Value);
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure TksoMRUComboBox.SaveToRegistry;
var
  Reg: TRegistry;
  i: integer;
begin
  if (FSaveKey <> '') and (FAutoSave) then
  begin
    Reg := TRegistry.Create;
    try
      Reg.OpenKey(FSaveKey+Name, true);
      Reg.WriteInteger('Count', Items.Count);
      for i := 0 to Items.Count-1 do
        Reg.WriteString(IntToStr(i), Items[i]);
    finally
      Reg.Free;
    end;
  end;
end;

procedure TksoMRUComboBox.ExitFocus;
begin
  inherited ExitFocus;
  AddToMRU;
end;

procedure TksoMRUComboBox.SetSaveKey(const Value: string);
begin
  FSaveKey := Value;
  If FSaveKey[Length(FSaveKey)] <> '\' then
    FSaveKey := FSaveKey + '\';
end;

end.
