
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit autocombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboBox, MRUCombo;

type

{ TksoAutoFillComboBox }

  TksoAutoFillComboBox = class(TksoMRUComboBox)
  private
    FOldText: string;
    FWork: boolean;
    FTimer: TTimer;
    procedure SetCompleteInterval(const Value: integer);
    function GetCompleteInterval: integer;
    procedure DoTimer(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Change; override;
  published
    property AutoSave;
    property CaseSensitive;
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
    property CompleteInterval: integer read GetCompleteInterval write SetCompleteInterval;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property BorderStyle;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SaveKey;
    property Sorted;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnAddToMRU;
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

{ TksoAutoFillComboBox }

constructor TksoAutoFillComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := true;
  FWork := false;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := false;
end;

destructor TksoAutoFillComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoAutoFillComboBox.Loaded;
begin
  inherited Loaded;
  FOldText := Text;
end;

procedure TksoAutoFillComboBox.Change;
begin
  inherited Change;
  if not FWork then
    FTimer.Enabled := true;
end;

procedure TksoAutoFillComboBox.SetCompleteInterval(const Value: integer);
begin
  FTimer.Interval := Value;
end;

function TksoAutoFillComboBox.GetCompleteInterval: integer;
begin
  Result := FTimer.Interval;
end;

procedure TksoAutoFillComboBox.DoTimer(Sender: TObject);
var
  i: integer;
  S1, S2: string;
begin
  FTimer.Enabled := false;
  if Length(Text) < Length(FOldText) then
  begin
    FOldText := Text;
    Exit;
  end;
  if (FOldText <> Text) and (not FWork) then
  begin
    FOldText := Text;
    FWork := true;
    for i := 0 to Items.Count-1 do
    begin
      S1 := Items[i];
      S2 := Text;
      if not CaseSensitive then
      begin
        S1 := LowerCase(S1);
        S2 := LowerCase(S2);
      end;
      if S2 = Copy(S1, 1, Length(S2)) then
      begin
        Text := Items[i];
        if Edit <> nil then
        begin
          Edit.SelStart := Length(S2);
          Edit.SelLength := Length(S1);
        end;
        FWork := false;
        Exit;
      end;
    end;
    FWork := false;
  end;
end;

end.
