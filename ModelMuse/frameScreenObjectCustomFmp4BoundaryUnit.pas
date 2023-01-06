unit frameScreenObjectCustomFmp4BoundaryUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ModflowFmp4BoundaryUnit, UndoItemsScreenObjects;

type
  TframeScreenObjectCustomFmp4Boundary = class(TframeScreenObjectNoParam)
  private
    FValuesCleared: Boolean;
    { Private declarations }
  protected
    function GetValueDescription: string; virtual; abstract;
    function GetBoundary(Item: TScreenObjectEditItem): TFmp4Boundary; virtual; abstract;
    procedure InitializeGrid;
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectCustomFmp4Boundary: TframeScreenObjectCustomFmp4Boundary;

implementation

uses
  ScreenObjectUnit;

{$R *.dfm}

{ TframeScreenObjectCustomFmp4Boundary }

procedure TframeScreenObjectCustomFmp4Boundary.GetData(
  List: TScreenObjectEditCollection);
var
  FoundFirst: Boolean;
  Index: Integer;
  Boundary: TFmp4Boundary;
  Item: TScreenObjectEditItem;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    InitializeGrid;
    FValuesCleared := False;
    try
      Assert(List.Count >= 1);
      FoundFirst := False;
//      FirstBoundary := nil;
      for Index := 0 to List.Count - 1 do
      begin
        Item := List[Index];
        Boundary := GetBoundary(Item);
        if (Boundary <> nil) and Boundary.Used then
        begin
          if FoundFirst then
          begin

          end
          else
          begin
            FoundFirst := True;

          end;

        end;
      end;

    finally

    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
end;

procedure TframeScreenObjectCustomFmp4Boundary.InitializeGrid;
begin
  ConductanceColumn := -1;
  rdgModflowBoundary.BeginUpdate;
  try
    InitializeNoParamFrame(nil);
    rdgModflowBoundary.Cells[2,0] := GetValueDescription;
    GetStartTimes(0);
    GetEndTimes(1);
  finally
    rdgModflowBoundary.EndUpdate;
  end;

end;

procedure TframeScreenObjectCustomFmp4Boundary.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
begin
end;

end.
