unit frameScreenObjectTransientKUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects;

type
  TframeScreenObjectTransientK = class(TframeScreenObjectNoParam)
  private
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectTransientK: TframeScreenObjectTransientK;

implementation

{$R *.dfm}

{ TframeScreenObjectTransientK }

procedure TframeScreenObjectTransientK.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
begin

end;

procedure TframeScreenObjectTransientK.SetData(
  List: TScreenObjectEditCollection; SetAll, ClearAll: boolean);
begin

end;

end.
