unit frmSwrVertexNumbersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, frameGridUnit, GoPhastTypes,
  StdCtrls, Buttons, ExtCtrls;

type
  TfrmSwrVertexNumbers = class(TfrmCustomGoPhast)
    frameVertexNumbers: TframeGrid;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
  private
    { Private declarations }
  public
    procedure GetData(VertexNumbers: string);
    function SetData: string;
    { Public declarations }
  end;

var
  frmSwrVertexNumbers: TfrmSwrVertexNumbers;

implementation

resourcestring
  StrReachNumbers = 'Reach numbers';

{$R *.dfm}

{ TfrmSwrVertexNumbers }

procedure TfrmSwrVertexNumbers.FormCreate(Sender: TObject);
begin
  inherited;
  frameVertexNumbers.Grid.Cells[0,0] := StrReachNumbers;
end;

procedure TfrmSwrVertexNumbers.GetData(VertexNumbers: string);
var
  Splitter: TStringList;
  index: Integer;
  AValue: integer;
begin
  Splitter := TStringList.Create;
  try
    Splitter.CommaText := VertexNumbers;
    for index := Splitter.Count - 1 downto 0 do
    begin
      if not TryStrToInt(Splitter[index], AValue) then
      begin
        Splitter.Delete(index);
      end;
    end;
    frameVertexNumbers.seNumber.AsInteger := Splitter.Count;
    frameVertexNumbers.seNumberChange(nil);
    frameVertexNumbers.Grid.BeginUpdate;
    try
      for index := 0 to Splitter.Count - 1 do
      begin
        frameVertexNumbers.Grid.Cells[0,index+1] := Splitter[index];
      end;
    finally
      frameVertexNumbers.Grid.EndUpdate;
    end;
  finally
    Splitter.Free;
  end;
end;

function TfrmSwrVertexNumbers.SetData: string;
var
  Joiner: TStringList;
  RowIndex: integer;
  AReach: Integer;
begin
  Joiner := TStringList.Create;
  try
    for RowIndex := 1 to frameVertexNumbers.Grid.RowCount - 1 do
    begin
      if TryStrToInt(frameVertexNumbers.Grid.Cells[0, RowIndex], AReach) then
      begin
        Joiner.Add(frameVertexNumbers.Grid.Cells[0, RowIndex])
      end;
    end;
    result := Joiner.CommaText;
    result := StringReplace(result, ',', ', ', [rfReplaceAll])
  finally
    Joiner.Free;
  end;
end;

end.
