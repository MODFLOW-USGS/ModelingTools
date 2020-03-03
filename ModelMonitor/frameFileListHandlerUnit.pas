unit frameFileListHandlerUnit;

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, FileIndexUnit, JvSpin, VirtualTrees,
  System.Generics.Collections, IntListUnit, JvRichEdit, System.UITypes;

type
  PIndexDataNode = ^TIndexDataNode;
  TIndexDataNode = record
    LineNumber: Integer;
    Text: string;
  end;

  TframeFileListHandler = class(TFrame)
    vstIndexLines: TVirtualStringTree;
    procedure vstIndexLinesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstIndexLinesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstIndexLinesNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure vstIndexLinesNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure vstIndexLinesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FStartPosition: Integer;
    FCount: integer;
    ParentNodes: TList<PVirtualNode>;
    AllNodes: TList<PVirtualNode>;
    FSelectedNode: PVirtualNode;
    FLineNumbers: TIntegerList;
    FChangingLine: Boolean;
    FLineNumber: integer;
    FDictionary: TDictionary<String,PVirtualNode>;
    function TooManyLines(NumberOAfLinesToAdd: Integer): Boolean;
    procedure SetSelectedNode(const Value: PVirtualNode);
    procedure UpdateNumbers(StartingNumber: Integer);
    function TryReopenFile: Boolean;
    { Private declarations }
  public
    Memo: TJvRichEdit;// TRichEdit;
    memoLineNumbers: TCustomMemo; // TMemo
    ListFile: TFileIndex;
    LineCountToRead: TJvSpinEdit;
    procedure Initialize;
    function AddLine(LineNumber: integer; ALine: string;
      Indent: Integer): integer;
    procedure AddToLine(LineIndex: integer; ALine: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLines;
    procedure InsertLines;
    procedure GoToLine(LineNumber: Integer);
    function ActiveLine: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    function HasContent: Boolean;
    procedure ChangeLineNumber(const LineNumber: integer);
    property SelectedNode: PVirtualNode read FSelectedNode
      write SetSelectedNode;
    procedure AddToDictionary(Key: string; LineNumber: integer; ALine: string);
    function NodeCount: Integer;
    { Public declarations }
  end;

implementation

uses
  Math, frmListAnalyzerUnit;

resourcestring
  StrAttemptingToReadT = 'Attempting to read this many lines may take a whil' +
  'e. Do you want to continue anyway?';
  StrTheFileHasChanged = 'The file has changed. Do you want to open the new ' +
  'version?';

{$R *.dfm}

{ TframeFileListHandler }

procedure TframeFileListHandler.AddLines;
var
  NewLines: TStringList;
  NumberOAfLinesToAdd: Integer;
  StartingPosition: Integer;
  SelStart: Integer;
  SelLength: Integer;
begin
//  StartingPosition := FStartPosition + FCount;
  StartingPosition := ListFile.CurrentStartLine + Memo.Lines.Count;

  if StartingPosition >= ListFile.LineCount then
  begin
    Exit;
  end;
  NumberOAfLinesToAdd := LineCountToRead.AsInteger;
  if TooManyLines(NumberOAfLinesToAdd) then
  begin
    Exit;
  end;
  NewLines := TStringList.Create;
  try
    if not TryReopenFile then
    begin
      Exit;
    end;
    ListFile.ReadLines(NewLines, StartingPosition, NumberOAfLinesToAdd);
    NumberOAfLinesToAdd := NewLines.Count;
    SelStart := Memo.SelStart;
    SelLength := Memo.SelLength;
    Memo.Tag := 1;

    try
      Memo.Lines.BeginUpdate;
      try
        Memo.Lines.AddStrings(NewLines);
      finally
        Memo.Lines.EndUpdate;
      end;
      Memo.SelStart := SelStart;
      Memo.SelLength := SelLength;
    finally
      Memo.Tag := 0;
    end;
    UpdateNumbers(FStartPosition);

    FCount := FCount + NumberOAfLinesToAdd;
  finally
    NewLines.Free;
    ListFile.CloseFile;
  end;
  if Memo.CanFocus then
  begin
    Memo.SetFocus;
  end;
end;

procedure TframeFileListHandler.ChangeLineNumber(const LineNumber: integer);
var
  ClosestPostion: Integer;
  ClosestLine : integer;
begin
  if FLineNumber = LineNumber then
  begin
    Exit;
  end;
  if (FLineNumbers.Count > 0) and not FChangingLine then
  begin
    FLineNumber := LineNumber;
    ClosestPostion := FLineNumbers.Nearest(LineNumber);
    ClosestLine := FLineNumbers[ClosestPostion];
    if (ClosestLine > LineNumber) and (ClosestPostion > 0) then
    begin
      Dec(ClosestPostion);
    end;
    vstIndexLines.Selected[AllNodes[ClosestPostion]] := True;
//    if not vstIndexLines.IsVisible[AllNodes[ClosestPostion]] then
    begin
      vstIndexLines.ScrollIntoView(AllNodes[ClosestPostion], True)
    end;
  end;
end;

function TframeFileListHandler.TooManyLines(NumberOAfLinesToAdd: Integer): Boolean;
begin
  result := False;
  if NumberOAfLinesToAdd > 10000 then
  begin
    Beep;
    if (MessageDlg(StrAttemptingToReadT, mtWarning,
      [mbYes, mbNo], 0) = mrNo) then
    begin
      result := True;
    end;
  end;
end;

constructor TframeFileListHandler.Create(AOwner: TComponent);
begin
  inherited;
  ParentNodes:= TList<PVirtualNode>.Create;
  AllNodes := TList<PVirtualNode>.Create;
  FLineNumbers := TIntegerList.Create;
  FDictionary := TDictionary<String,PVirtualNode>.Create;
  FLineNumber := -1;
end;

destructor TframeFileListHandler.Destroy;
begin
  FLineNumbers.Free;
  AllNodes.Free;
  ParentNodes.Free;
  FDictionary.Free;
  inherited;
end;

procedure TframeFileListHandler.EndUpdate;
begin
  vstIndexLines.EndUpdate;
end;

procedure TframeFileListHandler.AddToDictionary(Key: string;
  LineNumber: integer; ALine: string);
var
  ParentNode: PVirtualNode;
  ANode: PVirtualNode;
  NodeData: PIndexDataNode;
begin
  if not FDictionary.TryGetValue(Key, ParentNode) then
  begin
    vstIndexLines.RootNodeCount := vstIndexLines.RootNodeCount + 1;
    ParentNode := vstIndexLines.GetLast;
    FDictionary.Add(Key, ParentNode);
    NodeData := vstIndexLines.GetNodeData(ParentNode);
    NodeData.LineNumber := -1;
    NodeData.Text := Key;
  end;
  vstIndexLines.ChildCount[ParentNode] := vstIndexLines.ChildCount[ParentNode]+1;
  ANode := vstIndexLines.GetLastChild(ParentNode);
  NodeData := vstIndexLines.GetNodeData(ANode);
  NodeData.LineNumber := LineNumber;
  NodeData.Text := Trim(ALine);
  AllNodes.Add(ANode);

end;

function TframeFileListHandler.TryReopenFile: Boolean;
begin
  result := True;
  if not ListFile.TryReopenFile then
  begin
    result := False;
    if (MessageDlg(StrTheFileHasChanged,
      mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
    begin
      frmMain.OpenAFile(ListFile.FileName);
    end;
  end;
end;

procedure TframeFileListHandler.AddToLine(LineIndex: integer; ALine: string);
var
  ANode: PVirtualNode;
  NodeData: PIndexDataNode;
begin
  ANode := AllNodes[LineIndex];
  NodeData := vstIndexLines.GetNodeData(ANode);
  if Pos(ALine, NodeData.Text) <= 0 then
  begin
    NodeData.Text := NodeData.Text + ' ' + ALine;
  end;
end;

procedure TframeFileListHandler.BeginUpdate;
begin
  vstIndexLines.BeginUpdate;
end;

function TframeFileListHandler.ActiveLine: string;
var
  NodeData: PIndexDataNode;
begin
  if FSelectedNode <> nil then
  begin
    NodeData := vstIndexLines.GetNodeData(FSelectedNode);
    result := NodeData.Text;
  end
  else
  begin
    result := ''
  end;

end;

function TframeFileListHandler.AddLine(LineNumber: integer; ALine: string;
  Indent: Integer): integer;
var
  ParentNode: PVirtualNode;
  ANode: PVirtualNode;
  NodeData: PIndexDataNode;
begin
  while ParentNodes.Count > Indent do
  begin
    ParentNodes.Delete(ParentNodes.Count-1);
  end;
  if ParentNodes.Count > 0 then
  begin
    ParentNode := ParentNodes[ParentNodes.Count-1];
  end
  else
  begin
    ParentNode := nil;
  end;
  if ParentNode  = nil then
  begin
    vstIndexLines.RootNodeCount := vstIndexLines.RootNodeCount + 1;
  end
  else
  begin
    vstIndexLines.ChildCount[ParentNode] := vstIndexLines.ChildCount[ParentNode] + 1;
  end;
  ANode := vstIndexLines.GetLast(ParentNode);
  ParentNodes.Add(ANode);
  AllNodes.Add(ANode);
  NodeData := vstIndexLines.GetNodeData(ANode);
  NodeData.LineNumber := LineNumber;
  NodeData.Text := Trim(ALine);
  result := AllNodes.Count-1;
  FLineNumbers.Add(LineNumber);
end;

procedure TframeFileListHandler.Initialize;
begin
  FSelectedNode := nil;
  ParentNodes.Clear;
  AllNodes.Clear;
  vstIndexLines.Clear;
  FLineNumbers.Clear;
  FDictionary.Clear;
end;

procedure TframeFileListHandler.InsertLines;
var
  NumberOAfLinesToInsert: Integer;
  StartingPosition: Integer;
  NewLines: TStringList;
  LineIndex: Integer;
  InsertLength: Integer;
  SelStart: Integer;
  SelLength: Integer;
begin
  NumberOAfLinesToInsert := LineCountToRead.AsInteger;
  if TooManyLines(NumberOAfLinesToInsert) then
  begin
    Exit;
  end;
  StartingPosition := ListFile.CurrentStartLine - NumberOAfLinesToInsert;
  if StartingPosition < 0 then
  begin
    NumberOAfLinesToInsert := NumberOAfLinesToInsert + StartingPosition;
    StartingPosition := 0;
  end;
  NewLines := TStringList.Create;
  try
    if not TryReopenFile then
    begin
      Exit;
    end;

    SelStart := Memo.SelStart;
    SelLength := Memo.SelLength;
    ListFile.ReadLines(NewLines, StartingPosition, NumberOAfLinesToInsert, True);
    InsertLength := 0;
    Memo.Tag := 1;
    try
      Memo.Lines.BeginUpdate;
      try
        for LineIndex := NewLines.Count - 1 downto 0 do
        begin
          Memo.Lines.Insert(0, NewLines[LineIndex]);
          InsertLength := InsertLength + Length(NewLines[LineIndex])+1;
        end;
      finally
        Memo.Lines.EndUpdate;
      end;
      Memo.SelStart := SelStart + InsertLength;
      Memo.SelLength := SelLength;
      FStartPosition := StartingPosition;
      FCount := FCount + NumberOAfLinesToInsert;
    finally
      Memo.Tag := 0;
    end;
    UpdateNumbers(FStartPosition);
  finally
    NewLines.Free;
    ListFile.CloseFile;
  end;
  if Memo.CanFocus then
  begin
    Memo.SetFocus;
  end;
end;

function TframeFileListHandler.NodeCount: Integer;
begin
  result := AllNodes.Count;
end;

procedure TframeFileListHandler.SetSelectedNode(const Value: PVirtualNode);
var
  NodeData: PIndexDataNode;
  StartIndex: Integer;
  LineNumber: Integer;
  X: Integer;
begin
  FSelectedNode := Value;
  vstIndexLines.Selected[FSelectedNode] := True;
  NodeData := vstIndexLines.GetNodeData(FSelectedNode);
  Assert(Memo <> nil);
  Assert(ListFile <> nil);
  Assert(LineCountToRead <> nil);

  StartIndex := NodeData.LineNumber;
  FStartPosition := Max(0, StartIndex - 10);
  LineNumber := FStartPosition;

  Memo.Tag := 1;
  try
    GoToLine(LineNumber);
  finally
    Memo.Tag := 0;
  end;
  X := Length(Memo.Lines[StartIndex-FStartPosition]);
  Memo.CaretPos := Point(X, StartIndex-FStartPosition);
  Memo.SelLength := -X;
  if Memo.CanFocus then
  begin
    Memo.SetFocus;
  end;
end;

type
  TMemoCrack = class(TCustomMemo);

procedure TframeFileListHandler.UpdateNumbers(StartingNumber: Integer);
var
  index: Integer;
begin
  memoLineNumbers.Lines.BeginUpdate;
  try

    memoLineNumbers.Lines.Clear;
    for index := 0 to Memo.Lines.Count-1 do
    begin
      memoLineNumbers.Lines.Add(IntToStr(StartingNumber+index+1));
    end;

  finally
    memoLineNumbers.Lines.EndUpdate;
    if Assigned(Memo) and Assigned(Memo.OnVerticalScroll) then
    begin
      Memo.OnVerticalScroll(Memo);
    end;
  end;
end;

procedure TframeFileListHandler.GoToLine(LineNumber: Integer);
var
  NewCount: Integer;
begin
  try
    if not TryReopenFile then
    begin
      Exit;
    end;

    FChangingLine := True;
    try
      NewCount := LineCountToRead.AsInteger;
      if TooManyLines(NewCount) then
      begin
        Exit;
      end;
      FCount := NewCount;
      Memo.Lines.BeginUpdate;
      try
        if ListFile.ReadLines(Memo.Lines, LineNumber, FCount, True) then
        begin
          FStartPosition := LineNumber;
          UpdateNumbers(FStartPosition);
        end
        else
        begin
          Exit;
        end;
      finally
        Memo.Lines.EndUpdate;
      end;
      Memo.SelStart := 0;
      Memo.SelLength := 0;
      FCount := Memo.Lines.Count;
      if Assigned(TMemoCrack(Memo).OnClick) then
      begin
        TMemoCrack(Memo).OnClick(nil);
      end;
    finally
      FChangingLine := False;
    end;
  finally
    ListFile.CloseFile;
  end;
end;

function TframeFileListHandler.HasContent: Boolean;
begin
  result :=
    (vstIndexLines.RootNode.ChildCount > 0)
end;

procedure TframeFileListHandler.vstIndexLinesGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TIndexDataNode);
end;

procedure TframeFileListHandler.vstIndexLinesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PIndexDataNode;
begin
  NodeData := vstIndexLines.GetNodeData(Node);
  CellText := NodeData.Text;
end;

procedure TframeFileListHandler.vstIndexLinesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ANode: PVirtualNode;
  NodeData: PIndexDataNode;
  APoint: TPoint;
begin
  ANode := vstIndexLines.GetNodeAt(X, Y);
  if ANode <> nil then
  begin
    NodeData := vstIndexLines.GetNodeData(ANode);
    if vstIndexLines.Hint <> NodeData.Text then
    begin
      vstIndexLines.Hint := NodeData.Text;
      APoint.X := X;
      APoint.Y := Y;
      APoint := vstIndexLines.ClientToScreen(APoint);
      Application.ActivateHint(APoint);
    end;
  end;
end;

procedure TframeFileListHandler.vstIndexLinesNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  NodeData: PIndexDataNode;
begin
  FSelectedNode := HitInfo.HitNode;
  NodeData := vstIndexLines.GetNodeData(FSelectedNode);
  vstIndexLines.Hint := NodeData.Text;
  vstIndexLinesNodeDblClick(Sender, HitInfo);
end;

procedure TframeFileListHandler.vstIndexLinesNodeDblClick(
  Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if hiOnItemLabel in HitInfo.HitPositions then
  begin
    SelectedNode := HitInfo.HitNode;
  end;
end;

end.
