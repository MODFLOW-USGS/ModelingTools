{
May 22, 2006: introduced CustomizeControls.
}

{@abstract(The purpose of @name is to declare @link(TfrmCustomGoPhast),
  the ancestor of all TForms in ModelMuse.)

It also provides the @link(ShowAForm) method.
}
unit frmCustomGoPhastUnit;

interface

uses
  System.UITypes, CommDlg, RbwDataGrid4,
  Windows, Forms, SysUtils, Types,
  Classes, Graphics, Controls, Dialogs, StdCtrls, Grids,
  VirtualTrees, DataSetUnit, GLWin32Viewer, Generics.Collections,
  JvListComb, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  EAbortingImport = class(Exception);
  TDatasetResponse = (drNew, drUpdate, drAbort);


  // @name is used in @link(TfrmCustomGoPhast.AdjustFormPosition)
  // to tell whether the form
  // should be to the right or to the left of the main form.
  TDesiredPosition = (dpLeft, dpRight, dpBottomLeft, dpBottomRight);

  THelpFormat = (hfUndefined, hfLocal, hfWeb);

  {@abstract(@name is the ancestor of all TForms in GoPhast.)
   @name handles setting the color and font. @name also tries
   to keep from appearing at a location where it can't be seen. )}
  TfrmCustomGoPhast = class(TForm)
    // @name is the event handler for OnCreate.
    // It calls @link(CustomizeControls).
    procedure FormCreate(Sender: TObject); virtual;
    // @name is the eventhandler for OnShow.
    // It moves the form to try to get it completely on the screen
    // and calls @link(SetAppearance).
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
{$IF CompilerVersion < 23}
// Use this version for Delphi XE
   function FormHelp(Command: Word; Data:  Integer;
     var CallHelp: Boolean): Boolean;
{$ELSE}
// Use this version for Delphi XE2
   function FormHelp(Command: Word; Data:  NativeInt;
     var CallHelp: Boolean): Boolean;
{$IFEND}
    procedure FormDestroy(Sender: TObject); virtual;
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCallingHelp: Boolean;
    FCreateNewDataSet: Boolean;
    FAskedUser: Boolean;
    FLeft: Integer;
    FTop: Integer;
    FMonitorNumber: Integer;
    function CallHelpRouter: boolean;
    procedure DestroyGLSceneViewers(ParentComponent: TComponent);
    procedure EnsureFormVisible;
    procedure FixGridEditorPosition(AComponent: TComponent);
  protected
    procedure UpdateStringTreeViewCheckedState(TreeView: TVirtualStringTree;
      BaseNode: PVirtualNode; NewState: TCheckState);
    {@name adjusts the position of Form to the left or right of the main form,
     if possible.  If there isn't room for it at the desired position it will
     try the other side.  If there isn't room there either,
     it will have it overlap
     the form on the desired side.
     A side effect is that it sets Form.Position to
     poDesigned.}
    procedure AdjustFormPosition(DesiredPosition: TDesiredPosition);
    // @name checks if @link(GlobalFont) is nil. If not, it sets the
    // color, font, and icon used in the form
    // to be @link(GlobalFont), @link(GlobalColor) and Application.Icon.
    // @link(GlobalFont) and @link(GlobalColor) should be set in the
    // Applications OnCreate event handler.
    procedure SetAppearance;
    // @name is used when importing data to a model.
    // If the user cancels the dialog box, EAbortingImport is raised.
    function AskUserIfNewDataSet: Boolean;
  public
    procedure MouseClick;
    procedure UpdateSubComponents(AComponent: TComponent);
    // If the Application.MainForm has been assigned,
    // @name sets the color, font, and icon used in the form
    // to be the same as in Application.MainForm via a call to
    // @link(SetAppearance).
    procedure CustomizeControls;
    procedure FillComboBoxWithModels(ACombo: TComboBox);
    function CanDisplayDataSet(DataArray: TDataArray): boolean;
    procedure Show;
    { Public declarations }
  end;

  // @name changes all the cells in Grid that are in Column and that
  // are selected to NewText.
  procedure ChangeSelectedCellsInColumn(Grid: TRbwDataGrid4;
    const Column: integer; const NewText: string);
  // @name changes all the checked stated of all cells in Grid that
  // are in Column and that are selected to NewState.
  procedure ChangeSelectedCellsStateInColumn(Grid: TRbwDataGrid4;
    const Column: integer; const NewState: TCheckBoxState);
  // @name checks all the cells in Grid that are in Col.  If at least one
  // such cell is selected, AControl will be enabled. Otherwise, it will be
  // disabled.
  procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
    Col: integer); overload;
  procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
    Cols: array of integer); overload;
  // @name creates an instance of FormClass and calls ShowModal
  // on that instance.
  function ShowAForm(const FormClass: TFormClass): integer;
  // @name creates an instance of FormClass and calls Show
  // on that instance. The form is responsible for destroying itself when
  // closed by calling Release;
procedure DisplayAForm(const FormClass: TFormClass);

  // @name sets the Left and Width of Control and ALabel so that they are
// lined up over Column in Grid. ALabel.Alignment should be taCenter.
procedure LayoutControls(Grid: TRbwDataGrid4; Control: TControl;
  ALabel: TLabel; Column: Integer; ControlOffSet: integer = 0);

Procedure UpdateDialogBoxFileName(Dialog: TOpenDialog; NewFileName: string);

procedure FillComboWithModelNames(Combo: TComboBox);

// @name checks that at least one of the TCheckBoxes in CheckBoxArray is
// checked.  If not the color of all of them is set to clRed and all
// have their fonts changed to bold.
procedure EmphasizeCheckBoxes(const CheckBoxArray: array of TCheckBox);

function ShowHelp(const Keyword: string; HelpFormat: THelpFormat): boolean;

procedure ClearGrid(Grid: TRbwDataGrid4);

procedure UpdateNextTimeCell(Grid: TRbwDataGrid4; ACol, ARow: Integer);

procedure UpdateColorScheme(Combo: TComboBox; PaintBox: TPaintBox);

var
  GlobalFont: TFont = nil;
  GlobalColor: TColor = clBtnFace;
  ShowingForm: boolean = False;

resourcestring
  StrRainbow = 'Rainbow';
  StrGreenToMagenta = 'Green to Magenta';
  StrBlueToRed = 'Blue to Red';
  StrBlueToDarkOrange = 'Blue to Dark Orange';
  StrBlueToGreen = 'Blue to Green';
  StrBrownToBlue = 'Brown to Blue';
  StrBlueToGray = 'Blue to Gray';
  StrBlueToOrange = 'Blue to Orange';
  StrBlueToOrangeRed = 'Blue to Orange-Red';
  StrLightBlueToDarkB = 'Light Blue to Dark Blue';
  StrModifiedSpectralSc = 'Modified Spectral Scheme';
  StrSteppedSequential = 'Stepped Sequential';

implementation

uses GoPhastTypes, frmGoPhastUnit, PhastModelUnit,
  JvRollOut, Messages, ShellAPI, RbwInternetUtilities, frmUpdateDataSetsUnit;

{$R *.dfm}

type
  TStringGridCrack = class(TStringGrid);

// from http://delphi.about.com/cs/adptips2000/a/bltip0100_2.htm
function KillApp(const sCapt: PChar) : boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow(Nil, sCapt) ;
  if AppHandle <> 0 then
  begin
    Result:=PostMessage(AppHandle, WM_QUIT, 0, 0) ;
  end
  else
  begin
    Result := False;
  end;
end;


function ShowHelp(const Keyword: string; HelpFormat: THelpFormat): boolean;
const
  HelpUrl = 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/Help/index.html';
//var
//  AppHandle:THandle;
var
  Browser: string;
begin
{
  AppHandle:=FindWindow(Nil, 'ModelMuse Help');
  if AppHandle = 0 then
  begin
    if Keyword = '' then
    begin
  //    Result := ShellExecute(AppHandle, 'open', 'HH', PChar(Application.HelpFile),
      Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
        nil, SW_SHOWNORMAL) > 32;
    end
    else
    begin
  //    Result := ShellExecute(AppHandle, 'open', 'HH',
      Result := ShellExecute(0, 'open', 'HH',
        PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
        nil, SW_SHOWNORMAL) > 32;
    end;
  end
  else
  begin
    if Keyword = '' then
    begin
      Result := ShellExecute(AppHandle, 'explore', 'HH', PChar(Application.HelpFile),
//      Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
        nil, SW_SHOWNORMAL) > 32;
    end
    else
    begin
      Result := ShellExecute(AppHandle, 'explore', 'HH',
//      Result := ShellExecute(0, 'open', 'HH',
        PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
        nil, SW_SHOWNORMAL) > 32;
    end;
  end;
}

  case HelpFormat of
    hfLocal:
      begin
        KillApp('ModelMuse Help');
        if Keyword = '' then
        begin
          Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
            nil, SW_SHOWNORMAL) > 32;
        end
        else
        begin
          Result := ShellExecute(0, 'open', 'HH',
            PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
            nil, SW_SHOWNORMAL) > 32;
        end;
      end;
    hfWeb:
      begin
        if Keyword = '' then
        begin
          result := LaunchURL(Browser, HelpUrl);
        end
        else
        begin
          result := LaunchURL(Browser, HelpUrl + '?' + LowerCase(Keyword) + '.htm');
        end;
      end;
    else
      result := False;
      Assert(False);
  end;

end;

Procedure UpdateDialogBoxFileName(Dialog: TOpenDialog; NewFileName: string);
const
  CB_FILENAME_ID = 1148;
begin
  // This is supposed to update the file name displayed by the TOpenDialog.
  // It doesn't seem to work.
  SendMessage( GetParent(Dialog.Handle), CDM_SETCONTROLTEXT,
    CB_FILENAME_ID, LongInt(Pchar(ExtractFileName(NewFileName))));
//  SendMessage( GetParent(Dialog.Handle), CDM_SETCONTROLTEXT,
//    CB_FILENAME_ID, LongInt(PAnsiChar(AnsiString(ExtractFileName(NewFileName)))));
end;

procedure TfrmCustomGoPhast.CustomizeControls;
begin
  if (Application.MainForm <> nil) and (self <> Application.MainForm) then
  begin
    SetAppearance;
  end;
end;

procedure TfrmCustomGoPhast.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  FMonitorNumber := -1;
//  HelpKeyWord := 'files\' + HelpKeyWord + '.htm';
end;

procedure TfrmCustomGoPhast.DestroyGLSceneViewers(ParentComponent: TComponent);
var
  Index: Integer;
  AComponent: TComponent;
begin
   Exit;
  // It seems that some TGLSceneViewer's are causing errors when
  // the form is being destroyed.  This is an attempt
  // to get around that.
  for Index := ParentComponent.ComponentCount - 1 downto 0 do
  begin
    AComponent := ParentComponent.Components[Index];
    if AComponent is TGLSceneViewer then
    begin
      AComponent.Free;
    end
    else
    begin
      DestroyGLSceneViewers(AComponent);
    end;
  end;
end;

procedure TfrmCustomGoPhast.FormDestroy(Sender: TObject);
begin
  DestroyGLSceneViewers(self);
end;

function TfrmCustomGoPhast.CallHelpRouter: boolean;
var
  KeyWord: string;
  HelpControl: TControl;
begin
  KeyWord := HelpKeyword;
  HelpControl := ActiveControl;
  while (HelpControl <> nil) do
  begin
    if HelpControl.HelpKeyword <> '' then
    begin
      KeyWord := HelpControl.HelpKeyword;
      break;
    end
    else
    begin
      if (HelpControl = self) then
      begin
        Break;
      end;
      HelpControl := HelpControl.Parent;
    end;
  end;
  result := not ShowHelp(KeyWord, frmGoPhast.HelpFormat);
//  result := Application.HelpJump(KeyWord);
//  result := HelpRouter.HelpJump('', KeyWord);
end;

{$IF CompilerVersion < 23}
// Delphi XE
function TfrmCustomGoPhast.FormHelp(Command: Word; Data:  Integer;
  var CallHelp: Boolean): Boolean;
{$ELSE}
// Delphi XE2
function TfrmCustomGoPhast.FormHelp(Command: Word; Data:  NativeInt;
  var CallHelp: Boolean): Boolean;
{$IFEND}
begin
  if (Command in [HELP_CONTEXT, HELP_INDEX, HELP_FORCEFILE,
    HH_DISPLAY_SEARCH {, 15}])
    {or (Command = HELP_COMMAND)} then
  begin
    // 15 = help contents.
    result := False;

  end
  else
  begin
    if FCallingHelp then
    begin
      result := True;
      CallHelp := True;
    end
    else
    begin
      FCallingHelp := True;
      result := CallHelpRouter;
      CallHelp := False;
      FCallingHelp := False;
    end;
//    result := True;
  end;
end;

procedure TfrmCustomGoPhast.FormHide(Sender: TObject);
begin
  FLeft := Monitor.Left;
  FTop  := Monitor.Top;
  FMonitorNumber := Monitor.MonitorNum;
end;

procedure TfrmCustomGoPhast.FormResize(Sender: TObject);
begin
  if (Self <> frmGoPhast) and (WindowState = wsMinimized) then
  begin
    frmGoPhast.WindowState := wsMinimized;
  end;
end;

procedure TfrmCustomGoPhast.btnHelpClick(Sender: TObject);
begin
  inherited;
  CallHelpRouter;
end;


function ShowAForm(const FormClass: TFormClass): integer;
begin
  ShowingForm := True;
  try
    with FormClass.Create(nil) do
    begin
      try
        if ModalResult = mrNone then
        begin
          ShowModal;
        end;
        result := ModalResult;
      finally
        Free;
      end;
    end;
  finally
    ShowingForm := False;
  end;
end;

procedure DisplayAForm(const FormClass: TFormClass);
begin
  with FormClass.Create(nil) do
  begin
    Show;
    // the form needs to destroy itself when closed.
  end;
end;

procedure TfrmCustomGoPhast.FixGridEditorPosition(AComponent: TComponent);
var
  Grid: TCustomRBWDataGrid;
  AnotherComponent: TComponent;
  ComponentIndex: integer;
begin
  for ComponentIndex := 0 to AComponent.ComponentCount - 1 do
  begin
    AnotherComponent := AComponent.Components[ComponentIndex];
    if AnotherComponent is TCustomRBWDataGrid then
    begin
      Grid := TCustomRBWDataGrid(AnotherComponent);
      if goAlwaysShowEditor in Grid.Options then
      begin
        Grid.Options := Grid.Options - [goAlwaysShowEditor];
        Grid.Options := Grid.Options + [goAlwaysShowEditor];
      end;
    end
    else
    begin
      FixGridEditorPosition(AnotherComponent);
    end;
  end;
end;

procedure TfrmCustomGoPhast.EnsureFormVisible;
var
  FrameWidth: Integer;
  WorkAreaRect: TRect;
  FrameHeight: Integer;
  AMonitor: TMonitor;
begin
  {$IFDEF LINUX}
  //if not QWidget_isVisible(Widget) then Exit;

  {$ENDIF}
  {$IFDEF MSWINDOWS}
  // Because the CLX Screen.Height doesn't take the taskbar into account,
  // use the VCL Screen object instead under windows to determine the available
  // space on the screen.
  if (FMonitorNumber >= 0) and (FMonitorNumber < Screen.MonitorCount) then
  begin
    AMonitor := Screen.Monitors[FMonitorNumber];
    WorkAreaRect.Left := AMonitor.Left;
    WorkAreaRect.Top := AMonitor.Top;
    WorkAreaRect.Right := WorkAreaRect.Left + AMonitor.Width;
    WorkAreaRect.Bottom := WorkAreaRect.Top + AMonitor.Height;
  end
  else
  begin
    WorkAreaRect := Forms.Screen.WorkAreaRect;
  end;
  {$ELSE}

{$IFDEF LINUX}
  // With Linux, the screen area and the available work area are the same.
  // Use the CLX screen object to determine them.
  WorkAreaRect.Top := 0;
  WorkAreaRect.Left := 0;
  WorkAreaRect.Right := Screen.Width;
  WorkAreaRect.Bottom := Screen.Height;

  {$ELSE}
  Assert(False);

  {$ENDIF}

  {$ENDIF}
  //  QWidget_frameGeometry(Handle, @AFrameGeom);
  //  FrameHeight := AFrameGeom.Bottom - AFrameGeom.Top;
  //  FrameWidth := AFrameGeom.Right - AFrameGeom.Left;
  FrameHeight := Height;
  FrameWidth := Width;
  // Try to make sure the form is never off the screen.
  // However, this does not resize the form to fit it on the screen.
  // If the form is wider or taller than the screen, some of it will
  // be off the screen.
  if Left + FrameWidth > WorkAreaRect.Right then
  begin
    Left := WorkAreaRect.Right - FrameWidth;
  end;
  if Top + FrameHeight > WorkAreaRect.Bottom then
  begin
    Top := WorkAreaRect.Bottom - FrameHeight;
  end;
  if Left < WorkAreaRect.Left then
  begin
    Left := WorkAreaRect.Left;
  end;
  if Top < WorkAreaRect.Top then
  begin
    Top := WorkAreaRect.Top;
  end;
  CustomizeControls;
end;

procedure TfrmCustomGoPhast.UpdateStringTreeViewCheckedState(
  TreeView: TVirtualStringTree; BaseNode: PVirtualNode; NewState: TCheckState);
var
  ChildNode: PVirtualNode;
  ChildIndex: Integer;
begin
  ChildNode := nil;
  for ChildIndex := 0 to BaseNode.ChildCount - 1 do
  begin
    if ChildIndex = 0 then
    begin
      ChildNode := TreeView.GetFirstChild(BaseNode);
    end
    else
    begin
      ChildNode := TreeView.GetNextSibling(ChildNode);
    end;
    if TreeView.Selected[ChildNode] and (ChildNode.CheckState <> NewState) then
    begin
      ChildNode.CheckState := NewState;
      if Assigned(TreeView.OnChecked) then
      begin
        TreeView.OnChecked(TreeView, ChildNode);
      end;
    end;
    if ChildNode.ChildCount > 0 then
    begin
      UpdateStringTreeViewCheckedState(TreeView, ChildNode, NewState);
    end;
  end;
end;

procedure TfrmCustomGoPhast.UpdateSubComponents(AComponent: TComponent);
var
  ComponentIndex: Integer;
  Component: TComponent;
  AStringGrid: TStringGrid;
  RowHeight: Integer;
  Grid: TCustomRBWDataGrid;
  ColGrid: TRbwDataGrid4;
  Index: Integer;
  RowGrid: TRbwRowDataGrid;
begin
  for ComponentIndex := 0 to AComponent.ComponentCount - 1 do
  begin
    Component := AComponent.Components[ComponentIndex];
    if Component is TStringGrid then
    begin
      AStringGrid := TStringGrid(Component);
      AStringGrid.Font := Font;
      // This is a work-around for a bug in CLX.
//      TStringGridCrack(AStringGrid).ShowEditor;
//      AStringGrid.Font.Size := AStringGrid.Font.Size + 1;
//      AStringGrid.Font.Size := AStringGrid.Font.Size - 1;
      // Make sure the rows are tall enough to show the text.
      RowHeight := AStringGrid.Canvas.TextHeight('0');
      if RowHeight > AStringGrid.DefaultRowHeight then
      begin
        AStringGrid.DefaultRowHeight := RowHeight;
      end;
      AStringGrid.FixedColor := Color;
    end;
    if Component is TCustomRBWDataGrid then
    begin
      Grid := TCustomRBWDataGrid(Component);
      Grid.UnselectableColor := Color;
      if Grid is TRbwDataGrid4 then
      begin
        ColGrid := TRbwDataGrid4(Grid);
        for Index := 0 to ColGrid.ColCount - 1 do
        begin
          ColGrid.Columns[Index].ButtonFont := Font;
        end;
      end
      else
      begin
        RowGrid := Grid as TRbwRowDataGrid;
        for Index := 0 to RowGrid.RowCount - 1 do
        begin
          RowGrid.Rows[Index].ButtonFont := Font;
        end;
      end;
    end;
    if Component is TJvRollOut then
    begin
      TJvRollOut(Component).ButtonFont := Font;
    end;
    UpdateSubComponents(Component);
  end;
end;

procedure TfrmCustomGoPhast.SetAppearance;
var
  AComponent: TComponent;
begin
  if GlobalFont <> nil then
  begin
    Font := GlobalFont;
    Color := GlobalColor;
//    Icon := Application.Icon;
    AComponent := self;
    UpdateSubComponents(AComponent);
  end;
end;

procedure TfrmCustomGoPhast.Show;
begin
  inherited;
  if WindowState = wsMinimized then
  begin
    WindowState := wsNormal
  end;
end;

procedure TfrmCustomGoPhast.FormShow(Sender: TObject);
begin
  GetFormatSettings;
  if FMonitorNumber >= 0 then
  begin
    Left := FLeft + Left;
    Top := FTop + Top;
  end;
  EnsureFormVisible;
  FixGridEditorPosition(Self);
end;

procedure TfrmCustomGoPhast.MouseClick;
begin
  Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
end;

procedure TfrmCustomGoPhast.AdjustFormPosition(
  DesiredPosition: TDesiredPosition);
var
  NewLeft: integer;
  NewTop: integer;
  WorkAreaRect: TRect;
  FormHeight: integer;
  FormWidth: integer;
  MainFormWidth: integer;
  WorkAreaWidth: integer;
  WorkAreaHeight: integer;
  function RoomToLeft: boolean;
  begin
    result := Application.MainForm.Left - FormWidth >= 0;
  end;
  function RoomToRight: boolean;
  begin
    result := Application.MainForm.Left + MainFormWidth
      + FormWidth <= WorkAreaWidth;
  end;
begin
  Assert(Application.MainForm <> nil);

{$IFDEF LINUX}
  //if not QWidget_isVisible(Widget) then Exit;
{$ENDIF}

{$IFDEF MSWINDOWS}
  // Because the CLX Screen.Height doesn't take the taskbar into account,
  // use the VCL Screen object instead under windows to determine the available
  // space on the screen.
  WorkAreaRect := Application.MainForm.Monitor.WorkareaRect;
{$ELSE}
{$IFDEF LINUX}
  // With Linux, the screen area and the available work area are the same.
  // Use the CLX screen object to determine them.
  WorkAreaRect.Top := 0;
  WorkAreaRect.Left := 0;
  WorkAreaRect.Right := Screen.Width;
  WorkAreaRect.Bottom := Screen.Height;
{$ELSE}
  Assert(False);
{$ENDIF}
{$ENDIF}
  WorkAreaWidth := WorkAreaRect.Right - WorkAreaRect.Left;
  WorkAreaHeight := WorkAreaRect.Bottom - WorkAreaRect.Top;

  // The height of the form is the same as the Client height (a bug).
  // We need the full height instead.
//  QWidget_frameGeometry(Handle, @FormGeom);

//  FormHeight := FormGeom.Bottom - FormGeom.Top;
//  FormWidth := FormGeom.Right - FormGeom.Left;
  FormHeight := Height;
  FormWidth := Width;

//  QWidget_frameGeometry(Application.MainForm.Handle, @FormGeom);

//  MainFormHeight := FormGeom.Bottom - FormGeom.Top;
//  MainFormWidth := FormGeom.Right - FormGeom.Left;
//  MainFormHeight := Application.MainForm.Height;
  MainFormWidth := Application.MainForm.Width;

  // Change the desired position if there isn't enough room on the desired size
  // but there is enough room on the opposite side.
  case DesiredPosition of
    dpLeft:
      begin
        if not RoomToLeft and RoomToRight then
        begin
          DesiredPosition := dpRight;
        end;
      end;
    dpRight:
      begin
        if not RoomToRight and RoomToLeft then
        begin
          DesiredPosition := dpLeft;
        end;
      end;
    dpBottomLeft:
      begin
        if not RoomToLeft and RoomToRight then
        begin
          DesiredPosition := dpBottomRight;
        end;
      end;
    dpBottomRight:
      begin
        if not RoomToRight and RoomToLeft then
        begin
          DesiredPosition := dpBottomLeft;
        end;
      end;
  else
    Assert(False);
  end;

  // Get the new left coordinate of the form.
  NewLeft := 0;
  case DesiredPosition of
    dpLeft, dpBottomLeft:
      begin
        NewLeft := Application.MainForm.Left - FormWidth - 10;
        if NewLeft < 0 then
        begin
          NewLeft := 0;
        end;
      end;
    dpRight, dpBottomRight:
      begin
        NewLeft := Application.MainForm.Left + MainFormWidth + 10;
        if NewLeft + FormWidth > WorkAreaWidth then
        begin
          NewLeft := WorkAreaWidth - FormWidth;
        end;
      end;
    else
      Assert(False);
  end;

  NewTop := 0;
  case DesiredPosition of
    dpLeft, dpRight:
      begin
        NewTop := Application.MainForm.Top;
      end;
    dpBottomLeft, dpBottomRight:
      begin
        NewTop := Application.MainForm.Top
          + Application.MainForm.Height
          - FormHeight;
      end;
    else Assert(False);
  end;
  // Get the new top coordinate of the form.
  if NewTop + FormHeight > WorkAreaHeight then
  begin
    NewTop := WorkAreaHeight - FormHeight;
  end;

  if NewTop < 0 then
  begin
    NewTop := 0;
  end;

  // Set the forms position.
  Position := poDesigned;
  Left := NewLeft;
  Top := NewTop;
end;

procedure EmphasizeCheckBoxes(
  const CheckBoxArray: array of TCheckBox);
var
  Index: integer;
  AnyChecked: boolean;
begin
  for Index := 0 to Length(CheckBoxArray) -1 do
  begin
    CheckBoxArray[Index].ParentFont := True;
  end;
  AnyChecked := False;
  for Index := 0 to Length(CheckBoxArray) -1 do
  begin
    AnyChecked := (CheckBoxArray[Index].State in [cbChecked, cbGrayed]);
    if AnyChecked then
    begin
      break;
    end;
  end;
  if not AnyChecked then
  begin
    for Index := 0 to Length(CheckBoxArray) -1 do
    begin
      CheckBoxArray[Index].Font.Color := clRed;
      CheckBoxArray[Index].Font.Style
        := CheckBoxArray[Index].Font.Style + [fsBold];
    end;
  end;
end;

//procedure TfrmCustomGoPhast.btnHelpClick(Sender: TObject);
//begin
//  inherited;
//  Application.HelpJump(HelpKeyword);
//end;

//procedure TfrmCustomGoPhast.SetSpinColor;
//var
//  Index: integer;
//  SpinEdit: TSpinEdit;
//begin
//  for Index := 0 to ComponentCount - 1 do
//  begin
//    if Components[Index] is TSpinEdit then
//    begin
//      SpinEdit := TSpinEdit(Components[Index]);
//      if SpinEdit.Enabled then
//      begin
//        SpinEdit.Color := clWindow;
//      end
//      else
//      begin
//        SpinEdit.Color := clBtnFace;
//      end;
//    end;
//  end;
//end;

procedure LayoutControls(Grid: TRbwDataGrid4; Control: TControl;
      ALabel: TLabel; Column: Integer; ControlOffSet: integer = 0);
var
  Rect: TRect;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  if (Control = nil) and (ALabel = nil) then
  begin
    Exit;
  end;
  if Column >= Grid.ColCount then
  begin
    Exit;
  end;
  if Grid.RowCount = 0 then
  begin
    Exit;
  end;
  Rect := Grid.CellRect(Column, 0);
  if ((Rect.Left = 0) and (Rect.Right = 0))
    or (Rect.Right - Rect.Left <> Grid.ColWidths[Column]) then
  begin
    if Control <> nil then
    begin
      Control.Visible := False;
    end;
    if ALabel <> nil then
    begin
      ALabel.Visible := False;
    end;
  end
  else
  begin
    if Control <> nil then
    begin
      Control.Visible := True;
    end;
    if ALabel <> nil then
    begin
      ALabel.Visible := True;
    end;
  end;
  if Control <> nil then
  begin
    Control.Left := Grid.Left + Rect.Left + ControlOffSet;
    Control.Width := Grid.ColWidths[Column] - ControlOffSet;
    if Control.Visible and (Control is TJvImageComboBox) then
    begin
      TJvImageComboBox(Control).DroppedWidth := Control.Width;
    end;
  end;
  if ALabel <> nil then
  begin
    ALabel.Left := Grid.Left + Rect.Left;
    ALabel.Width := Grid.ColWidths[Column];
  end;
end;

procedure EnableMultiEditControl(Grid: TRbwDataGrid4;
  AControl: TControl; Col: integer);
var
  ShouldEnable: boolean;
  Index: Integer;
begin
  ShouldEnable := False;
  for Index := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Col,Index);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  AControl.Enabled := ShouldEnable;
end;

procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
  Cols: array of integer); overload;
var
  ShouldEnable: boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  Col: Integer;
begin
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColIndex := 0 to Length(Cols) - 1 do
    begin
      Col := Cols[ColIndex];
      ShouldEnable := Grid.IsSelectedCell(Col,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  AControl.Enabled := ShouldEnable;
end;


procedure ChangeSelectedCellsInColumn(Grid: TRbwDataGrid4;
  const Column: integer; const NewText: string);
var
  Index: Integer;
  TempOptions: Grids.TGridOptions;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  for Index := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    if Grid.IsSelectedCell(Column, Index) then
    begin
      Grid.Cells[Column, Index] := NewText;
      if Assigned(Grid.OnSetEditText) then
      begin
        Grid.OnSetEditText(Grid,Column,Index, NewText);
      end;
    end;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
end;

procedure ChangeSelectedCellsStateInColumn(
  Grid: TRbwDataGrid4; const Column: integer; const NewState: TCheckBoxState);
var
  Index: Integer;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  for Index := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    if Grid.IsSelectedCell(Column, Index) then
    begin
      Grid.State[Column, Index] := NewState;
    end;
  end;
end;

procedure FillComboWithModelNames(Combo: TComboBox);
var
  ChildModel: TChildModel;
  Index: Integer;
begin
  Combo.Clear;
  Combo.AddItem(StrParentModel, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      if ChildModel <> nil then
      begin
        Combo.AddItem(ChildModel.ModelName, ChildModel);
      end;
    end;
  end;
  Combo.ItemIndex := 0;
end;


function TfrmCustomGoPhast.CanDisplayDataSet(DataArray: TDataArray): boolean;
begin
  result := False;
  case DataArray.EvaluatedAt of
    eaBlocks: result := True;
    eaNodes: result := frmGoPhast.PhastModel.ModelSelection
      in [msPhast, msSutra22, msSutra30, msSutra40];
    else Assert(False);
  end;
end;


procedure TfrmCustomGoPhast.FillComboBoxWithModels(ACombo: TComboBox);
var
  ChildModel: TChildModel;
  ChildIndex: integer;
begin
  ACombo.AddItem(StrParentModel, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    ACombo.Enabled := True;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ACombo.AddItem(ChildModel.ModelName, ChildModel);
      end;
    end;
  end
  else
  begin
    ACombo.Enabled := false;
  end;
  ACombo.ItemIndex := 0;
end;

procedure ClearGrid(Grid: TRbwDataGrid4);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
        Grid.Checked[ColIndex,RowIndex] := False;
        Grid.Objects[ColIndex,RowIndex] := nil;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure UpdateNextTimeCell(Grid: TRbwDataGrid4; ACol, ARow: Integer);
var
  SelectIndex: Integer;
begin
  if (ARow >= Grid.FixedRows + PestRowOffset) and (ACol in [0, 1])then
  begin
    SelectIndex := Grid.ItemIndex[ACol, ARow];
    if SelectIndex >= 0 then
    begin
      if (ACol = 0) then
      begin
        if Grid.Cells[1, ARow] = '' then
        begin
          Grid.ItemIndex[1, ARow] := SelectIndex;
          if Assigned(Grid.OnSetEditText) then
          begin
            Grid.OnSetEditText(Grid, 1, ARow, Grid.Cells[1, ARow]);
          end;
        end;
      end
      else if (ACol = 1) then
      begin
        if (ARow + 1 < Grid.RowCount) and
          (Grid.Cells[0, ARow + 1] = '') then
        begin
          if SelectIndex + 1 < Grid.Columns[0].PickList.Count then
          begin
            Grid.ItemIndex[0, ARow + 1] := SelectIndex + 1;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(Grid, 0, ARow + 1, Grid.Cells[0, ARow + 1]);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure UpdateColorScheme(Combo: TComboBox; PaintBox: TPaintBox);
var
  StoredIndex: integer;
  StoredName: string;
  index: Integer;
  NewIndex: Integer;
begin
  StoredIndex := Combo.ItemIndex;
  StoredName := Combo.Text;
  Combo.Items.Clear;
  Combo.Items.Add(StrRainbow);
  Combo.Items.Add(StrGreenToMagenta);
  Combo.Items.Add(StrBlueToRed);
  Combo.Items.Add(StrBlueToDarkOrange);
  Combo.Items.Add(StrBlueToGreen);
  Combo.Items.Add(StrBrownToBlue);
  Combo.Items.Add(StrBlueToGray);
  Combo.Items.Add(StrBlueToOrange);
  Combo.Items.Add(StrBlueToOrangeRed);
  Combo.Items.Add(StrLightBlueToDarkB);
  Combo.Items.Add(StrModifiedSpectralSc);
  Combo.Items.Add(StrSteppedSequential);

  for index := 0 to frmGoPhast.PhastModel.ColorSchemes.Count - 1 do
  begin
    Combo.Items.Add(frmGoPhast.PhastModel.ColorSchemes[index].Name);
  end;
  NewIndex := Combo.Items.IndexOf(StoredName);
  if (NewIndex < 0) then
  begin
    if StoredIndex < Combo.Items.Count then
    begin
      NewIndex := StoredIndex;
    end
    else
    begin
      NewIndex := 0
    end;
  end;
  Combo.ItemIndex := NewIndex;
  PaintBox.Invalidate;
end;

function TfrmCustomGoPhast.AskUserIfNewDataSet: Boolean;
var
  UsedResponse: TDatasetResponse;
begin
  if FCreateNewDataSet then
  begin
    UsedResponse := drNew;
  end
  else
  begin
    UsedResponse := drUpdate;
  end;
  UsedResponse := AskIfNewDataSet(FAskedUser, UsedResponse);
  result := UsedResponse = drNew;
  case UsedResponse of
    drNew:
      begin
        FCreateNewDataSet := True;
      end;
    drUpdate:
      begin
        FCreateNewDataSet := false;
      end;
    drAbort:
      begin
        raise EAbortingImport.Create('Aborting import of model results');
      end;
  else
    begin
      raise EAbortingImport.Create('Aborting import of model results');
    end;
  end;
end;

initialization

finalization
  KillApp('ModelMuse Help');

end.
