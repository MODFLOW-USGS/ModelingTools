{@abstract(The main purpose of @name is to define @link(TfrmImportBitmap)
  which is used to help import or edit bitmaps in GoPhast.)
  @name also defines @link(frmImportBitmap) which is used to
  communicate with @link(TfrmPixelPoint).}
unit frmImportBitmapUnit;

interface

uses
  System.UITypes, Windows,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Grids, RbwDataGrid4, 
  ExtCtrls, Buttons, CompressedImageUnit, ZoomBox2, Mask, JvExMask,
  JvSpin, pngimage;

type
  {@abstract(@name is used to help import or edit bitmaps in GoPhast.)
    See TfrmGoPhast.@link(TfrmGoPhast.miImportBitmapClick),
    TfrmGoPhast.@link(TfrmGoPhast.miEditBitmapsClick), and
    TfrmSelectImage.@link(TfrmSelectImage.btnOKClick).}
  TfrmImportBitmap = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    {@name is used to import ESRI *.wld files.
    See http://support.esri.com/index.cfm?fa=knowledgebase.techarticles.articleShow&d=16106

    The basic format is

    pixel-x,pixel-y real-world-x,real-world-y

    @unorderedList(
    @item(There is a space between each pair of coordinates.)
    @item(There is a comma (with no space) separating
    the X and X coordinates of each pair.)
    @item(There are two such points))
    }
    btnImportWorldFile: TButton;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // See @link(btnSelectImageClick).
    btnSelectImage: TButton;
    // @name: TCheckBox;
    // @name determines whether or not a bitmap will be visible or not.
    cbVisible: TCheckBox;
    // @name: TEdit;
    // @name holds the name for the bitmap.
    edName: TEdit;
    // @name: TLabel;
    // @name displays "Name".
    lblName: TLabel;
    // @name is used to select a .wld file.
    // @seealso(btnImportWorldFile).
    odWorldFiled: TOpenDialog;
    // @name: TOpenDialog;
    // @name is used to select the bitmap to import.
    OpenDialogBitmap: TOpenDialog;
    // @name: TPanel;
    // @name holds the buttons and other controls at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TRadioGroup;
    // @name determines whether the bitmap is to be displayed on the top,
    // front, or side view of the model.
    rgViewDirection: TRadioGroup;
    // @name: TSplitter;
    // @name is used to resize the relative areas of @link(dgPoints) and
    // @link(ZoomBox).
    Splitter1: TSplitter;
    dgPoints: TRbwDataGrid4;
    seNumRows: TJvSpinEdit;
    lblNumRows: TLabel;
    GridPanel1: TGridPanel;
    sbAddRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbDeleteRow: TSpeedButton;
    ScrollBox1: TScrollBox;
    ZoomBox: TQRbwZoomBox2;
    // @name imports a .wld file.  @seealso(btnImportWorldFile).
    procedure btnImportWorldFileClick(Sender: TObject);
    // Clicking @name causes the bitmap that is being dealt with to
    // either be imported into GoPhast or its properties to be edited.
    procedure btnOKClick(Sender: TObject);
    // @name uses @link(OpenDialogBitmap) to select a bitmap
    // which is then read into @link(FBitMap).
    procedure btnSelectImageClick(Sender: TObject);
    // @name causes @link(ZoomBox) to be redrawn and enables or disables
    // @link(btnOK).
    // See @link(DrawPointsOnBitMap32).
    procedure dgPointsExit(Sender: TObject);
    // @name causes @link(ZoomBox) to be redrawn and enables or disables
    // @link(btnOK).
    // See @link(DrawPointsOnBitMap32).
    procedure dgPointsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name destroys @link(FBitMap).
    procedure FormDestroy(Sender: TObject); override;
    // @name calls @link(LabelColumns).
    procedure rgViewDirectionClick(Sender: TObject);
    procedure ZoomBoxImage32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure seNumRowsChange(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure dgPointsEndUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // @name is the bitmap that is being imported or edited.
    FBitMap: TBitmap;
    // @name: TCompressedBitmapItem;
    // @name is used to store @link(FBitMap) in GoPhast.
    FBitmapItem: TCompressedBitmapItem;
    FImageFileName: String;
    FEditing: Boolean;
    // @name enables or disables @link(btnOK) depending on whether or not
    // an image has been selected and
    // enough points have been specified.
    procedure EnableOKButton;
    // @name imports a .wld file. @seealso(btnImportWorldFile).
    procedure ImportWorldFile(const FileName: string);
    // @name labels the columns in @link(dgPoints) depending on the direction
    // from which the bitmap will be viewed.
    // See @link(rgViewDirection).
    procedure LabelColumns;
    // @name numbers the rows in @link(dgPoints).
    procedure NumberRows;
    // @name stores @link(FBitMap) in a new or existing @link(FBitmapItem)
    // along with the associated coordinates of pixels
    // and real-world coordinates.
    procedure SetData;
    procedure DrawPointsOnBitMap32(Sender: TObject; Buffer: TBitmap32);
    { Private declarations }
  public
    // @name adds a point to dgPoints.
    procedure AddPoint(const PixelX, PixelY: integer;
      const RealX, RealY: double);
    // @name is called when editing a bitmap to retrieve data from ABitmapItem.
    // @name is not called when importing a bitmap.
    procedure GetData(ABitmapItem: TCompressedBitmapItem);
    { Public declarations }
  end;

var
  // @name is the current instance of @link(TfrmImportBitmap).
  // @name is set in @link(TfrmImportBitmap.ZoomBoxImage32MouseUp) while
  // a @link(TfrmPixelPoint) is in use.
  frmImportBitmap: TfrmImportBitmap;

implementation

uses frmPixelPointUnit, GoPhastTypes, frmGoPhastUnit, BigCanvasMethods,
  frmWorldFileTypeUnit, frmGoToUnit, jpeg,
  {$IFDEF Win32}
  GraphicEx, Pcx,
  {$ENDIF}
  ModelMuseUtilities, UndoItems;

resourcestring
  StrUnableToReadWorld = 'Unable to read world file. Check that the world fi' +
  'le is properly formatted.';
  StrThereWasAnErrorR = 'There was an error reading the file.  The file may ' +
  'be corrupt, it may be open in another program or there may be a bug in Mo' +
  'delMuse. Please contact rbwinst@usgs.gov for futher help with this proble' +
  'm.';
  StrPixelX = 'Pixel X';
  StrPixelY = 'Pixel Y';
  StrPixelZ = 'Pixel Z';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrClickOnTheImageA = 'Click on the image and assign real-world coordinate' +
  's to the points you clicked or fill in the information in the table.';
  StrEditBitMap = 'Edit BitMap';
  StrAllSupportedImage = 'All Supported Image Formats|*.bmp;*.jpg;*.jpeg;*.p' +
  'ng;*.wmf;*.emf;*.mng;|Bitmaps (*.bmp)|*.bmp|Jpeg images (*.jpg, *.jpeg)|*' +
  '.jpg;*.jpeg|Portable Network Graphics (*.png)|*.png|Enhanced Windows Meta' +
  'files (*.emf)|*.emf|Windows Metafiles (*.wmf)|*.wmf';
  StrSorryThereWasAP = 'Sorry, there was a problem reading this file.';
  StrThisFileDoesNotA = 'This file does not appear to be in the correct form' +
  'at. Please contact the software developer for help.';
  StrAnErrorOccurredWh = 'An error occurred while attempting to open %0:s. T' +
  'he error message was %1:s.';
  StrID = 'ID';

{$R *.dfm}

type
  // @name identifies the columns in @link(TfrmImportBitmap.dgPoints
  // TfrmImportBitmap.dgPoints).
  TPixelColumns = (pcNone, pcPixelX, pcPixelY, pcRealWorldX, pcRealWorldY, pcID);

  TUndoImportBitMap = class(TCustomUndo)
  private
    FNewBitmapItem: TCompressedBitmapItem;
    FIndex: Integer;
    procedure UpdateView;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    constructor Create;
  end;

  TUndoEditBitMap = class(TUndoImportBitMap)
  private
    FOldBitmapItem: TCompressedBitmapItem;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    constructor Create;
  end;

{ TfrmImportBitmap }

procedure TfrmImportBitmap.NumberRows;
var
  Row: integer;
begin
  for Row := 1 to dgPoints.RowCount - 1 do
  begin
    dgPoints.Cells[Ord(pcNone), Row] := IntToStr(Row);
  end;
end;

procedure TfrmImportBitmap.LabelColumns;
begin
  case TViewDirection(rgViewDirection.ItemIndex) of
    vdTop:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := StrPixelX;
        dgPoints.Cells[Ord(pcPixelY), 0] := StrPixelY;
        dgPoints.Cells[Ord(pcRealWorldX), 0] := StrX;
        dgPoints.Cells[Ord(pcRealWorldY), 0] := StrY;
      end;
    vdFront:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := StrPixelX;
        dgPoints.Cells[Ord(pcPixelY), 0] := StrPixelZ;
        dgPoints.Cells[Ord(pcRealWorldX), 0] := StrX;
        dgPoints.Cells[Ord(pcRealWorldY), 0] := StrZ;
      end;
    vdSide:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := StrPixelZ;
        dgPoints.Cells[Ord(pcPixelY), 0] := StrPixelY;
        dgPoints.Cells[Ord(pcRealWorldX), 0] := StrZ;
        dgPoints.Cells[Ord(pcRealWorldY), 0] := StrY;
      end;
  else
    Assert(False);
  end;
  dgPoints.Cells[Ord(pcID), 0] := StrID;
end;

procedure TfrmImportBitmap.FormCreate(Sender: TObject);
var
  ALayer: TPositionedLayer;
begin
  inherited;
  FImageFileName := '';
  dgPoints.ColWidths[Ord(pcNone)] := dgPoints.DefaultColWidth div 2;
  LabelColumns;
  NumberRows;

  ALayer := ZoomBox.Image32.Layers.Add(TPositionedLayer) as TPositionedLayer;
  ALayer.OnPaint := DrawPointsOnBitMap32;
end;

procedure TfrmImportBitmap.dgPointsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  Index: Integer;
begin
  inherited;
  for Index := 1 to dgPoints.RowCount - 1 do
  begin
    dgPoints.Cells[0,Index] := IntToStr(Index);
  end;
  EnableOKButton;
  ZoomBox.InvalidateImage32;
end;

procedure TfrmImportBitmap.dgPointsEndUpdate(Sender: TObject);
begin
  inherited;
  if seNumRows <> nil then
  begin
    seNumRows.AsInteger := dgPoints.RowCount -1;
  end;
end;

procedure TfrmImportBitmap.dgPointsExit(Sender: TObject);
begin
  inherited;
  EnableOKButton;
  ZoomBox.InvalidateImage32;
end;

procedure TfrmImportBitmap.EnableOKButton;
var
  ShouldEnable: boolean;
  Count: integer;
  Row: integer;
begin
  ShouldEnable := (FBitMap <> nil) and (dgPoints.RowCount >= 3);
  if ShouldEnable then
  begin
    Count := 0;
    for Row := 1 to dgPoints.RowCount -1 do
    begin
      if (Trim(dgPoints.Cells[Ord(pcPixelX),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcPixelY),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcRealWorldX),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcRealWorldY),Row]) <> '') then
      begin
        Inc(Count);
        ShouldEnable := Count >= 2;
        if ShouldEnable then
        begin
          break;
        end;
      end;
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportBitmap.btnSelectImageClick(Sender: TObject);
var
  WorldFile: string;
  Extension: string;
  MetaFile : TMetafile;
  WorldFileNames: TStringList;
  Index: Integer;
  jpegImage: TJPEGImage;
  png: TPngImage;
{$IFDEF Win32}
  TiffImage: TTIFFGraphic;
{$ENDIF}
  procedure ShowError;
  begin
    FreeAndNil(FBitMap);
    Beep;
    MessageDlg(StrThereWasAnErrorR, mtError, [mbOK], 0);
  end;
begin
  inherited;
  {$IFNDEF Win32}
  OpenDialogBitmap.Filter := StrAllSupportedImage;
  {$ENDIF}
  if OpenDialogBitmap.Execute then
  begin
    FBitMap.Free;
    FBitMap := TBitmap.Create;

    FImageFileName := OpenDialogBitmap.FileName;
    Extension := ExtractFileExt(FImageFileName);
    if (CompareText(Extension, '.wmf') = 0)
      or (CompareText(Extension, '.emf') = 0) then
    begin
{ TODO : It would be better to use the metafile directly
instead of converting it to a bitmap so
there is no loss in resolution at higher magnifications. }
      MetaFile := TMetafile.Create;
      try
        try
          MetaFile.LoadFromFile(OpenDialogBitmap.FileName);
          FBitMap.Height := Metafile.Height;
          FBitMap.Width := Metafile.Width;
          FBitMap.Canvas.Draw(0, 0, MetaFile) ;
        except
          on E: Exception do
          begin
            ShowError;
            Exit;
          end;
        end;
      finally
        MetaFile.Free;
      end;
    end
    else if (CompareText(Extension, '.bmp') = 0) then
    begin
      try
        FBitMap.LoadFromFile(FImageFileName);
      except
        on EFOpenError do
        begin
          CantOpenFileMessage(FImageFileName);
          Exit;
        end;
        on E: Exception do
        begin
          MessageDlg(Format(StrAnErrorOccurredWh, [FImageFileName, E.message]), mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end
    else if (CompareText(Extension, '.jpg') = 0)
      or (CompareText(Extension, '.jpeg') = 0) then
    begin
      jpegImage := TJPEGImage.Create;
      try
        try
          jpegImage.LoadFromFile(FImageFileName);
        except
          on EFOpenError do
          begin
            CantOpenFileMessage(FImageFileName);
            Exit;
          end;
          on E: Exception do
          begin
            MessageDlg(Format(StrAnErrorOccurredWh, [FImageFileName, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;
        FBitMap.Assign(jpegImage);
      finally
        jpegImage.Free
      end;
    end
    else if (CompareText(Extension, '.png') = 0) then
    begin
      png := TPngImage.Create;
      try
        try
          png.LoadFromFile(FImageFileName);
        except
          on EFOpenError do
          begin
            CantOpenFileMessage(FImageFileName);
            Exit;
          end;
          on E: Exception do
          begin
            MessageDlg(Format(StrAnErrorOccurredWh, [FImageFileName, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;
        FBitMap.Assign(png);
      finally
        png.Free;
      end;
    end
{$IFDEF Win32}
    else if (CompareText(Extension, '.pcx') = 0) then
    begin
      try
        LoadFromFileX(FImageFileName, FBitMap);
      except
        on EFOpenError do
        begin
          CantOpenFileMessage(FImageFileName);
          Exit;
        end;
        on E: Exception do
        begin
          MessageDlg(Format(StrAnErrorOccurredWh, [FImageFileName, E.message]), mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end
    else if (CompareText(Extension, '.tif') = 0)
      or (CompareText(Extension, '.tiff') = 0) then
    begin
      TiffImage := TTIFFGraphic.Create;
      try
        try
          TiffImage.LoadFromFile(FImageFileName);
        except
          on EFOpenError do
          begin
            CantOpenFileMessage(FImageFileName);
            Exit;
          end;
          on E: Exception do
          begin
            MessageDlg(Format(StrAnErrorOccurredWh, [FImageFileName, E.message]), mtError, [mbOK], 0);
            Exit;
          end;
        end;
        FBitMap.Assign(TiffImage);
      finally
        TiffImage.Free;
      end;
    end
{$ENDIF}
    else
    begin
      Beep;
      MessageDlg(StrSorryThereWasAP, mtError, [mbOK], 0);
      Exit;
    end;

    try
      ZoomBox.Image32.Bitmap.Assign(FBitMap);
      ZoomBox.InvalidateImage32;
      ZoomBox.Width := FBitMap.Width;
      ZoomBox.Height := FBitMap.Height;
    except on Exception do
      begin
        ShowError;
        ZoomBox.Image32.Bitmap := nil;
      end;

    end;

    edName.Text := ExtractFileName(OpenDialogBitmap.FileName);
    WorldFileNames := TStringList.Create;
    try
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.jgw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.jpgw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.tfw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.tifw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.wld'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.pgw'));
      for Index := 0 to WorldFileNames.Count - 1 do
      begin
        WorldFile := WorldFileNames[Index];
        if FileExists(WorldFile) then
        begin
          ImportWorldFile(WorldFile);
          break;
        end
      end;
    finally
      WorldFileNames.Free;
    end;

    EnableOKButton;
    if not btnOK.Enabled then
    begin
      MessageDlg(StrClickOnTheImageA, mtInformation, [mbOK], 0);
    end;
  end;
end;

procedure TfrmImportBitmap.FormDestroy(Sender: TObject);
begin
  inherited;
  FBitMap.Free;
end;

procedure TfrmImportBitmap.FormShow(Sender: TObject);
begin
  inherited;
  rgViewDirection.Buttons[Ord(vdSide)].Enabled :=
    not (frmGoPhast.ModelSelection in SutraSelection)
end;

procedure TfrmImportBitmap.GetData(ABitmapItem: TCompressedBitmapItem);
var
  Index: integer;
  MeasurementPoint: TMeasurementPointItem;
begin
  FEditing := True;
  Caption := StrEditBitMap;
  Assert(ABitmapItem <> nil);
  FBitmapItem := ABitmapItem;
  FBitMap.Free;
  FBitMap := TBitmap.Create;
  FBitMap.Assign(FBitmapItem.BitMap);
  ZoomBox.Image32.Bitmap.Assign(FBitMap);
  ZoomBox.Width := FBitMap.Width;
  ZoomBox.Height := FBitMap.Height;

  rgViewDirection.ItemIndex := Ord(FBitmapItem.ViewDirection);

  edName.Text := FBitmapItem.Name;
  seNumRows.AsInteger := ABitmapItem.MeasurementPoints.Count;
  dgPoints.RowCount := ABitmapItem.MeasurementPoints.Count + 1;
  for Index := 0 to ABitmapItem.MeasurementPoints.Count - 1 do
  begin
    MeasurementPoint := ABitmapItem.MeasurementPoints.Items[Index]
      as TMeasurementPointItem;
    dgPoints.Cells[Ord(pcPixelX), Index + 1] := IntToStr(MeasurementPoint.PixelX);
    dgPoints.Cells[Ord(pcPixelY), Index + 1] := IntToStr(MeasurementPoint.PixelY);
    dgPoints.Cells[Ord(pcRealWorldX), Index + 1] := FloatToStr(MeasurementPoint.X);
    dgPoints.Cells[Ord(pcRealWorldY), Index + 1] := FloatToStr(MeasurementPoint.Y);
    dgPoints.Cells[Ord(pcID), Index + 1] := MeasurementPoint.ID;
  end;
  cbVisible.Checked := ABitmapItem.Visible;
  btnOK.Enabled := True;
end;

procedure TfrmImportBitmap.SetData;
var
  Index: integer;
  MeasurementPoint: TMeasurementPointItem;
  XPixel, YPixel: integer;
  X, Y: double;
  NewItem: boolean;
  UndoEdit: TUndoImportBitMap;
  ID: string;
begin
  if FImageFileName <> '' then
  begin
    frmGoPhast.PhastModel.AddFileToArchive(FImageFileName);
  end;
  UndoEdit := nil;
  try
  if FBitmapItem = nil then
  begin
    UndoEdit := TUndoImportBitMap.Create;
    FBitmapItem := UndoEdit.FNewBitmapItem;
//    NewItem := True;
  end
  else
  begin
    UndoEdit := TUndoEditBitMap.Create;
    TUndoEditBitMap(UndoEdit).FOldBitmapItem.Assign(FBitmapItem);
    UndoEdit.FIndex := FBitmapItem.Index;
  end;
  FBitmapItem.ViewDirection := TViewDirection(rgViewDirection.ItemIndex);
  FBitmapItem.BitMap.Assign(FBitMap);

  FBitmapItem.Name := edName.Text;
  FBitmapItem.Visible := cbVisible.Checked;

  FBitmapItem.MeasurementPoints.Clear;
  for Index := 1 to dgPoints.RowCount - 1 do
  begin
    if (dgPoints.Cells[Ord(pcPixelX), Index] = '')
      or (dgPoints.Cells[Ord(pcPixelY), Index] = '')
      or (dgPoints.Cells[Ord(pcRealWorldX), Index] = '')
      or (dgPoints.Cells[Ord(pcRealWorldY), Index] = '') then
    begin
      Continue;
    end;
    // initialize variables to prevent a compiler warning.
    XPixel := 0;
    YPixel := 0;
    X := 0;
    Y := 0;
    ID := dgPoints.Cells[Ord(pcID), Index];

    if not TryStrToInt(dgPoints.Cells[Ord(pcPixelX), Index], XPixel) then
    begin
      Continue;
    end;
    if not TryStrToInt(dgPoints.Cells[Ord(pcPixelY), Index], YPixel) then
    begin
      Continue;
    end;

    if not TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldX), Index], X) then
    begin
      Continue;
    end;
    if not TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldY), Index], Y) then
    begin
      Continue;
    end;

    MeasurementPoint := FBitmapItem.MeasurementPoints.Add
      as TMeasurementPointItem;
    MeasurementPoint.PixelX := XPixel;
    MeasurementPoint.PixelY := YPixel;
    MeasurementPoint.X := X;
    MeasurementPoint.Y := Y;
    MeasurementPoint.ID := ID;
  end;
  case FBitmapItem.ViewDirection of
    vdTop:
      begin
        frmGoPhast.TopScreenObjectsChanged := True;
      end;
    vdFront:
      begin
        frmGoPhast.FrontScreenObjectsChanged := True;
      end;
    vdSide:
      begin
        frmGoPhast.SideScreenObjectsChanged := True;
      end;
  else
    Assert(False);
  end;
  if FEditing then
  begin
    UndoEdit.FNewBitmapItem.Assign(FBitmapItem);
  end
  else
  begin
    MoveToImage(FBitmapItem);
  end;
  except on E: Exception do
    begin
      UndoEdit.Free;
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
  frmGoPhast.UndoStack.Submit(UndoEdit);
  //frmGoPhast.Invalidate;
end;

procedure TfrmImportBitmap.sbAddRowClick(Sender: TObject);
begin
  inherited;
  seNumRows.AsInteger := seNumRows.AsInteger +1;
  seNumRows.OnChange(seNumRows);
end;

procedure TfrmImportBitmap.sbDeleteRowClick(Sender: TObject);
begin
  inherited;
  if (dgPoints.Row >= dgPoints.FixedRows)
    and (dgPoints.RowCount > 2) then
  begin
    dgPoints.DeleteRow(dgPoints.Row);
    seNumRows.AsInteger := dgPoints.RowCount-1;
    NumberRows;
  end;
end;

procedure TfrmImportBitmap.sbInsertRowClick(Sender: TObject);
begin
  inherited;
  if (dgPoints.Row >= dgPoints.FixedRows)
    and (dgPoints.Row < dgPoints.RowCount) then
  begin
    dgPoints.InsertRow(dgPoints.Row);
    seNumRows.AsInteger := dgPoints.RowCount-1;
    NumberRows;
  end;
end;

procedure TfrmImportBitmap.seNumRowsChange(Sender: TObject);
begin
  inherited;
  dgPoints.RowCount := seNumRows.AsInteger + 1;
  NumberRows;
end;

procedure TfrmImportBitmap.ZoomBoxImage32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  frmPixelPoint: TfrmPixelPoint;
  AValue: double;
begin
  inherited;
  if (dgPoints.SelectedRow > 0)
    and (dgPoints.Cells[Ord(pcPixelX), dgPoints.SelectedRow] = '')
    and (dgPoints.Cells[Ord(pcPixelY), dgPoints.SelectedRow] = '')
    and TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldX), dgPoints.SelectedRow], AValue)
    and TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldY), dgPoints.SelectedRow], AValue)
    then
  begin
    dgPoints.Cells[Ord(pcPixelX), dgPoints.SelectedRow] := IntToStr(X);
    dgPoints.Cells[Ord(pcPixelY), dgPoints.SelectedRow] := IntToStr(Y);
    ZoomBox.InvalidateImage32;
  end
  else
  begin
    frmImportBitmap := self;
    try
      Application.CreateForm(TfrmPixelPoint, frmPixelPoint);
      try
        frmPixelPoint.PopupParent := self;
        frmPixelPoint.GetData(TViewDirection(rgViewDirection.ItemIndex), X, Y);
        if frmPixelPoint.ShowModal = mrOK then
        begin
          ZoomBox.InvalidateImage32;
        end;
      finally
        frmPixelPoint.Free;
      end;
    finally
      frmImportBitmap := nil;
    end;
  end;
end;

procedure TfrmImportBitmap.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportBitmap.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  LabelColumns;
end;

Type
  TWorldFileType = (wftCAD, sftRaster);

procedure TfrmImportBitmap.ImportWorldFile(const FileName: string);
  function ExtractWord(Var AString: string): string;
  const
    Space = ' ';
    Comma = ',';
  var
    SpacePosition: integer;
    CommaPosition: integer;
  begin
    try
    SpacePosition := Pos(Space,AString);
    CommaPosition := Pos(Comma,AString);
    if (SpacePosition > 0) and (CommaPosition > 0) then
    begin
      if SpacePosition < CommaPosition then
      begin
        Result := Copy(AString, 1, SpacePosition-1);
        AString := Copy(AString, SpacePosition+1, MAXINT);
      end
      else
      begin
        Result := Copy(AString, 1, CommaPosition-1);
        AString := Copy(AString, CommaPosition+1, MAXINT);
      end;
    end
    else if (SpacePosition > 0) then
    begin
      Result := Copy(AString, 1, SpacePosition-1);
      AString := Copy(AString, SpacePosition+1, MAXINT);
    end
    else if (CommaPosition > 0) then
    begin
      Result := Copy(AString, 1, CommaPosition-1);
      AString := Copy(AString, CommaPosition+1, MAXINT);
    end
    else
    begin
      Result := AString;
      AString := '';
    end;
    finally
      result := Trim(Result);
    end;
  end;
var
  WorldFile: TStringList;
  LineIndex: integer;
  Line: string;
  PixelX: integer;
  PixelY: integer;
  RealWorldX: double;
  RealWorldY: double;
  Extension: string;
  FileType: TWorldFileType;
  A, B, C, D, E, F: double;
begin
  inherited;
  Extension := LowerCase(ExtractFileExt(FileName));
  if Extension = '.wld' then
  begin
    FileType := wftCAD;
  end
  else if (Extension = '.tfw')
    or (Extension = '.tifw')
    or (Extension = '.jgw')
    or (Extension = '.jpgw')
    or (Extension = '.pgw')
     then
  begin
    FileType := sftRaster;
  end
  else
  begin
    with TfrmWorldFileType.Create(nil) do
    begin
      try
        ShowModal;
        if rbRaster.Checked then
        begin
          FileType := sftRaster;
        end
        else if rbCAD.Checked then
        begin
          FileType := wftCAD;
        end
        else
        begin
          Exit;
        end;
      finally
        Free;
      end;
    end;
  end;
  WorldFile := TStringList.Create;
  try
    try
      WorldFile.LoadFromFile(FileName);
    except on EFOpenError do
      begin
        CantOpenFileMessage(FileName);
        Exit;
      end;
    end;
    try
      case FileType of
        wftCAD:
          begin
            dgPoints.BeginUpdate;
            try
              for LineIndex := 0 to WorldFile.Count -1 do
              begin
                Line := WorldFile[LineIndex];
                if Line <> '' then
                begin
                  PixelX := Round(FortranStrToFloat(ExtractWord(Line)));
                  PixelY := Round(FortranStrToFloat(ExtractWord(Line)));
                  RealWorldX := FortranStrToFloat(ExtractWord(Line));
                  RealWorldY := FortranStrToFloat(ExtractWord(Line));
                  AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
                end;
              end;
            finally
              dgPoints.EndUpdate;
            end;
          end;
        sftRaster:
          begin
            if WorldFile.Count < 6 then
            begin
              Beep;
              MessageDlg(StrThisFileDoesNotA, mtError, [mbOK], 0);
              Exit;
            end;
            Assert(WorldFile.Count >= 6);
            A := FortranStrToFloat(Trim(WorldFile[0]));
            D := FortranStrToFloat(Trim(WorldFile[1]));
            B := FortranStrToFloat(Trim(WorldFile[2]));
            E := FortranStrToFloat(Trim(WorldFile[3]));
            C := FortranStrToFloat(Trim(WorldFile[4]));
            F := FortranStrToFloat(Trim(WorldFile[5]));
            PixelX := 0;
            PixelY := 0;
            RealWorldX := A*PixelX + B*PixelY + C;
            RealWorldY := D*PixelX + E*PixelY + F;
            dgPoints.BeginUpdate;
            try
              AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
              if FBitMap <> nil then
              begin
                PixelX := FBitMap.Width-1;
                PixelY := FBitMap.Height-1;
                RealWorldX := A*PixelX + B*PixelY + C;
                RealWorldY := D*PixelX + E*PixelY + F;
                AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
              end;
            finally
              dgPoints.EndUpdate
            end;
          end;
        else Assert(False);
      end;
    except on EConvertError do
      begin
        Beep;
        MessageDlg(StrUnableToReadWorld, mtError, [mbOK], 0);
      end;
    end;
  finally
    WorldFile.Free;
  end;
end;

procedure TfrmImportBitmap.btnImportWorldFileClick(Sender: TObject);
begin
  inherited;
  if odWorldFiled.Execute then
  begin
    ImportWorldFile(odWorldFiled.FileName);
  end;
end;

procedure TfrmImportBitmap.AddPoint(const PixelX, PixelY: integer;
  const RealX, RealY: double);
var
  Row: integer;
begin
  if (seNumRows.AsInteger <= 1) and
    ((dgPoints.Cells[Ord(pcPixelX), 1] = '')
    or (dgPoints.Cells[Ord(pcPixelY), 1] = '')
    or (dgPoints.Cells[Ord(pcRealWorldX), 1] = '')
    or (dgPoints.Cells[Ord(pcRealWorldY), 1] = '')) then
  begin
    Row := 1;
  end
  else
  begin
    Row := dgPoints.RowCount;
    dgPoints.RowCount := Row + 1;
  end;
  dgPoints.Cells[Ord(pcPixelX), Row] := IntToStr(PixelX);
  dgPoints.Cells[Ord(pcPixelY), Row] := IntToStr(PixelY);
  dgPoints.Cells[Ord(pcRealWorldX), Row] := FloatToStr(RealX);
  dgPoints.Cells[Ord(pcRealWorldY), Row] := FloatToStr(RealY);
  NumberRows;
  EnableOKButton;
  ZoomBox.InvalidateImage32;
end;

procedure TfrmImportBitmap.DrawPointsOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Row: integer;
  X, Y: integer;
begin
  Buffer.BeginUpdate;
  try
    for Row := 1 to dgPoints.RowCount - 1 do
    begin
      if (dgPoints.Cells[Ord(pcPixelX), Row] <> '')
        and (dgPoints.Cells[Ord(pcPixelY), Row] <> '') then
      begin
        try
          X := StrToInt(dgPoints.Cells[Ord(pcPixelX), Row]);
          Y := StrToInt(dgPoints.Cells[Ord(pcPixelY), Row]);
          DrawBigRectangle32(Buffer, clBlack32, clBlack32, 0,
            X - 3, Y - 3, X + 3, Y + 3);
        except on EConvertError do
          begin
            Continue;
          end
        end;
      end;
    end;
  finally
    Buffer.EndUpdate
  end;
end;

{ TUndoEditBitMap }

constructor TUndoEditBitMap.Create;
begin
  inherited;
  FOldBitmapItem := TCompressedBitmapItem.Create(nil);
end;

function TUndoEditBitMap.Description: string;
begin
  result := 'edit bitmap';
end;

procedure TUndoEditBitMap.DoCommand;
var
  BitMapItem: TCollectionItem;
begin
//  inherited;
  BitMapItem := frmGoPhast.PhastModel.Bitmaps.Items[FIndex];
  BitMapItem.Assign(FNewBitmapItem);
  UpdateView;
end;

procedure TUndoEditBitMap.Undo;
var
  BitMapItem: TCollectionItem;
begin
//  inherited;
  BitMapItem := frmGoPhast.PhastModel.Bitmaps.Items[FIndex];
  BitMapItem.Assign(FOldBitmapItem);
  UpdateView;
end;

{ TUndoImportBitMap }

constructor TUndoImportBitMap.Create;
begin
  FNewBitmapItem := TCompressedBitmapItem.Create(nil);
end;

function TUndoImportBitMap.Description: string;
begin
  result := 'import bitmap';
end;

procedure TUndoImportBitMap.DoCommand;
var
  NewBitmap: TCollectionItem;
begin
  NewBitmap := frmGoPhast.PhastModel.Bitmaps.Add;
  FIndex := NewBitmap.Index;
  NewBitmap.Assign(FNewBitmapItem);
  UpdateView;
end;

procedure TUndoImportBitMap.Undo;
begin
  frmGoPhast.PhastModel.Bitmaps.Delete(FIndex);
  UpdateView;
end;

procedure TUndoImportBitMap.UpdateView;
begin
  case FNewBitmapItem.ViewDirection of
    vdTop:
      begin
        frmGoPhast.TopDiscretizationChanged := True;
        frmGoPhast.frameTopView.ZoomBox.InvalidateImage32
      end;
    vdFront:
      begin
        frmGoPhast.FrontDiscretizationChanged := True;
        frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32
      end;
    vdSide:
      begin
        frmGoPhast.SideDiscretizationChanged := True;
        frmGoPhast.frameSideView.ZoomBox.InvalidateImage32
      end;
    else
      Assert(False);
  end;
end;

end.

