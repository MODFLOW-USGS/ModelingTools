unit frmImportModflowUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, Mask, JvExMask, JvToolEdit,
  ArgusDataEntry, ComCtrls, JvPageList, JvExControls, Vcl.ExtCtrls, StrUtils,
  SubPolygonUnit, Modflow2005ImporterUnit, GrayTabs, GoPhastTypes;

type
  TfrmImportModflow = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pbProgress: TProgressBar;
    sbStatusBar: TStatusBar;
    pgcMain: TPageControl;
    tabModel: TTabSheet;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    rdeX: TRbwDataEntry;
    rdeY: TRbwDataEntry;
    rdeGridAngle: TRbwDataEntry;
    fedNameFile: TJvFilenameEdit;
    lblWarning: TLabel;
    lblGridOrigin: TLabel;
    cbOldStream: TCheckBox;
    comboGridOrigin: TComboBox;
    tabSubmodel: TTabSheet;
    rgSubmodelChoice: TRadioGroup;
    pglstBoundaySpecification: TJvPageList;
    jvspNone: TJvStandardPage;
    jvspPolygon: TJvStandardPage;
    jvspColRow: TJvStandardPage;
    grpBoundaryConditions: TGroupBox;
    fedHead: TJvFilenameEdit;
    lblHeads: TLabel;
    lblFlows: TLabel;
    fedFlow: TJvFilenameEdit;
    rdeFirstCol: TRbwDataEntry;
    lblFirstCol: TLabel;
    lblLastCol: TLabel;
    rdeLastCol: TRbwDataEntry;
    lblFirstRow: TLabel;
    rdeFirstRow: TRbwDataEntry;
    lblLastRow: TLabel;
    rdeLastRow: TRbwDataEntry;
    lblPolygon: TLabel;
    fedPolygon: TJvFilenameEdit;
    rgMethod: TRadioGroup;
    procedure fedNameFileChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rgSubmodelChoiceClick(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    FReadModflowInputProperly: Boolean;
    FConsoleLines: TStringList;
    procedure HandleModflowConsolLine(const Text: string);
    procedure UpdateStatusBar(const Text: string);
    procedure ShowProgress(Position, Total: integer);
    function SpecifySubModel(ImportParameters: TImportParameters): Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  EShapeOutlineError = class(Exception);

  TShapeOutline = class(TOutline)
  public
    Constructor Create(ShapeFile: String);
  end;

implementation

{$R *.dfm}

uses JclSysUtils, frmShowHideObjectsUnit,
  frmDisplayDataUnit, ModelMuseUtilities, frmConsoleLinesUnit,
  Generics.Collections, System.SysConst, ConvertSensUnit, ShapefileUnit,
  System.IOUtils, TempFiles;

resourcestring
  StrTheMODFLOWNameFil = 'The MODFLOW Name file appears to be invalid';
  StrSWasNotFound = '%s was not found.';
  StrNoLISTFileWasFou = 'No LIST file was found in the MODFLOW Name file.';
  StrThereWasAnErrorR = 'There was an error reading the MODFLOW input files.' +
  '  Check the console lines below and %s for error messages.';
  StrTheListingFile = 'The listing file, "%s", was not found.';
  StrTheNameOfTheMODF = 'The name of the MODFLOW Name file can not contain a' +
  'ny spaces.';
  StrReadingStressPerio = 'Reading Stress Period %s';
  StrAbortingModelMuse = 'Aborting.  ModelMuse was unable to create %0:s. Pl' +
  'ease correct this problem. The error message was %1:s.';
  StrADirectoryListedI = 'A directory listed in the name file "%s" does not ' +
  'exist and could not be created.';
  StrUnableToSaveTempo = 'Unable to save temporaray name file: %s';
  StrTheSolverPackages = 'The solver packages can not be imported in MODFLOW' +
  '-2000 models.';
  StrTheShapefileUsedT = 'The Shapefile used to specify the submodel outline' +
  ' must contain only one shape. The shape may not contain multiple parts.';

  StrYouHaveSpecifiedT = 'You have specified the names of both a head file a' +
  'nd a cell-by-cell flow file for the submodel. You only need to specify on' +
  'e or the other. Specified head boundaries override specified flow boundar' +
  'ies. Do you want to continue anyway?';
  StrTheShapefileOutlin = 'The Shapefile outline used to define the submodel' +
  ' can contain only one shape.';
  StrTheShapefileForTh = 'The Shapefile for the submodel outline must contai' +
  'n only polygons.';
  StrShapefileOnePart = 'The Shapefile for the submodel outline must have on' +
  'ly one part.';
  StrSDoesNotExist = 'The shapefile that you specified for the submodel boundary, "%s" does not exist';
  StrErrorReadingS = 'Error reading "%s". Check to make sure that it is the ' +
  'name file of a MODFLOW-2005 or MODFLOW-NWT model.';

{$IF CompilerVersion < 24}
// This is a workaround for a bug in SysUtils.DirectoryExists
// In Delphi XE2. If fixed in the version of Delphi used to compile
// this unit, this function may be removed.
function DirectoryExists(Directory: string): Boolean;
var
  Code: Cardinal;
  LastError: Cardinal;
begin
  Code := GetFileAttributes(PChar(Directory));

  if Code <> INVALID_FILE_ATTRIBUTES then
  begin
    result := SysUtils.DirectoryExists(Directory)
  end
  else
  begin
    LastError := GetLastError;
    Result := (LastError <> ERROR_FILE_NOT_FOUND) and
      (LastError <> ERROR_PATH_NOT_FOUND) and
      (LastError <> ERROR_INVALID_NAME) and
      (LastError <> ERROR_BAD_NETPATH) and
      (LastError <> ERROR_NOT_READY);
  end;
end;

// This is a workaround for a bug in Delphi XE2.
// If fixed in the version of Delphi used to compile
// this unit, this function may be removed.
function ForceDirectories(Dir: string): Boolean;
var
  E: EInOutError;
begin
  Result := True;
  if Dir = '' then
  begin
    E := EInOutError.CreateRes(@SCannotCreateDir);
    E.ErrorCode := 3;
    raise E;
  end;
  Dir := ExcludeTrailingPathDelimiter(Dir);
{$IFDEF MSWINDOWS}
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
{$ENDIF}
{$IFDEF POSIX}
  if (Dir = '') or DirectoryExists(Dir) then Exit;
{$ENDIF POSIX}
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;
{$IFEND}


procedure TfrmImportModflow.btnOKClick(Sender: TObject);
var
  ModflowImporterName: string;
  NameFile: TStringList;
  Index: Integer;
  ALine: string;
  LineContents: TStringList;
  ListFileName: string;
  CurrentDir: string;
  XOrigin: double;
  YOrigin: double;
  GridAngle: double;
  OldFile: string;
  LineIndex: Integer;
  Splitter: TStringList;
  Ftype: string;
  Nunit: Integer;
  BadUnitNumberLine: Integer;
  UnitNumbers: TGenericIntegerList;
  Fname: string;
  FullFileName: string;
  FileDir: string;
  UnitNumberIndex: Integer;
  NameFileName: string;
  Modflow2000Model: Boolean;
  ImportParameters: TImportParameters;
  UnitNumber: string;
  SenFileName: string;
  PValFileName: string;
//  DelimPos: Integer;
begin
  inherited;
  PValFileName := '';
  Enabled := False;
  CurrentDir := GetCurrentDir;
  try
    ModflowImporterName := ExtractFileDir(Application.ExeName)
      + '\' + 'MF2005_Importer.exe';
    if not FileExists(ModflowImporterName) then
    begin
      Beep;
      MessageDlg(Format(StrSWasNotFound, [ModflowImporterName]), mtError, [mbOK], 0);
      Exit;
    end;

    NameFileName := fedNameFile.FileName;
    
    if not FileExists(NameFileName) then
    begin
      Beep;
      MessageDlg(Format(StrSWasNotFound, [NameFileName]), mtError, [mbOK], 0);
      Exit;
    end;
    
    SetCurrentDir(ExtractFileDir(fedNameFile.FileName));


    BadUnitNumberLine := -1;
    NameFile := TStringList.Create;
    Splitter := TStringList.Create;
    UnitNumbers := TGenericIntegerList.Create;
    try
      Splitter.Delimiter := ' ';
      try
      NameFile.LoadFromFile(NameFileName);
      except on ERangeError do
        begin
          Beep;
          MessageDlg(Format(StrErrorReadingS, [NameFileName]), mtError, [mbOK], 0);
          Exit;
        end;
      end;
      Modflow2000Model := False;
      for LineIndex := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[LineIndex];
        if (Length(ALine) > 0) and (ALine[1] <> '#') then
        begin
          if Pos('"', ALine) >= 1 then
          begin
            Splitter.QuoteChar := '"';
          end
          else if Pos('''', ALine) >= 1 then
          begin
            Splitter.QuoteChar := '''';
          end;
          Splitter.DelimitedText := ALine;
          if Splitter.Count > 0 then
          begin
            Ftype := UpperCase(Splitter[0]);
            if (Ftype = 'GLOBAL')
               or (Ftype = 'BTN') or (Ftype = 'ADV') or (Ftype = 'DSP')
               or (Ftype = 'GCG') or (Ftype = 'VDF') or (Ftype = 'SSM')
               or (Ftype = 'RCT') or (Ftype = 'SOR') or (Ftype = 'SEN')
               or (Ftype = 'PES') or (Ftype = 'OBS') or (Ftype = 'LMG')
               or (Ftype = 'DAF') or (Ftype = 'DAFG') or (Ftype = 'VSC')
               or (Ftype = 'DTOB') or (Ftype = 'ADV2') or (Ftype = 'GWT')
               then
            begin
              Modflow2000Model := True;
            end;
          end;
        end;
      end;
      for LineIndex := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[LineIndex];
        if (Length(ALine) > 0) and (ALine[1] <> '#') then
        begin
          if Pos('"', ALine) >= 1 then
          begin
            Splitter.QuoteChar := '"';
          end
          else if Pos('''', ALine) >= 1 then
          begin
            Splitter.QuoteChar := '''';
          end;
          Splitter.DelimitedText := ALine;
          if Splitter.Count > 0 then
          begin
            Ftype := UpperCase(Splitter[0]);
            if Ftype = 'SEN' then
            begin
              UnitNumber := Splitter[1];
              SenFileName := Splitter[2];
              ConvertSens(SenFileName, PValFileName);
              ALine := Format('PVAL %0:s %1:s', [UnitNumber, PValFileName]);
              NameFile[LineIndex] := ALine;
            end;
            // comment out MODFLOW-2000 files or other unhandled file types.
            if (Ftype = 'OBS') or (Ftype = 'LMG')
               or (Ftype = 'PES') or (Ftype = 'GLOBAL')
               or (Ftype = 'SOR') or (Ftype = 'DAF') or (Ftype = 'DAFG')
               or (Ftype = 'DTOB') or (Ftype = 'ADV2')
               or (Ftype = 'BTN') or (Ftype = 'ADV') or (Ftype = 'DSP')
               or (Ftype = 'GCG') or (Ftype = 'VDF') or (Ftype = 'SSM')
               or (Ftype = 'RCT') or (Ftype = 'VSC')
               or (Ftype = 'CFP') or (Ftype = 'CRCH') or (Ftype = 'COC')
               or (Ftype = 'BFH') or (Ftype = 'BFH2') or (Ftype = 'RIP')
               or (Ftype = 'FMP')
               // CLB, NDC, and WHS are only in Visual MODFLOW.
               or (Ftype = 'CLB') or (Ftype = 'NDC') or (Ftype = 'WHS')
               or (Ftype = 'GWT') or (Ftype = 'ASP') then
            begin
              ALine := '#' + ALine;
              NameFile[LineIndex] := ALine;
            end
            else if Modflow2000Model and
              ((Ftype = 'DE4') or (Ftype = 'GMG') or (Ftype = 'LMG')
              or (Ftype = 'PCG') or (Ftype = 'PCGN') or (Ftype = 'SIP')
               or (Ftype = 'SOR')) then
            begin
              ALine := '#' + ALine;
              NameFile[LineIndex] := ALine;
              Beep;
              MessageDlg(StrTheSolverPackages, mtInformation, [mbOK], 0);
            end
            else if Splitter.Count > 2 then
            begin
              if (Ftype = 'DATAGLO(BINARY)') then
              begin
                Splitter[0] := 'DATA(BINARY)';
                NameFile[LineIndex] := Splitter.DelimitedText;
              end
              else if (Ftype = 'DATAGLO') then
              begin
                Splitter[0] := 'DATA';
                NameFile[LineIndex] := Splitter.DelimitedText;
              end;

              if TryStrToInt(Splitter[1], Nunit) then
              begin
                if Nunit = 6 then
                begin
                  BadUnitNumberLine := LineIndex;
                end
                else
                begin
                  UnitNumbers.Add(Nunit);
                end;
                Fname := Splitter[2];
                FullFileName := ExpandFileName(Fname);
                FileDir := ExtractFileDir(FullFileName);
                if not DirectoryExists(FileDir) then
                begin
                  if not ForceDirectories(FileDir) then
                  begin
                    Beep;
                    MessageDlg(Format(StrADirectoryListedI, [FileDir]),
                      mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      if BadUnitNumberLine >= 0 then
      begin
        Splitter.DelimitedText := NameFile[BadUnitNumberLine];
        for UnitNumberIndex := 7 to MAXINT do
        begin
          if UnitNumbers.IndexOf(UnitNumberIndex) < 0 then
          begin
            Splitter[1] := IntToStr(UnitNumberIndex);
            Break;
          end;
        end;
        NameFile[BadUnitNumberLine] := Splitter.DelimitedText;
      end;
      NameFileName := IncludeTrailingPathDelimiter(ExtractFileDir(fedNameFile.FileName))
        + 'TempNameFile.nam';
      try
        NameFile.WriteBOM := False;
        NameFile.SaveToFile(NameFileName);
      except  on EFCreateError do
        begin
          Beep;
          MessageDlg(Format(StrUnableToSaveTempo, [NameFileName]), mtError, [mbOK], 0);
        end;

      end;
    finally
      NameFile.Free;
      Splitter.Free;
      UnitNumbers.Free;
    end;

//    if Pos(' ', ExtractFileName(edNameFile.FileName)) > 0 then
//    begin
//      Beep;
//      MessageDlg(StrTheNameOfTheMODF, mtError, [mbOK], 0);
//      Exit;
//    end;

    XOrigin := StrToFloat(rdeX.Text);
    YOrigin := StrToFloat(rdeY.Text);
    GridAngle := StrToFloat(rdeGridAngle.Text) * Pi/180;

    try
      OldFile := ExtractFileDir(NameFileName) + '\old.txt';
      if cbOldStream.Checked then
      begin
        With TStringList.Create do
        begin
          WriteBOM := False;
          SaveToFile(OldFile);
          Free;
        end;
      end
      else
      begin
        DeleteFile(OldFile);
      end;
    except on E: EFCreateError do
      begin
        Beep;
        MessageDlg(Format(StrAbortingModelMuse, [OldFile, E.Message]), mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end;

    ListFileName := '';
    NameFile := TStringList.Create;
    LineContents := TStringList.Create;
    try
      LineContents.Delimiter := ' ';
      try
        NameFile.LoadFromFile(NameFileName);
      except on EFOpenError do
        begin
          CantOpenFileMessage(NameFileName);
          Exit;
        end;
      end;
      for Index := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[Index];
        if (Length(ALine) > 0) and (ALine[1] <> '#') then
        begin
          LineContents.DelimitedText := UpperCase(ALine);
          if LineContents.Count = 0 then
          begin
            Beep;
            MessageDlg(StrTheMODFLOWNameFil, mtError, [mbOK], 0);
            ModalResult := mrNone;
            Exit;
          end;
          if Trim(LineContents[0]) = 'LIST' then
          begin
            LineContents.DelimitedText := ALine;
            if LineContents.Count < 3 then
            begin
              Beep;
              MessageDlg(StrTheMODFLOWNameFil, mtError, [mbOK], 0);
              ModalResult := mrNone;
              Exit;
            end;
            ListFileName := LineContents[2];
            break;
          end;
        end;
      end;
    finally
      NameFile.Free;
      LineContents.Free;
    end;
    if ListFileName = '' then
    begin
      Beep;
      MessageDlg(StrNoLISTFileWasFou, mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    end;
    SetCurrentDir(ExtractFileDir(NameFileName));
//    if Copy(ListFileName,1,2) = '.\' then
//    begin
//      DelimPos := PosEx(PathDelim,ListFileName,3);
//      if DelimPos > 0 then
//      begin
//        ListFileName := Copy(ListFileName,DelimPos+1,MaxInt);
//      end;
//
//    end;
    ListFileName := ExpandFileName(ListFileName);

    ImportParameters := TImportParameters.Create;
    try
      if not SpecifySubModel(ImportParameters) then
      begin
        ModalResult := mrNone;
        Exit;
      end;
      FReadModflowInputProperly := False;
      Execute('"' + ModflowImporterName + '" '
        + ExtractFileName(NameFileName), HandleModflowConsolLine);
      if not FReadModflowInputProperly then
      begin
        Beep;
        frmConsoleLines := TfrmConsoleLines.Create(nil);
        try
          frmConsoleLines.lblMessage.Caption :=
            Format(StrThereWasAnErrorR, [ListFileName]);
          frmConsoleLines.memoConsoleLines.Lines := FConsoleLines;
          frmConsoleLines.ShowModal;
        finally
          frmConsoleLines.Free;
        end;
  //      MessageDlg(Format(StrThereWasAnErrorR, [ListFileName]),
  //        mtError, [mbOK], 0);
        Exit;
      end;
      if not FileExists(ListFileName) then
      begin
        Beep;
        MessageDlg(Format(StrTheListingFile, [ListFileName]),
          mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
      sbStatusBar.SimpleText := '';
      FreeAndNil(frmShowHideObjects);
      FreeAndNil(frmDisplayData);
      ImportParameters.ListFileName := ListFileName;
      ImportParameters.XOrigin := XOrigin;
      ImportParameters.YOrigin := YOrigin;
      ImportParameters.GridAngle := GridAngle;
      ImportParameters.textHandler := UpdateStatusBar;
      ImportParameters.ProgressHandler := ShowProgress;
      ImportParameters.ModelType := mtParent;
      ImportParameters.NameFile := NameFileName;
      ImportParameters.GridOrigin := TGridOrigin(comboGridOrigin.ItemIndex);
      ImportParameters.AssignmentMethod := TCellAssignmentMethod(rgMethod.ItemIndex);
      ImportModflow2005(ImportParameters);
      DeleteFile(NameFileName);
      if PValFileName <> '' then
      begin
        DeleteFile(PValFileName);
      end;
      if FileExists(OldFile) then
      begin
        DeleteFile(OldFile);
      end;
    finally
      ImportParameters.Free;
    end;
  finally
    SetCurrentDir(CurrentDir);
    Enabled := True;
  end;
end;
procedure TfrmImportModflow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmImportModflow.fedNameFileChange(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := FileExists(fedNameFile.FileName);
end;

procedure TfrmImportModflow.FormCreate(Sender: TObject);
begin
  inherited;
  FConsoleLines := TStringList.Create;
  pgcMain.ActivePageIndex := 0;
  pglstBoundaySpecification.ActivePageIndex := 0;
end;

procedure TfrmImportModflow.FormDestroy(Sender: TObject);
begin
  inherited;
  FConsoleLines.Free;
end;

procedure TfrmImportModflow.FormShow(Sender: TObject);
begin
  inherited;
  SetAppearance;
  lblWarning.Width := Width - 16;
  lblWarning.Font.Style := [fsBold];
end;

procedure TfrmImportModflow.HandleModflowConsolLine(const Text: string);
const
  Normal = 'Normal termination of simulation';
  SP = 'Solving:  Stress period:';
  TS = 'Time step:';
var
  SpPos: Integer;
  TsPos: Integer;
  StressPeriod: string;
  SPStart: integer;
begin
  FConsoleLines.Add(Text);
  SpPos := Pos(SP, Text);
  TsPos := Pos(TS, Text);
  if (SpPos > 0) and (TsPos > 0) then
  begin
    SPStart := SpPos + Length(SP);
    StressPeriod := Trim(Copy(Text, SPStart, TsPos-SPStart));
    sbStatusBar.SimpleText := Format(StrReadingStressPerio, [StressPeriod]);
  end
  else
  begin
    if Trim(Text) <> '' then
    begin
      sbStatusBar.SimpleText := Text;
    end;
  end;
  Application.ProcessMessages;

  if Trim(Text) = Normal then
  begin
    FReadModflowInputProperly := True;
  end;
end;

procedure TfrmImportModflow.rgSubmodelChoiceClick(Sender: TObject);
begin
  inherited;
  pglstBoundaySpecification.ActivePageIndex := rgSubmodelChoice.ItemIndex;
  fedHead.Enabled := rgSubmodelChoice.ItemIndex > 0;
  fedFlow.Enabled := rgSubmodelChoice.ItemIndex > 0;
  lblHeads.Enabled := rgSubmodelChoice.ItemIndex > 0;
  lblFlows.Enabled := rgSubmodelChoice.ItemIndex > 0;

end;

procedure TfrmImportModflow.ShowProgress(Position, Total: integer);
begin
  if pbProgress.Max <> Total then
  begin
    pbProgress.Max := Total
  end;
  pbProgress.Position := Position;
  Application.ProcessMessages;
end;

function TfrmImportModflow.SpecifySubModel(ImportParameters: TImportParameters): Boolean;
begin
  result := True;
  ImportParameters.Outline := nil;
  ImportParameters.HeadFile := '';
  ImportParameters.FlowFile := '';
  ImportParameters.FirstCol := 0;
  ImportParameters.LastCol := 0;
  ImportParameters.FirstRow := 0;
  ImportParameters.LastRow := 0;
  if rgSubmodelChoice.ItemIndex > 0 then
  begin
	  case rgSubmodelChoice.ItemIndex of
	  1:
	    begin
        try
          if TFile.Exists(fedPolygon.FileName) then
          begin
            ImportParameters.Outline := TShapeOutline.Create(fedPolygon.FileName);
          end
          else
          begin
            result := False;
            Beep;
            MessageDlg(Format(StrSDoesNotExist, [fedPolygon.FileName]), mtError, [mbOK], 0);
            Exit;
          end;
        except
          on E: EShapeOutlineError Do
          begin
            result := False;
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
		  end;
	  2:
	    begin
		    ImportParameters.FirstCol := rdeFirstCol.IntegerValue;
			  ImportParameters.LastCol := rdeLastCol.IntegerValue;
			  ImportParameters.FirstRow := rdeFirstRow.IntegerValue;
			  ImportParameters.LastRow := rdeLastRow.IntegerValue;
      end;
    else
      Assert(False);
		end;

    if (fedHead.FileName <> '') and (fedFlow.FileName <> '') then
    begin
      Beep;
      if not (MessageDlg(StrYouHaveSpecifiedT, mtWarning, [mbYes, mbNo], 0, mbNo) = mrYes) then
      begin
        result := False;
      end;
    end;

    if fedHead.FileName <> '' then
    begin
      ImportParameters.HeadFile := TempFileName;
      TFile.Delete(ImportParameters.HeadFile);
      TFile.Copy(fedHead.FileName, ImportParameters.HeadFile);
    end
    else
    begin
      ImportParameters.HeadFile := '';
    end;

    if fedHead.FilterIndex in [1,3] then
    begin
      ImportParameters.HeadFileType := hftText
    end
    else
    begin
      ImportParameters.HeadFileType := hftBinary
    end;

    if fedFlow.FileName <> '' then
    begin
      ImportParameters.FlowFile := TempFileName;
      TFile.Delete(ImportParameters.FlowFile);
      TFile.Copy(fedFlow.FileName, ImportParameters.FlowFile);
    end
    else
    begin
      ImportParameters.FlowFile := '';
    end;
  end;
end;

procedure TfrmImportModflow.UpdateStatusBar(const Text: string);
begin
  sbStatusBar.SimpleText := Text;
  Application.ProcessMessages;
end;

{ TShapeOutline }

constructor TShapeOutline.Create(ShapeFile: String);
var
  ShapeReader: TShapefileGeometryReader;
  ShapeIndexFile: string;
  PolyIndex: Integer;
  AShape: TShapeObject;
  PartIndex: Integer;
  Points: TRealPointArray;
  ArrayLength: Integer;
  ShapePoint: TShapePoint;
  PointIndex: Integer;
begin
  inherited Create;
  Assert(TFile.Exists(ShapeFile));
  ShapeIndexFile := ChangeFileExt(ShapeFile, '.shx');
  Assert(TFile.Exists(ShapeIndexFile));
  ShapeReader := TShapefileGeometryReader.Create;
  try
    ShapeReader.ReadFromFile(ShapeFile, ShapeIndexFile);
    if not(ShapeReader.FileHeader.ShapeType
      in [stPolygon, stPolygonZ, stPolygonM]) then
    begin
      raise EShapeOutlineError.Create(StrTheShapefileForTh);
    end;
    if ShapeReader.Count <> 1 then
    begin
      raise EShapeOutlineError.Create(StrTheShapefileOutlin);
    end;
    for PolyIndex := 0 to ShapeReader.Count -1 do
    begin
      AShape := ShapeReader[PolyIndex];
      if AShape.FNumParts <> 1 then
      begin
        raise EShapeOutlineError.Create(StrShapefileOnePart);
      end;
      for PartIndex := 0 to AShape.FNumParts -1 do
      begin
        if PartIndex < AShape.FNumParts - 1 then
        begin
          ArrayLength := AShape.FParts[PartIndex+1]	- AShape.FParts[PartIndex];
        end
        else
        begin
          ArrayLength := AShape.FNumPoints	- AShape.FParts[PartIndex]
        end;
        SetLength(Points, ArrayLength+1);
        for PointIndex := 0 to ArrayLength-1 do
        begin
          ShapePoint := AShape.FPoints[PointIndex + AShape.FParts[PartIndex]];
          Points[PointIndex].X := ShapePoint.X;
          Points[PointIndex].Y := ShapePoint.Y;
        end;
        Points[ArrayLength] := Points[0];
        FPolygons.Add(TSubPolygon.Create(Points, ArrayLength+1, 0, 0));
      end;
    end;
  finally
    ShapeReader.Free;
  end;
end;

end.
