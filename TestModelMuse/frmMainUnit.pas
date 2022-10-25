unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, Spin, ComCtrls, Buttons, ImgList,
  Mask, JvExMask, JvToolEdit, IniFiles, JvComponentBase, JvCreateProcess,
  System.ImageList;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    odSelectGoPhast: TOpenDialog;
    odSelectFiles: TOpenDialog;
    edGoPhast: TEdit;
    btnBrowse: TButton;
    odSettings: TOpenDialog;
    sdSettings: TSaveDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SelectGoPhast1: TMenuItem;
    RunTest1: TMenuItem;
    Exit1: TMenuItem;
    odSelectOutputFiles: TOpenDialog;
    seTimeDelay: TSpinEdit;
    lblTimeDelay: TLabel;
    StatusBar1: TStatusBar;
    tvModflow: TTreeView;
    SelectModelMuseMODFLOWFiles1: TMenuItem;
    btnDelete: TButton;
    pbFiles: TProgressBar;
    BitBtn1: TBitBtn;
    ImageList1: TImageList;
    memoErrors: TMemo;
    Splitter1: TSplitter;
    SpeedButton1: TSpeedButton;
    pmFiles: TPopupMenu;
    miRefresh: TMenuItem;
    lblModelMuseLocation: TLabel;
    fedlBeyondCompareLocation: TJvFilenameEdit;
    lblBeyondCompareLocation: TLabel;
    jvcrtprcs1: TJvCreateProcess;
    miRefreshAll: TMenuItem;
    jvcrtprcsRunModelMuse: TJvCreateProcess;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnRunTestsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SelectModelMuseMODFLOWFiles1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure tvModflowCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure BitBtn1Click(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure tvModflowDblClick(Sender: TObject);
    procedure miRefreshAllClick(Sender: TObject);
  private
    Modified: boolean;
    FAbort: Boolean;
    AllFilesTheSame: boolean;
    MainIniFile: TMemIniFile;
    function TestFiles(out ErrorMessage: string): boolean;
    function CompareModflowFiles(const OutputFileName, ArchiveFileName: string;
      out ErrorMessage: string): boolean;
    function ComparePhastFiles(
      const OutputFileName, ArchiveFileName: string; var ErrorMessage: string): Boolean;
    procedure AddModelOutputFiles(Node: TTreeNode; InputFiles: TStringList);
    function EncloseQuotes(const AString: string): string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses IniFileUtilities;

resourcestring
  StrTransdat = '.trans.dat';
  StrFileLocations = 'File Locations';
  StrBeyondCompare = 'Beyond Compare';

{$R *.dfm}

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
begin
  if StartDir[length(StartDir)] <> '\' then
    StartDir := StartDir + '\';

  { Build a list of the files in directory StartDir
     (not the directories!)                         }

  IsFound :=
    FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
  while IsFound do begin
    FilesList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := FindFirst(StartDir+'*.*', faAnyFile, SR) = 0;
  while IsFound do begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
    FindFiles(FilesList, DirList[i], FileMask);

  DirList.Free;
end;


function FileLastModified
   (const TheFile: string): TDateTime;
var
  FileH : THandle;
  LocalFT : TFileTime;
  DosFT : DWORD;
  FindData : TWin32FindData;
begin
  Result := 0;
  FileH := FindFirstFile(PChar(TheFile), FindData) ;
  if FileH <> INVALID_HANDLE_VALUE then begin
   Windows.FindClose(FileH) ;
   if (FindData.dwFileAttributes AND
       FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
     FileTimeToLocalFileTime
      (FindData.ftLastWriteTime,LocalFT) ;
     FileTimeToDosDateTime
      (LocalFT,LongRec(DosFT).Hi,LongRec(DosFT).Lo) ;
     result := FileDateToDateTime(DosFT) ;
    end;
  end;
end;


procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  FAbort := true;
end;

procedure TfrmMain.btnBrowseClick(Sender: TObject);
begin
  if odSelectGoPhast.Execute then
  begin
    edGoPhast.Text := odSelectGoPhast.FileName;
    Modified := True;
  end;

end;

function TfrmMain.CompareModflowFiles(const OutputFileName, ArchiveFileName: string;
  out ErrorMessage: string): boolean;
const
  TenSeconds = 1 / 24/ 360;
  OneSecond = 1/24/3600;
var
  Index: integer;
  ArchiveFile, OutputFile: TStringList;
  ArchiveLine: string;
  OutputLine: string;
  StartingTime: Extended;
  JtfFile: Boolean;
  AxmlFile: Boolean;
  PestTemplate: Boolean;
  ReadArraysLine: Boolean;
begin
  result := True;
  ErrorMessage:= '';
  ArchiveFile := TStringList.Create;
  OutputFile := TStringList.Create;
  try
    ArchiveFile.LoadFromFile(ArchiveFileName);
    if FileExists(OutputFileName) then
    begin
      StartingTime := Now;
      While True do
      begin
        try
          OutputFile.LoadFromFile(OutputFileName);
          break;
        except on E: EFOpenError do
          begin
            if (Now - StartingTime) > (seTimeDelay.Value * OneSecond) then
            begin
              result := False;
              ErrorMessage := 'error opening ' + OutputFileName + '.';
              Exit;
            end
            else
            begin
              Sleep(1000);
            end;
          end;
        end;
      end;
    end
    else
    begin
      result := False;
      ErrorMessage := OutputFileName + ' does not exist.';
      Exit;
    end;
    if ArchiveFile.Count <> OutputFile.Count  then
    begin
      result := False;
      ErrorMessage := OutputFileName + ' and ' + ArchiveFileName
        + ' are not the same length.';
      Exit;
    end;
    JtfFile := ExtractFileExt(ArchiveFileName) = '.jtf';
    AxmlFile := ExtractFileExt(ArchiveFileName) = '.axml';
    PestTemplate := ExtractFileExt(ArchiveFileName) = '.tpl';
    ReadArraysLine := False;
    for Index := 0 to ArchiveFile.Count - 1 do
    begin
      ArchiveLine := ArchiveFile[Index];
      OutputLine := OutputFile[Index];
      if PestTemplate then
      begin
        if (Index = 2)
          and (Length(ArchiveLine) <> 0)
          and (ArchiveLine[1] = '#')
          and (Length(OutputLine) <> 0)
          and (OutputLine[1] = '#') then
        begin
          Continue;
        end
        else if (Index = 2) and (Pos('%ReadArrays', OutputLine) > 0) then
        begin
          ReadArraysLine := True;
        end;
        if (Index = 3) and ReadArraysLine
          and (Length(ArchiveLine) <> 0)
          and (ArchiveLine[1] = '#')
          and (Length(OutputLine) <> 0)
          and (OutputLine[1] = '#') then
        begin
          Continue;
        end
      end
      else if JtfFile then
      begin
        if (Index = 1)
          and (Length(ArchiveLine) <> 0)
          and (ArchiveLine[1] = '#')
          and (Length(OutputLine) <> 0)
          and (OutputLine[1] = '#') then
        begin
          Continue;
        end;
      end
      else
      begin
        if //(Index = 0)
          {and} (Length(ArchiveLine) <> 0)
          and (ArchiveLine[1] = '#')
          and (Length(OutputLine) <> 0)
          and (OutputLine[1] = '#') then
        begin
          Continue;
        end;
      end;
      result := ArchiveLine = OutputLine;
      if (not Result) and AxmlFile then
      begin
        if (Pos('C:\', ArchiveLine) > 0) or (Pos('C:\', OutputLine) > 0)  then
        begin
          result := True;
        end;

      end;

      if not result then
      begin
        ErrorMessage := 'Line ' + IntToStr(Index+1) + ' of ' +
          OutputFileName + ' and ' + ArchiveFileName
        + ' are not the same.';
        Exit;
      end;
    end;
  finally
    OutputFile.Free;
    ArchiveFile.Free;
  end;
end;

function TfrmMain.ComparePhastFiles(
  const OutputFileName, ArchiveFileName: string; var ErrorMessage: string): Boolean;
var
  TestFile: TStringList;
  LineIndex: Integer;
  OutPutFile: TStringList;
begin
  result := True;
  ErrorMessage := '';
  OutPutFile := TStringList.Create;
  TestFile := TStringList.Create;
  try
    OutPutFile.LoadFromFile(OutputFileName);
    TestFile.LoadFromFile(ArchiveFileName);
    if OutPutFile.Count <> TestFile.Count then
    begin
      result := False;
      ErrorMessage := 'Failed with "' + OutputFileName + '" because the number of lines doesn''t match.';
    end
    else
    begin
      for LineIndex := 0 to OutPutFile.Count - 1 do
      begin
        if OutPutFile[LineIndex] <> TestFile[LineIndex] then
        begin
          result := False;
          ErrorMessage := 'Failed with "' + OutputFileName + '" because line number ' + IntToStr(LineIndex + 1) + ' doesn''t match.';
        end;
      end;
    end;
  finally
    OutPutFile.Free;
    TestFile.Free;
  end;
end;

procedure TfrmMain.AddModelOutputFiles(Node: TTreeNode; InputFiles: TStringList);
var
  InputIndex: Integer;
  Directory: string;
  FileName: string;
  ModelDirectory: string;
begin
  FileName := Node.Text;
  ModelDirectory := ExtractFileDir(FileName);
  Directory := ModelDirectory + '\Archive\';
  FileName := ChangeFileExt(FileName, '');
  FileName := ExtractFileName(FileName);

//  InputFiles.Clear;
//  FindFiles(InputFiles, Directory, FileName + '.*');
//  for InputIndex := 0 to InputFiles.Count - 1 do
//  begin
//    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
//  end;
//
//  if Pos(' ', FileName) > 0 then
//  begin
    FileName := StringReplace(FileName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);

    InputFiles.Clear;
    FindFiles(InputFiles, Directory, FileName + '.*');
    for InputIndex := 0 to InputFiles.Count - 1 do
    begin
      tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
    end;
//  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, FileName + '_Child*.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, 'usgs.' +FileName + '.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, 'usgs.' +FileName + '_Modpath.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, 'usgs.' +FileName + '_ZoneBudget.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, 'usgs.' +FileName + '_MT3DMS.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  InputFiles.Clear;
  FindFiles(InputFiles, Directory, 'usgs.' +FileName + '_MT3D-USGS.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

  Directory := ModelDirectory + '\arrays\Archive\';
  InputFiles.Clear;
  FindFiles(InputFiles, Directory, FileName + '.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;
  InputFiles.Clear;
  FindFiles(InputFiles, Directory, FileName + '_Child*.*');
  for InputIndex := 0 to InputFiles.Count - 1 do
  begin
    tvModflow.Items.AddChild(Node, InputFiles[InputIndex]);
  end;

end;

function TfrmMain.TestFiles(out ErrorMessage: string): boolean;
var
  Time: TDateTime;
  TestDate: TDateTime;
  Index: Integer;
  OutputFileName: string;
  FileDate: TDateTime;
  TimeIndex: Integer;
  Success: Boolean;
  ParentNode: TTreeNode;
  ChildNode: TTreeNode;
  ModelFileName: string;
  GoPhastExeName: string;
  OutPutFiles: TStringList;
  ModelDirectory: string;
  ArchiveFileName: string;
  BatName: string;
  LocalErrorMessage: string;
  InArrays: Boolean;
  CommandLine: string;
begin
  AllFilesTheSame := True;
  ErrorMessage := '';
  result := True;
  GoPhastExeName := edGoPhast.Text;
  OutPutFiles := TStringList.Create;
  try
    pbFiles.Max := tvModflow.Items.Count;
    pbFiles.Position := 0;
    ChildNode := nil;
    ParentNode := tvModflow.Items.GetFirstNode;
    while ParentNode <> nil do
    begin
      if FAbort then Exit;
      ParentNode.Selected := True;
      ModelFileName := ParentNode.Text;
      ModelDirectory := ExtractFileDir(ModelFileName);

      // make a list of the output files to be tested and
      // delete any that already exist.
      OutPutFiles.Clear;
      ChildNode := ParentNode.getFirstChild;
      while ChildNode <> nil do
      begin
        OutputFileName := ChildNode.Text;
        InArrays := Pos('\arrays\',OutputFileName) > 0;
        OutputFileName := ExtractFileName(OutputFileName);
        if InArrays then
        begin
          OutputFileName := ModelDirectory + '\arrays\' + OutputFileName;
        end
        else
        begin
          OutputFileName := ModelDirectory + '\' + OutputFileName;
        end;
        OutPutFiles.Add(OutputFileName);

        if FileExists(OutputFileName) then
        begin
          DeleteFile(OutputFileName);
        end;

        ChildNode := ChildNode.getNextSibling;
      end;
      ParentNode := ParentNode.getNextSibling;
    end;

    ChildNode := nil;
    ParentNode := tvModflow.Items.GetFirstNode;
    while ParentNode <> nil do
    begin
      ModelFileName := ParentNode.Text;
      ModelDirectory := ExtractFileDir(ModelFileName);
      StatusBar1.SimpleText := 'testing ' + ModelFileName;

      ChildNode := ParentNode.getFirstChild;
      jvcrtprcsRunModelMuse.CommandLine := EncloseQuotes(GoPhastExeName)
        + ' ' + EncloseQuotes(ModelFileName)
        + ' -E -C';
      jvcrtprcsRunModelMuse.Run;

      pbFiles.StepIt;

      OutPutFiles.Clear;
      ChildNode := ParentNode.getFirstChild;
      while ChildNode <> nil do
      begin
        OutputFileName := ChildNode.Text;
        InArrays := Pos('\arrays\',OutputFileName) > 0;
        OutputFileName := ExtractFileName(OutputFileName);
        if InArrays then
        begin
          OutputFileName := ModelDirectory + '\arrays\' + OutputFileName;
        end
        else
        begin
          OutputFileName := ModelDirectory + '\' + OutputFileName;
        end;
        OutPutFiles.Add(OutputFileName);

        ChildNode := ChildNode.getNextSibling;
      end;


      Index := 0;
      ChildNode := ParentNode.getFirstChild;
      while ChildNode <> nil do
      begin
        if FAbort then Exit;
        ChildNode.Selected := True;
        ArchiveFileName := ChildNode.Text;


        OutputFileName := OutPutFiles[Index];
        StatusBar1.SimpleText := 'testing ' + OutputFileName;

        Application.ProcessMessages;
        FileDate := 0;
        // Wait for a few seconds
        for TimeIndex := 0 to seTimeDelay.Value - 1 do
        begin
          Time := Now;
          while Now - Time < 1 / 24 / 3600 do
          begin
            if FAbort then Exit;
            Application.ProcessMessages;
            Sleep(100);
          end;
          if FileExists(OutputFileName) then
          begin
            TestDate := FileLastModified(OutputFileName);
            if TestDate <> FileDate then
            begin
              FileDate := TestDate;
            end
            else
            begin
              break;
            end;
          end;
        end;

        if Pos(StrTransdat, OutputFileName) =
          Length(OutputFileName) - Length(StrTransdat) +1 then
        begin
          if ComparePhastFiles(OutputFileName, ArchiveFileName,
            LocalErrorMessage) then
          begin
            Assert(FileExists(OutputFileName));
            DeleteFile(OutputFileName);
            ChildNode.StateIndex := 1;
            if ParentNode.StateIndex < ChildNode.StateIndex then
            begin
              ParentNode.StateIndex := ChildNode.StateIndex
            end;
          end
          else
          begin
            AllFilesTheSame := false;
            ChildNode.StateIndex := 2;
            if ParentNode.StateIndex < ChildNode.StateIndex then
            begin
              ParentNode.StateIndex := ChildNode.StateIndex
            end;
            Assert(LocalErrorMessage <> '');
            memoErrors.Lines.Add(OutputFileName);
            memoErrors.Lines.Add(LocalErrorMessage);
            memoErrors.Lines.Add('');
//            Exit;
          end;
        end
        else
        begin
          if CompareModflowFiles(OutputFileName, ArchiveFileName, LocalErrorMessage) then
          begin
            Assert(FileExists(OutputFileName));
            DeleteFile(OutputFileName);
            ChildNode.StateIndex := 1;
            if ParentNode.StateIndex < ChildNode.StateIndex then
            begin
              ParentNode.StateIndex := ChildNode.StateIndex
            end;
          end
          else
          begin
            AllFilesTheSame := false;
            Assert(LocalErrorMessage <> '');
            memoErrors.Lines.Add(OutputFileName);
            memoErrors.Lines.Add(LocalErrorMessage);
            memoErrors.Lines.Add('');
            ChildNode.StateIndex := 2;
            if ParentNode.StateIndex < ChildNode.StateIndex then
            begin
              ParentNode.StateIndex := ChildNode.StateIndex
            end;
          end;
        end;
        pbFiles.StepIt;

        Inc(Index);
        ChildNode := ChildNode.getNextSibling;
      end;
      ParentNode.Collapse(False);
      BatName := ModelDirectory + '\RunModflow.Bat';
      if FileExists(BatName) then
      begin
        DeleteFile(BatName);
      end;
      ParentNode := ParentNode.getNextSibling;
    end;
  finally
    OutPutFiles.Free;
  end;
end;

procedure TfrmMain.tvModflowCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Selected then
  begin
    Sender.Canvas.Brush.Color := clBlue;
    Sender.Canvas.Font.Color := clWhite;
  end;
end;

function TfrmMain.EncloseQuotes(const AString: string): string;
begin
  if Pos(' ', AString) > 0 then
  begin
    result := '"' + AString + '"';
  end
  else
  begin
    result := AString;
  end;
end;


procedure TfrmMain.tvModflowDblClick(Sender: TObject);
var
  ParentNode: TTreeNode;
  ModelFileName: string;
  ModelDirectory: string;
  ChildNode: TTreeNode;
  OutputFileName: string;
  InArrays: Boolean;
begin
  if tvModflow.Selected.HasChildren  then
  begin
    Beep;
    MessageDlg('You need to double-click on an output file',
      mtError, [mbOK], 0);
    Exit;
  end;
  if not FileExists(fedlBeyondCompareLocation.FileName) then
  begin
    Beep;
    MessageDlg('You need to specify the location of Beyond Compare',
      mtError, [mbOK], 0);
    Exit;
  end;
  ChildNode := tvModflow.Selected;
  ParentNode := ChildNode.Parent;
  ModelFileName := ParentNode.Text;
  ModelDirectory := ExtractFileDir(ModelFileName);

  OutputFileName := ChildNode.Text;
  if not FileExists(OutputFileName) then
  begin
    Beep;
    MessageDlg(OutputFileName + 'does not exist',
      mtError, [mbOK], 0);
    Exit;
  end;

  InArrays := Pos('\arrays\',OutputFileName) > 0;
  OutputFileName := ExtractFileName(OutputFileName);
  if InArrays then
  begin
    OutputFileName := ModelDirectory + '\arrays\' + OutputFileName;
  end
  else
  begin
    OutputFileName := ModelDirectory + '\' + OutputFileName;
  end;

  if not FileExists(OutputFileName) then
  begin
    Beep;
    MessageDlg(OutputFileName + 'does not exist',
      mtError, [mbOK], 0);
    Exit;
  end;

  jvcrtprcs1.CommandLine := EncloseQuotes(fedlBeyondCompareLocation.FileName)
    + ' ' + EncloseQuotes(OutputFileName) + ' ' + EncloseQuotes(ChildNode.Text);
  jvcrtprcs1.Run;
end;

procedure TfrmMain.btnRunTestsClick(Sender: TObject);
var
  Time: TDateTime;
  ErrorMessage: string;
  TimeIndex: integer;
  IFileName: string;
  IniFile: TStringList;
  FileDate, TestDate: TDateTime;
  IniFileExists: boolean;
  Node: TTreeNode;
begin
  FAbort := False;
  if not FileExists(edGoPhast.Text) then
  begin
    Beep;
    MessageDlg('ModelMuse location has not been specified.', mtError, [mbOK], 0);
    Exit;
  end;

  IFileName := IniFileName(Handle, edGoPhast.Text);
  IniFileExists := FileExists(IFileName);
  if IniFileExists then
  begin
    IniFile := TStringList.Create;
  end
  else
  begin
    IniFile := nil;
  end;
  try
    if IniFileExists then
    begin
      IniFile.LoadFromFile(IFileName);
    end;
    try
      memoErrors.Lines.Clear;
      Node := tvModflow.Items.GetFirstNode;
      while Node <> nil do
      begin
        Node.StateIndex := 0;
        Node := Node.GetNext;
      end;
      if TestFiles(ErrorMessage) then
      begin
        Assert(ErrorMessage = '');
      end
      else
      begin
        Assert(ErrorMessage <> '');
        Beep;
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
        Exit;
      end;
    finally
      FileDate := 0;
      for TimeIndex := 0 to seTimeDelay.Value -1 do
      begin
        Time := Now;
        While Now - Time < 1/24/3600 do
        begin
          Application.ProcessMessages;
        end;
        if FileExists(IFileName) then
        begin
          TestDate := FileLastModified(IFileName);
          if TestDate <> FileDate then
          begin
            FileDate := TestDate;
          end
          else
          begin
            break;
          end;
        end;
      end;
      if IniFileExists then
      begin
        IniFile.SaveToFile(IFileName);
      end
      else
      begin
        if FileExists(IFileName) then
        begin
          DeleteFile(IFileName);
        end
      end;
    end;
  finally
    IniFile.Free;
  end;
  if FAbort then
  begin
    ShowMessage('Aborted');
  end
  else if AllFilesTheSame then
  begin
    ShowMessage('Success');
  end
  else
  begin
    ShowMessage('Failed');
  end;
  StatusBar1.SimpleText := '';
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
begin
  if tvModflow.Selected <> nil then
  begin
    tvModflow.Items.Delete(tvModflow.Selected);
    Modified := True;
  end;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close
end;

procedure TfrmMain.Open1Click(Sender: TObject);
var
  Lines: TStringList;
  Count: integer;
  Index: integer;
  StartIndex: integer;
  ParentNode: TTreeNode;
  ALine: string;
begin
  if odSettings.Execute then
  begin
    tvModflow.Items.Clear;

    sdSettings.FileName := odSettings.FileName;
    Lines := TStringList.Create;
    try

      Lines.LoadFromFile(odSettings.FileName);
      SetCurrentDir(ExtractFileDir(odSettings.FileName));
      edGoPhast.Text := ExpandFileName(Lines[0]);
      Lines.Delete(0);
      StartIndex := 0;
      if StartIndex < Lines.Count then
      begin
        Count := StrToInt(Lines[StartIndex]);
        Inc(StartIndex);
        ParentNode := nil;
        for Index := 0 to Count - 1 do
        begin
          ALine := Lines[StartIndex+Index];
          Assert(Length(ALine) > 0);
          if ALine[1] = #9 then
          begin
            tvModflow.Items.AddChild(ParentNode, ExpandFileName(Trim(ALine)));
          end
          else
          begin
            ParentNode := tvModflow.Items.Add(nil, ExpandFileName(ALine));
          end;
        end;
      end;
    finally
      Lines.Free;
    end;
    Modified := False;
  end;
end;

procedure TfrmMain.Save1Click(Sender: TObject);
var
  Lines: TStringList;
  Base: string;
  ParentNode, ChildNode: TTreeNode;
begin
  if sdSettings.Execute then
  begin
    Lines := TStringList.Create;
    try

      Base := ExtractFileDir(sdSettings.FileName);
      if Base[Length(Base)] <> PathDelim then
      begin
        Base := Base + PathDelim;
      end;

      Lines.Add(ExtractRelativePath(Base , edGoPhast.Text));

      Lines.Add(IntToStr(tvModflow.Items.Count));

      ParentNode := tvModflow.Items.GetFirstNode;
      while ParentNode <> nil do
      begin
        Lines.Add(ExtractRelativePath(Base , ParentNode.Text));

        ChildNode := ParentNode.getFirstChild;
        while ChildNode <> nil do
        begin
          Lines.Add(#9 + ExtractRelativePath(Base , ChildNode.Text));
          ChildNode := ChildNode.getNextSibling;
        end;

        ParentNode := ParentNode.getNextSibling;
      end;

      Lines.SaveToFile(sdSettings.FileName);
    finally
      Lines.Free;
    end;
    Modified := False;
    MainIniFile.WriteString(StrFileLocations, StrBeyondCompare,
      fedlBeyondCompareLocation.FileName);
    MainIniFile.UpdateFile;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Modified and (MessageDlg('Do you want to save your file?',
    mtInformation, [mbYes, mbNo], 0) = mrYes)  then
  begin
    Save1Click(nil);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Modified := False;
  MainIniFile:= TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  fedlBeyondCompareLocation.FileName :=
    MainIniFile.ReadString(StrFileLocations, StrBeyondCompare, '');
end;

procedure TfrmMain.miRefreshClick(Sender: TObject);
var
  ModelNode: TTreeNode;
  OutputFiles: TStringList;
begin
  ModelNode := tvModflow.Selected;
  if ModelNode = nil then
  begin
    Exit;
  end;
  if not ModelNode.HasChildren then
  begin
    ModelNode := ModelNode.Parent;
  end;
  ModelNode.DeleteChildren;
  OutputFiles := TStringList.Create;
  try
    AddModelOutputFiles(ModelNode, OutputFiles);
  finally
    OutputFiles.Free;
  end;
end;

procedure TfrmMain.SelectModelMuseMODFLOWFiles1Click(Sender: TObject);
var
  Index: integer;
  InputFiles: TStringList;
  Node: TTreeNode;
  FileName2: string;
begin
  if odSelectFiles.Execute then
  begin
    InputFiles := TStringList.Create;
    try
      if odSelectFiles.Files.Count > 0 then
      begin
        Modified := True;
      end;
      for Index := 0 to odSelectFiles.Files.Count -1 do
      begin
        FileName2 := odSelectFiles.Files[Index];
        Node := tvModflow.Items.Add(nil, FileName2);
        AddModelOutputFiles(Node, InputFiles);
      end;
    finally
      InputFiles.Free;
    end;
  end;
end;

procedure TfrmMain.miRefreshAllClick(Sender: TObject);
var
  index: Integer;
  OutputFiles: TStringList;
  ModelNode: TTreeNode;
begin
//
  OutputFiles := TStringList.Create;
  try
    ModelNode := tvModflow.Items.GetFirstNode;
    while ModelNode <> nil do
    begin
      ModelNode.DeleteChildren;
      AddModelOutputFiles(ModelNode, OutputFiles);
      ModelNode := ModelNode.getNextSibling;
    end;

    for index := 0 to tvModflow.Items.Count do
    begin

    end;
  finally
    OutputFiles.Free
  end;    

end;

end.
