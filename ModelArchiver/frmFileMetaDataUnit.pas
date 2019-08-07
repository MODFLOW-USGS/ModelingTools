unit frmFileMetaDataUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.ComCtrls,
  System.Generics.Collections, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs;

type
  TFileData = class(TObject)
    FileName: string;
    // file size in MB
    FileSize: double;
    FileType: string;
    Description: string;
    URL: string;
  end;

  TfrmFileMetaData = class(TForm)
    tvFiles: TTreeView;
    mm1: TMainMenu;
    miFile: TMenuItem;
    miSelectArchiveDirectory: TMenuItem;
    pnl1: TPanel;
    memoDescription: TMemo;
    lblDescription: TLabel;
    edUrl: TLabeledEdit;
    edFiletype: TLabeledEdit;
    edReportID: TLabeledEdit;
    spl1: TSplitter;
    miSaveDescriptions: TMenuItem;
    sdDescriptions: TSaveDialog;
    dlgOpenTemplate: TOpenDialog;
    procedure miSelectArchiveDirectoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvFilesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edReportIDChange(Sender: TObject);
    procedure edFiletypeChange(Sender: TObject);
    procedure edUrlChange(Sender: TObject);
    procedure memoDescriptionChange(Sender: TObject);
    procedure miSaveDescriptionsClick(Sender: TObject);
  private
    FFileDataList: TObjectList<TFileData>;
    FData: TFileData;
    procedure SetFileData(const Value: TFileData);
    property FileData: TFileData read FData write SetFileData;
    procedure UpdateXML(MetaDataFileName: TFileName);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFileMetaData: TfrmFileMetaData;

implementation

uses
  System.IOUtils, System.Types, Vcl.FileCtrl, Xml.VerySimple;

resourcestring
  StrHttpwaterusgsgo = 'http://water.usgs.gov/GIS/dsdl/gwmodels/%0:s/%1:s';

{$R *.dfm}

procedure TfrmFileMetaData.edFiletypeChange(Sender: TObject);
begin
  if FileData <> nil then
  begin
    FileData.FileType := edFiletype.Text;
  end;
end;

procedure TfrmFileMetaData.edReportIDChange(Sender: TObject);
var
  Report: TCaption;
  FileData: TFileData;
begin
  Report := edReportID.Text;
  if Report = '' then
  begin
    Report := '<report ID>';
  end;


  for FileData in FFileDataList do
  begin
    FileData.URL := Format(StrHttpwaterusgsgo, [Report, FileData.FileName]);
  end;
end;

procedure TfrmFileMetaData.edUrlChange(Sender: TObject);
begin
  if FileData <> nil then
  begin
    FileData.URL := edUrl.Text;
  end;
end;

procedure TfrmFileMetaData.FormCreate(Sender: TObject);
begin
  FFileDataList := TObjectList<TFileData>.Create;
end;

procedure TfrmFileMetaData.FormDestroy(Sender: TObject);
begin
  FFileDataList.Free;
end;

procedure TfrmFileMetaData.memoDescriptionChange(Sender: TObject);
begin
  if FileData <> nil then
  begin
    FileData.Description := memoDescription.Lines.Text;
  end;
end;

procedure TfrmFileMetaData.miSaveDescriptionsClick(Sender: TObject);
const
  Template =
  '			<digform>'#13#10
  +  '				<digtinfo>'#13#10
  +  '					<formname>%0:s</formname>'#13#10
  +  '					<formvern>1.0</formvern>'#13#10
  +  '					<formspec>%1:s</formspec>'#13#10
  +  '					<formcont>%2:s</formcont>'#13#10
  +  '					<transize>%3:g</transize>'#13#10
  +  '				</digtinfo>'#13#10
  +  '				<digtopt>'#13#10
  +  '					<onlinopt>'#13#10
  +  '						<computer>'#13#10
  +  '							<networka>'#13#10
  +  '								<networkr>%4:s</networkr>'#13#10
  +  '							</networka>'#13#10
  +  '						</computer>'#13#10
  +  '					</onlinopt>'#13#10
  +  '				</digtopt>'#13#10
  +  '			</digform>';

var
  Lines: TStringList;
  MetaDataFileName: TFileName;
begin
  if dlgOpenTemplate.Execute then
  begin
    MetaDataFileName := dlgOpenTemplate.FileName;
    UpdateXML(MetaDataFileName);
  end;
end;

procedure TfrmFileMetaData.miSelectArchiveDirectoryClick(Sender: TObject);
var
  chosenDirectory: string;
  Files: TStringDynArray;
  FileIndex: Integer;
  FileName: string;
  FileData: TFileData;
  Reader: TFileStream;
  Extension: string;
  Report: string;
  Nodes: TList<TTreeNode>;
  Splitter: TStringList;
  DirIndex: Integer;
  SplitIndex: Integer;
  InnerIndex: Integer;
  TopNode: TTreeNode;
begin
  if selectdirectory('Select a directory', '', chosenDirectory) then
  begin
    FFileDataList.Clear;

    Report := edReportID.Text;
    if Report = '' then
    begin
      Report := '<report ID>';
    end;

    Nodes := TList<TTreeNode>.Create;
    Splitter := TStringList.Create;
    tvFiles.Items.BeginUpdate;
    try
      Splitter.Delimiter := PathDelim;

      tvFiles.Items.Clear;

      chosenDirectory := IncludeTrailingPathDelimiter(chosenDirectory);

      Files := TDirectory.GetFiles(chosenDirectory, '*.*', TSearchOption.soAllDirectories);
      for FileIndex := 0 to Length(Files) - 1 do
      begin
        FileName := ExtractRelativePath(chosenDirectory, Files[FileIndex]);

        FileData := TFileData.Create;
        FFileDataList.Add(FileData);

        FileData.FileName := FileName;

        Reader := TFile.OpenRead(Files[FileIndex]);
        try
          FileData.FileSize := Reader.Size/1024/1024;
        finally
          Reader.Free;
        end;

        Extension := LowerCase(ExtractFileExt(Files[FileIndex]));
        if Extension = '.zip' then
        begin
          FileData.FileType := 'ZIP file';
        end
        else if Extension = '.txt' then
        begin
          FileData.FileType := 'ASCII text file';
        end
        else
        begin
          FileData.FileType := 'Unknown file type';
        end;

        FileData.Description := '';

        FileData.URL := Format(StrHttpwaterusgsgo, [Report, FileName]);

        Splitter.DelimitedText := FileName;

        TopNode := nil;
        SplitIndex := -1;
        for DirIndex := 0 to Splitter.Count - 2 do
        begin
          if Nodes.Count > DirIndex then
          begin
            TopNode := Nodes[DirIndex];
            if TopNode.Text = Splitter[DirIndex] then
            begin
              SplitIndex := DirIndex;
            end
            else
            begin
              for InnerIndex := Nodes.Count - 1 downto DirIndex do
              begin
                Nodes.Delete(InnerIndex);
              end;
              Break;
            end;
          end;
        end;

        if Nodes.Count > 0 then
        begin
          TopNode := Nodes.Last;
        end
        else
        begin
          TopNode := nil;
        end;
        for DirIndex := SplitIndex+1 to Splitter.Count - 2 do
        begin
          TopNode := tvFiles.Items.AddChild(TopNode, Splitter[DirIndex]);
          Nodes.Add(TopNode);
        end;

        tvFiles.Items.AddChildObject(TopNode, FileName, FileData)
      end;
    finally
      tvFiles.Items.EndUpdate;
      Nodes.Free;
      Splitter.Free;
    end;

    if tvFiles.Items.Count > 0 then
    begin
      tvFiles.Selected := tvFiles.Items[0];
      Self.FileData := tvFiles.Selected.Data;
    end;
  end;
end;

procedure TfrmFileMetaData.SetFileData(const Value: TFileData);
begin
  FData := Value;
  memoDescription.Lines.Clear;
  if FData = nil then
  begin
    edFiletype.Text := '';
    edUrl.Text := '';
  end
  else
  begin
    edFiletype.Text := FData.FileType;
    edUrl.Text := FData.URL;
    memoDescription.Lines.Add(FData.Description)
  end;
end;

procedure TfrmFileMetaData.UpdateXML(MetaDataFileName: TFileName);
var
  Xml: TXmlVerySimple;
  distinfo: TXmlNode;
  stdorder: TXmlNode;
  FileIndex: Integer;
  AFileData: TFileData;
  digform: TXmlNode;
  digtinfo: TXmlNode;
  formname: TXmlNode;
  formvern: TXmlNode;
  formspec: TXmlNode;
  formcont: TXmlNode;
  transize: TXmlNode;
  digtopt: TXmlNode;
  onlinopt: TXmlNode;
  computer: TXmlNode;
  networka: TXmlNode;
  networkr: TXmlNode;
begin
  Xml := TXmlVerySimple.Create;
  try
    Xml.LoadFromFile(MetaDataFileName);
    distinfo := Xml.DocumentElement.Find('distinfo');
    if distinfo = nil then
    begin
      distinfo := Xml.DocumentElement.AddChild('distinfo');
    end;
    stdorder := distinfo.Find('stdorder');
    if stdorder <> nil then
    begin
      stdorder.Clear;
    end
    else
    begin
      stdorder := distinfo.AddChild('stdorder');
    end;
    //      Lines := TStringList.Create();
    try
      for FileIndex := 0 to FFileDataList.Count - 1 do
      begin
        AFileData := FFileDataList[FileIndex];
        digform := stdorder.AddChild('digform');
        digtinfo := digform.AddChild('digtinfo');
        formname := digtinfo.AddChild('formname');
        formname.Text := AFileData.FileName;
        formvern := digtinfo.AddChild('formvern');
        formvern.Text := '1.0';
        formspec := digtinfo.AddChild('formspec');
        formspec.Text := AFileData.FileType;
        formcont := digtinfo.AddChild('formcont');
        formcont.Text := Trim(AFileData.Description);
        transize := digtinfo.AddChild('transize');
        transize.Text := Format('%g', [AFileData.FileSize]);
        digtopt := digform.AddChild('digtopt');
        onlinopt := digtopt.AddChild('onlinopt');
        computer := onlinopt.AddChild('computer');
        networka := computer.AddChild('networka');
        networkr := networka.AddChild('networkr');
        networkr.Text := AFileData.URL;
      end;
      //          Lines.Add(Format(Template, [AFileData.FileName, AFileData.FileType,
      //            Trim(AFileData.Description), AFileData.FileSize, AFileData.URL]));
    finally
    end;
    Xml.SaveToFile(MetaDataFileName);
  finally
    Xml.Free;
  end;
end;

procedure TfrmFileMetaData.tvFilesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if tvFiles.Selected <> nil then
  begin
    FileData := tvFiles.Selected.Data;
  end
  else
  begin
    FileData := nil;
  end;
end;

end.
