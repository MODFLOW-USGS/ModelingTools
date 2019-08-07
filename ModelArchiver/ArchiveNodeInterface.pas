unit ArchiveNodeInterface;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  EArchiveError = class(Exception);

  TNodeType = (ntCategory, ntCategoryUncompressed, ntCategoryCompressed,
    ntModel, ntFolder, ntFile, ntArchiveRoot, ntSkip);

  IArchiveNodeInterface = interface
    ['{031FA773-EBD2-4545-98DE-25FFB2F84A4D}']
    function GetParentNode: IArchiveNodeInterface;
    function GetNodeText: string;
    function GetModelDirectory: string;
    function GetNodeType: TNodeType;
    function GetCount: Integer;
    function GetChild(Index: Integer): IArchiveNodeInterface;
    function GetDescription: string;
    property ParentNode: IArchiveNodeInterface read GetParentNode;
    property NodeText: string read GetNodeText;
    property ModelDirectory: string read GetModelDirectory;
    property NodeType: TNodeType read GetNodeType;
    property Count: Integer read GetCount;
    property Children[Index: Integer]: IArchiveNodeInterface read GetChild;
    property Description: string read GetDescription;
  end;

  TArchiveNodeList = TList<IArchiveNodeInterface>;

resourcestring
  StrAncillary = 'ancillary';
  StrBinary = 'bin';
  StrModelInputFiles = 'model';
  StrModelOutputFiles = 'output';
  StrGeoref = 'georef';

const
  ArchiveExt = '.archive';

implementation

end.
