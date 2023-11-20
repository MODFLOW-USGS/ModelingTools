//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Support-code to load STL Files into TGLFreeForm-Components in GLScene. 
  Note that you must manually add this unit to one of your project's uses
  to enable support for STL files at run-time. 
}

unit GLFileSTL;

interface

uses
  System.Classes,
  System.SysUtils,

  GLVectorTypes,
  GLVectorGeometry,
  GLVectorLists,
  GLVectorFileObjects,
  GLApplicationFileIO,
  GLUtils;

type
  TSTLHeader = packed record
    dummy: array [0 .. 79] of byte;
    nbFaces: Longint;
  end;

  TSTLFace = packed record
    normal: TAffineVector; // facet surface normal
    v1: TAffineVector; // vertex 1
    v2: TAffineVector; // vertex 2
    v3: TAffineVector; // vertex 3
    padding: array [0 .. 1] of byte;
  end;

type
  (*  The STL vector file (stereolithography format).
    It is a list of the triangular surfaces that describe a computer generated
    solid model. This is the standard input for most rapid prototyping machines.
    There are two flavors of STL, the "text" and the "binary", this class
    reads both, but exports only the "binary" version.
    Original Binary importer code by Paul M. Bearne, Text importer by Adem. *)
  TGLSTLVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cSOLID_LABEL = 'SOLID';
  cFACETNORMAL_LABEL = 'FACET NORMAL ';
  cOUTERLOOP_LABEL = 'OUTER LOOP';
  cVERTEX_LABEL = 'VERTEX';
  cENDLOOP_LABEL = 'ENDLOOP';
  cENDFACET_LABEL = 'ENDFACET';
  cENDSOLID_LABEL = 'ENDSOLID';
  cFULL_HEADER_LEN = 80;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

class function TGLSTLVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLSTLVectorFile.LoadFromStream(aStream: TStream);
var
  sl: TStringList;

  procedure DecodeSTLNormals(const aString: String; var aNormal: TAffineVector);
  begin
    sl.CommaText := aString;
    if sl.Count <> 5 then
      raise Exception.Create('Invalid Normal')
    else
    begin
      aNormal.X := GLUtils.StrToFloatDef(sl[2], 0);
      aNormal.Y := GLUtils.StrToFloatDef(sl[3], 0);
      aNormal.Z := GLUtils.StrToFloatDef(sl[4], 0);
    end;
  end;

  procedure DecodeSTLVertex(const aString: String; var aVertex: TAffineVector);
  begin
    sl.CommaText := aString;
    if (sl.Count <> 4) or (CompareText(sl[0], cVERTEX_LABEL) <> 0) then
      raise Exception.Create('Invalid Vertex')
    else
    begin
      aVertex.X := GLUtils.StrToFloatDef(sl[1], 0);
      aVertex.Y := GLUtils.StrToFloatDef(sl[2], 0);
      aVertex.Z := GLUtils.StrToFloatDef(sl[3], 0);
    end;
  end;

var
  isBinary: Boolean;
  headerBuf: array [0 .. cFULL_HEADER_LEN - 1] of AnsiChar;
  positionBackup: Integer;
  fileContent: TStringList;
  curLine: String;
  i: Integer;
  Mesh: TMeshObject;
  Header: TSTLHeader;
  DataFace: TSTLFace;
  calcNormal: TAffineVector;
begin
  positionBackup := aStream.Position;
  aStream.Read(headerBuf[0], cFULL_HEADER_LEN);
  aStream.Position := positionBackup;
  isBinary := True;
  i := 0;
  while i < cFULL_HEADER_LEN-1 do
  begin
    if (headerBuf[i] < #32) and (headerBuf[i] <> #0) then
    begin
      isBinary := False;
      Break;
    end;
    Inc(i);
  end;

  Mesh := TMeshObject.CreateOwned(Owner.MeshObjects);
  try
    Mesh.Mode := momTriangles;
    if isBinary then
    begin
         aStream.Seek(PositionBackup,soBeginning);
      aStream.Read(Header, SizeOf(TSTLHeader));
      for i := 0 to Header.nbFaces - 1 do
      begin
        aStream.Read(DataFace, SizeOf(TSTLFace));
        with DataFace do
        begin
          // STL faces have a normal, but do not necessarily follow the winding rule,
          // so we must first determine if the triangle is properly oriented
          // and rewind it properly if not...
          calcNormal := CalcPlaneNormal(v1, v2, v3);
          if VectorDotProduct(calcNormal, normal) > 0 then
            Mesh.Vertices.Add(v1, v2, v3)
          else
            Mesh.Vertices.Add(v3, v2, v1);
          Mesh.Normals.Add(normal, normal, normal);
        end;
      end;

    end
    else
    begin
      fileContent := TStringList.Create;
      sl := TStringList.Create;
      try
        fileContent.LoadFromStream(aStream);
        i := 0;
        curLine := Trim(UpperCase(fileContent[i]));
        if Pos(cSOLID_LABEL, curLine) = 1 then
        begin
          mesh.Vertices.Capacity := (fileContent.Count - 2) div 7;
          mesh.Normals.Capacity := (fileContent.Count - 2) div 7;
          Inc(i);
          curLine := Trim(UpperCase(fileContent[i]));
          while i < fileContent.Count do
          begin
            if Pos(cFACETNORMAL_LABEL, curLine) = 1 then
            begin
              DecodeSTLNormals(curLine, DataFace.normal);
              Inc(i);
              curLine := Trim(UpperCase(fileContent[i]));
              if Pos(cOUTERLOOP_LABEL, curLine) = 1 then
              begin
                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, DataFace.v1);

                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, DataFace.v2);

                Inc(i);
                curLine := Trim(fileContent[i]);
                DecodeSTLVertex(curLine, DataFace.v3);
              end;
              Inc(i);
              curLine := Trim(UpperCase(fileContent[i]));
              if Pos(cENDLOOP_LABEL, curLine) <> 1 then
                raise Exception.Create('End of Loop Not Found')
              else
              begin
                calcNormal := CalcPlaneNormal(DataFace.v1, DataFace.v2, DataFace.v3);
                if VectorDotProduct(calcNormal, DataFace.normal) > 0 then
                  mesh.Vertices.Add(DataFace.v1, DataFace.v2, dataFace.v3)
                else
                  Mesh.Vertices.Add(DataFace.v3, DataFace.v2, DataFace.v1);
                Mesh.Normals.Add(DataFace.normal, DataFace.normal, DataFace.normal);
              end;
            end;
            Inc(i);
            curLine := Trim(UpperCase(fileContent[i]));
            if Pos(cENDFACET_LABEL, curLine) <> 1 then
              raise Exception.Create('End of Facet Not found');
            Inc(i);
            curLine := Trim(UpperCase(fileContent[i]));
            if Pos(cENDSOLID_LABEL, curLine) = 1 then
              Break;
          end;
        end;
      finally
        sl.Free;
        fileContent.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      Mesh.Free;
    end;
  end;
end;

procedure TGLSTLVectorFile.SaveToStream(aStream: TStream);
var
  i: Integer;
  Header: TSTLHeader;
  DataFace: TSTLFace;
  List: TAffineVectorList;
const
  cHeaderTag = 'GLScene STL export';
begin
  List := Owner.MeshObjects.ExtractTriangles;
  try
    FillChar(Header.dummy[0], SizeOf(Header.dummy), 0);
    Move(cHeaderTag, Header.dummy[0], Length(cHeaderTag));
    Header.nbFaces := List.Count div 3;
    aStream.Write(Header, SizeOf(Header));
    i := 0;
    while i < list.Count do
    begin
      DataFace.normal := CalcPlaneNormal(List[i], List[i + 1], List[i + 2]);
      DataFace.v1 := List[i];
      DataFace.v2 := List[i + 1];
      DataFace.v3 := List[i + 2];
      aStream.Write(DataFace, SizeOf(DataFace));
      Inc(i, 3);
    end;
  finally
    List.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterVectorFileFormat('stl', 'Stereolithography files', TGLSTLVectorFile);

end.
