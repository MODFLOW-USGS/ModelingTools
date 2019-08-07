unit Export2DMeshUnit;

interface

uses
  Classes;

procedure Export2DMesh(const AFileName: string; OnExport: TNotifyEvent);

implementation

uses
  frmGoPhastUnit, SutraMeshUnit, IOUtils, frmProgressUnit;

procedure ExportArgusMesh(const AFileName: string; OnExport: TNotifyEvent);
var
  Mesh2D: TSutraMesh2D;
  MeshWriter: TStreamWriter;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
begin
  Mesh2D := frmGoPhast.SutraMesh.Mesh2D;
  MeshWriter := TFile.CreateText(AFileName);
  try
    MeshWriter.Write(Mesh2D.Elements.Count);
    MeshWriter.Write(#9);
    MeshWriter.WriteLine(Mesh2D.Nodes.Count);
    for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
    begin
      ANode := Mesh2D.Nodes[NodeIndex];
      MeshWriter.Write('N'#9);
      MeshWriter.Write(NodeIndex+1);
      MeshWriter.Write(#9);
      MeshWriter.Write(ANode.X);
      MeshWriter.Write(#9);
      MeshWriter.WriteLine(ANode.Y);
      if Assigned(OnExport) then
      begin
        OnExport(nil);
      end;
      if not frmFileProgress.ShouldContinue then
      begin
        Exit;
      end;
    end;
    for ElementIndex := 0 to Mesh2D.Elements.Count - 1 do
    begin
      AnElement := Mesh2D.Elements[ElementIndex];
      MeshWriter.Write('E'#9);
      MeshWriter.Write(ElementIndex+1);
      MeshWriter.Write(#9);
      for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
      begin
        ANode := AnElement.Nodes[NodeIndex].Node;
        MeshWriter.Write(ANode.Index+1);
        if NodeIndex < AnElement.Nodes.Count - 1 then
        begin
          MeshWriter.Write(#9);
        end
        else
        begin
          MeshWriter.WriteLine;
        end;
      end;
      if Assigned(OnExport) then
      begin
        OnExport(nil);
      end;
      if not frmFileProgress.ShouldContinue then
      begin
        Exit;
      end;
    end;
  finally
    MeshWriter.Free;
  end;
end;


procedure Export2DMesh(const AFileName: string; OnExport: TNotifyEvent);
begin
  ExportArgusMesh(AFileName, OnExport)
end;

end.
