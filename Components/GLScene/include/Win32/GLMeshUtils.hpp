// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshUtils.pas' rev: 36.00 (Windows)

#ifndef GlmeshutilsHPP
#define GlmeshutilsHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glmeshutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TSubdivideEdgeEvent)(const int idxA, const int idxB, const int newIdx);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vImprovedFixingOpenTriangleEdge;
extern DELPHI_PACKAGE System::LongWord vEdgeInfoReserveSize;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Glvectorlists::TAffineVectorList* const strip, Glvectorlists::TAffineVectorList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Glvectorlists::TIntegerList* const strip, Glvectorlists::TIntegerList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Glvectorlists::TAffineVectorList* const strip, Glvectorlists::TIntegerList* const indices, Glvectorlists::TAffineVectorList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertIndexedListToList(Glvectorlists::TAffineVectorList* const data, Glvectorlists::TIntegerList* const indices, Glvectorlists::TAffineVectorList* list);
extern DELPHI_PACKAGE Glvectorlists::TIntegerList* __fastcall BuildVectorCountOptimizedIndices(Glvectorlists::TAffineVectorList* const vertices, Glvectorlists::TAffineVectorList* const normals = (Glvectorlists::TAffineVectorList*)(0x0), Glvectorlists::TAffineVectorList* const texCoords = (Glvectorlists::TAffineVectorList*)(0x0));
extern DELPHI_PACKAGE void __fastcall RemapReferences(Glvectorlists::TAffineVectorList* reference, Glvectorlists::TIntegerList* const indices)/* overload */;
extern DELPHI_PACKAGE void __fastcall RemapReferences(Glvectorlists::TIntegerList* reference, Glvectorlists::TIntegerList* const indices)/* overload */;
extern DELPHI_PACKAGE void __fastcall RemapAndCleanupReferences(Glvectorlists::TAffineVectorList* reference, Glvectorlists::TIntegerList* indices);
extern DELPHI_PACKAGE Glvectorlists::TIntegerList* __fastcall RemapIndicesToIndicesMap(Glvectorlists::TIntegerList* remapIndices);
extern DELPHI_PACKAGE void __fastcall RemapTrianglesIndices(Glvectorlists::TIntegerList* indices, Glvectorlists::TIntegerList* indicesMap);
extern DELPHI_PACKAGE void __fastcall RemapIndices(Glvectorlists::TIntegerList* indices, Glvectorlists::TIntegerList* indicesMap);
extern DELPHI_PACKAGE void __fastcall UnifyTrianglesWinding(Glvectorlists::TIntegerList* indices);
extern DELPHI_PACKAGE void __fastcall InvertTrianglesWinding(Glvectorlists::TIntegerList* indices);
extern DELPHI_PACKAGE Glvectorlists::TAffineVectorList* __fastcall BuildNormals(Glvectorlists::TAffineVectorList* reference, Glvectorlists::TIntegerList* indices);
extern DELPHI_PACKAGE Glvectorlists::TIntegerList* __fastcall BuildNonOrientedEdgesList(Glvectorlists::TIntegerList* triangleIndices, Glvectorlists::TIntegerList* triangleEdges = (Glvectorlists::TIntegerList*)(0x0), Glvectorlists::TIntegerList* edgesTriangles = (Glvectorlists::TIntegerList*)(0x0));
extern DELPHI_PACKAGE void __fastcall IncreaseCoherency(Glvectorlists::TIntegerList* indices, int cacheSize);
extern DELPHI_PACKAGE void __fastcall WeldVertices(Glvectorlists::TAffineVectorList* vertices, Glvectorlists::TIntegerList* indicesMap, float weldRadius);
extern DELPHI_PACKAGE Glpersistentclasses::TPersistentObjectList* __fastcall StripifyMesh(Glvectorlists::TIntegerList* indices, int maxVertexIndex, bool agglomerateLoneTriangles = false);
extern DELPHI_PACKAGE void __fastcall SubdivideTriangles(float smoothFactor, Glvectorlists::TAffineVectorList* vertices, Glvectorlists::TIntegerList* triangleIndices, Glvectorlists::TAffineVectorList* normals = (Glvectorlists::TAffineVectorList*)(0x0), TSubdivideEdgeEvent onSubdivideEdge = 0x0);
extern DELPHI_PACKAGE Glvectorlists::TLongWordList* __fastcall MakeTriangleAdjacencyList(const Glvectorgeometry::PLongWordArray AindicesList, System::LongWord Count, const Glvectorgeometry::PAffineVectorArray AVerticesList);
extern DELPHI_PACKAGE Glvectorlists::TLongWordList* __fastcall ConvertStripToList(const Glvectorgeometry::PLongWordArray AindicesList, System::LongWord Count, System::LongWord RestartIndex)/* overload */;
extern DELPHI_PACKAGE Glvectorlists::TLongWordList* __fastcall ConvertFansToList(const Glvectorgeometry::PLongWordArray AindicesList, System::LongWord Count, System::LongWord RestartIndex);
}	/* namespace Glmeshutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHUTILS)
using namespace Glmeshutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshutilsHPP
