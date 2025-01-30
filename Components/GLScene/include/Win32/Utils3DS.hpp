// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Utils3DS.pas' rev: 36.00 (Windows)

#ifndef Utils3DSHPP
#define Utils3DSHPP

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
#include <File3DS.hpp>
#include <Types3DS.hpp>
#include <GLStrings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utils3ds
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ShowError(const System::UnicodeString ErrorMessage);
extern DELPHI_PACKAGE void __fastcall ShowErrorFormatted(const System::UnicodeString ErrorMessage, const System::TVarRec *Args, const System::NativeInt Args_High);
extern DELPHI_PACKAGE Types3ds::TMeshSet3DS __fastcall GetMeshSet(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TAtmosphere3DS __fastcall GetAtmosphere(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TBackground3DS __fastcall GetBackground(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TViewport3DS __fastcall GetViewport(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ChunkTagToString(System::Word Tag);
extern DELPHI_PACKAGE void __fastcall ChunkHeaderReport(System::Classes::TStrings* &Strings, Types3ds::PChunk3DS Chunk, int IndentLevel);
extern DELPHI_PACKAGE void __fastcall DumpKeyHeader(System::Classes::TStrings* Strings, const Types3ds::TKeyHeader3DS &Key, int IndentLevel);
extern DELPHI_PACKAGE void __fastcall DumpChunk(File3ds::TFile3DS* const Source, System::Classes::TStrings* &Strings, Types3ds::PChunk3DS Chunk, int IndentLevel, Types3ds::TDumpLevel DumpLevel);
extern DELPHI_PACKAGE void __fastcall AddChild(Types3ds::PChunk3DS Parent, Types3ds::PChunk3DS Child);
extern DELPHI_PACKAGE void __fastcall AddChildOrdered(Types3ds::PChunk3DS Parent, Types3ds::PChunk3DS Child);
extern DELPHI_PACKAGE Types3ds::PChunk3DS __fastcall FindChunk(Types3ds::PChunk3DS Top, System::Word Tag);
extern DELPHI_PACKAGE Types3ds::PChunk3DS __fastcall FindNextChunk(Types3ds::PChunk3DS Local, System::Word Tag);
extern DELPHI_PACKAGE void __fastcall FreeChunkData(Types3ds::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall InitChunk(Types3ds::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall ReleaseChunk(Types3ds::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall ReleaseChunkList(Types3ds::PChunkList3DS &List);
extern DELPHI_PACKAGE int __fastcall GetMaterialCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseMaterial(Types3ds::PMaterial3DS Mat);
extern DELPHI_PACKAGE Types3ds::PChunk3DS __fastcall FindNamedObjectByIndex(File3ds::TFile3DS* Source, const Types3ds::TDatabase3DS &DB, System::Word AType, int Index);
extern DELPHI_PACKAGE void __fastcall DeleteChunk(Types3ds::PChunk3DS &Chunk);
extern DELPHI_PACKAGE int __fastcall GetChunkValue(System::Word Tag);
extern DELPHI_PACKAGE Types3ds::TMaterial3DS __fastcall GetMaterialByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE int __fastcall GetMeshCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseMeshObj(Types3ds::PMesh3DS Mesh);
extern DELPHI_PACKAGE Types3ds::TMesh3DS __fastcall GetMeshByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE int __fastcall GetOmnilightCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE int __fastcall GetSpotlightCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseLight(Types3ds::PLight3DS Light);
extern DELPHI_PACKAGE Types3ds::TLight3DS __fastcall GetOmnilightByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE Types3ds::TLight3DS __fastcall GetSpotlightByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE void __fastcall ReleaseCamera(Types3ds::PCamera3DS Camera);
extern DELPHI_PACKAGE int __fastcall GetCameraCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TCamera3DS __fastcall GetCameraByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE Types3ds::TKFSets3DS __fastcall GetKFSettings(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseCameraMotion(Types3ds::PKFCamera3DS Camera);
extern DELPHI_PACKAGE void __fastcall GetCameraNodeNameList(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE int __fastcall GetCameraNodeCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TKFCamera3DS __fastcall GetCameraMotion(File3ds::TFile3DS* const Source, Types3ds::PChunk3DS CamChunk, Types3ds::PChunk3DS TargetChunk);
extern DELPHI_PACKAGE Types3ds::TKFCamera3DS __fastcall GetCameraMotionByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE void __fastcall ReleaseAmbientLightMotion(Types3ds::PKFAmbient3DS Light);
extern DELPHI_PACKAGE Types3ds::TKFAmbient3DS __fastcall GetAmbientLightMotion(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall InitObjectMotion(Types3ds::TKFMesh3DS &Obj, unsigned NewNPKeys, unsigned NewNRKeys, unsigned NewNSKeys, unsigned NewNMKeys, unsigned NewNHKeys);
extern DELPHI_PACKAGE void __fastcall ReleaseObjectMotion(Types3ds::PKFMesh3DS Obj);
extern DELPHI_PACKAGE int __fastcall GetObjectNodeCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetObjectNodeNameList(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Types3ds::TKFMesh3DS __fastcall GetObjectMotionByName(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Types3ds::TKFMesh3DS __fastcall GetObjectMotionByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE void __fastcall ReleaseOmnilightMotion(Types3ds::PKFOmni3DS Light);
extern DELPHI_PACKAGE unsigned __fastcall GetOmnilightNodeCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetOmnilightNodeNameList(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Types3ds::TKFOmni3DS __fastcall GetOmnilightMotionByName(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Types3ds::TKFOmni3DS __fastcall GetOmnilightMotionByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE void __fastcall ReleaseSpotlightMotion(Types3ds::PKFSpot3DS Spot);
extern DELPHI_PACKAGE unsigned __fastcall GetSpotlightNodeCount(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetSpotlightNodeNameList(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Types3ds::TKFSpot3DS __fastcall GetSpotlightMotionByName(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Types3ds::TKFSpot3DS __fastcall GetSpotlightMotionByIndex(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE Types3ds::TReleaseLevel __fastcall GetM3dMagicRelease(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TReleaseLevel __fastcall GetMeshRelease(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TReleaseLevel __fastcall GetKfRelease(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
extern DELPHI_PACKAGE Types3ds::TReleaseLevel __fastcall GetDatabaseRelease(File3ds::TFile3DS* const Source, Types3ds::TDatabase3DS &DB);
}	/* namespace Utils3ds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTILS3DS)
using namespace Utils3ds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utils3DSHPP
