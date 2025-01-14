// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCoordinates.pas' rev: 36.00 (Windows)

#ifndef GlcoordinatesHPP
#define GlcoordinatesHPP

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
#include <GLVectorGeometry.hpp>
#include <GLVectorTypes.hpp>
#include <OpenGLTokens.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glcoordinates
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCustomCoordinates;
class DELPHICLASS TGLCoordinates2;
class DELPHICLASS TGLCoordinates3;
class DELPHICLASS TGLCoordinates4;
__interface DELPHIINTERFACE IGLCoordinatesUpdateAble;
typedef System::DelphiInterface<IGLCoordinatesUpdateAble> _di_IGLCoordinatesUpdateAble;
class DELPHICLASS TGLCoordinatesUpdateAbleComponent;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLCoordinatesStyle : unsigned char { csPoint2D, csPoint, csVector, csUnknown };

class PASCALIMPLEMENTATION TGLCustomCoordinates : public Glbaseclasses::TGLUpdateAbleObject
{
	typedef Glbaseclasses::TGLUpdateAbleObject inherited;
	
public:
	Opengltokens::TGLfloat operator[](const int AIndex) { return this->Coordinate[AIndex]; }
	
private:
	Glvectorgeometry::TVector FCoords;
	TGLCoordinatesStyle FStyle;
	Glvectorgeometry::PVector FPDefaultCoords;
	void __fastcall SetAsPoint2D(const Glvectortypes::TVector2f &Value);
	void __fastcall SetAsVector(const Glvectorgeometry::TVector &Value);
	void __fastcall SetAsAffineVector(const Glvectorgeometry::TAffineVector &Value);
	Glvectorgeometry::TAffineVector __fastcall GetAsAffineVector();
	Glvectortypes::TVector2f __fastcall GetAsPoint2D();
	System::UnicodeString __fastcall GetAsString();
	Opengltokens::TGLfloat __fastcall GetCoordinate(const int AIndex);
	void __fastcall SetCoordinate(const int AIndex, const Opengltokens::TGLfloat AValue);
	Opengltokens::TGLfloat __fastcall GetDirectCoordinate(const int Index);
	void __fastcall SetDirectCoordinate(const int Index, const Opengltokens::TGLfloat AValue);
	
protected:
	void __fastcall SetDirectVector(const Glvectorgeometry::TVector &V);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall TGLCustomCoordinates(System::Classes::TPersistent* AOwner, const Glvectorgeometry::TVector &AValue, const TGLCoordinatesStyle AStyle);
	__fastcall virtual ~TGLCustomCoordinates();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall WriteToFiler(System::Classes::TWriter* Writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* Reader);
	void __fastcall Initialize(const Glvectorgeometry::TVector &Value);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property TGLCoordinatesStyle Style = {read=FStyle, write=FStyle, nodefault};
	void __fastcall Translate(const Glvectorgeometry::TVector &TranslationVector)/* overload */;
	void __fastcall Translate(const Glvectorgeometry::TAffineVector &TranslationVector)/* overload */;
	void __fastcall AddScaledVector(const float Factor, const Glvectorgeometry::TVector &TranslationVector)/* overload */;
	void __fastcall AddScaledVector(const float Factor, const Glvectorgeometry::TAffineVector &TranslationVector)/* overload */;
	void __fastcall Rotate(const Glvectorgeometry::TAffineVector &AnAxis, float AnAngle)/* overload */;
	void __fastcall Rotate(const Glvectorgeometry::TVector &AnAxis, float AnAngle)/* overload */;
	void __fastcall Normalize();
	void __fastcall Invert();
	void __fastcall Scale(float Factor);
	Opengltokens::TGLfloat __fastcall VectorLength();
	Opengltokens::TGLfloat __fastcall VectorNorm();
	float __fastcall MaxXYZ();
	HIDESBASE bool __fastcall Equals(const Glvectorgeometry::TVector &AVector);
	void __fastcall SetVector(const float X, const float Y, float Z = 0.000000E+00f)/* overload */;
	void __fastcall SetVector(const float X, const float Y, const float Z, const float W)/* overload */;
	void __fastcall SetVector(const Glvectorgeometry::TAffineVector &V)/* overload */;
	void __fastcall SetVector(const Glvectorgeometry::TVector &V)/* overload */;
	void __fastcall SetPoint(const float X, const float Y, const float Z)/* overload */;
	void __fastcall SetPoint(const Glvectorgeometry::TAffineVector &V)/* overload */;
	void __fastcall SetPoint(const Glvectorgeometry::TVector &V)/* overload */;
	void __fastcall SetPoint2D(const float X, const float Y)/* overload */;
	void __fastcall SetPoint2D(const Glvectorgeometry::TAffineVector &Vector)/* overload */;
	void __fastcall SetPoint2D(const Glvectorgeometry::TVector &Vector)/* overload */;
	void __fastcall SetPoint2D(const Glvectortypes::TVector2f &Vector)/* overload */;
	void __fastcall SetToZero();
	Opengltokens::PGLfloat __fastcall AsAddress();
	__property Glvectorgeometry::TVector AsVector = {read=FCoords, write=SetAsVector};
	__property Glvectorgeometry::TAffineVector AsAffineVector = {read=GetAsAffineVector, write=SetAsAffineVector};
	__property Glvectortypes::TVector2f AsPoint2D = {read=GetAsPoint2D, write=SetAsPoint2D};
	__property Opengltokens::TGLfloat X = {read=GetCoordinate, write=SetCoordinate, index=0};
	__property Opengltokens::TGLfloat Y = {read=GetCoordinate, write=SetCoordinate, index=1};
	__property Opengltokens::TGLfloat Z = {read=GetCoordinate, write=SetCoordinate, index=2};
	__property Opengltokens::TGLfloat W = {read=GetCoordinate, write=SetCoordinate, index=3};
	__property Opengltokens::TGLfloat Coordinate[const int AIndex] = {read=GetCoordinate, write=SetCoordinate/*, default*/};
	__property System::UnicodeString AsString = {read=GetAsString};
	__property Glvectorgeometry::TVector DirectVector = {read=FCoords, write=SetDirectVector};
	__property Opengltokens::TGLfloat DirectX = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=0};
	__property Opengltokens::TGLfloat DirectY = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=1};
	__property Opengltokens::TGLfloat DirectZ = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=2};
	__property Opengltokens::TGLfloat DirectW = {read=GetDirectCoordinate, write=SetDirectCoordinate, index=3};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCustomCoordinates(System::Classes::TPersistent* AOwner) : Glbaseclasses::TGLUpdateAbleObject(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCoordinates2 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates2(System::Classes::TPersistent* AOwner, const Glvectorgeometry::TVector &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates2() { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates2(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCoordinates3 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
	__property Z = {stored=false, index=2, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates3(System::Classes::TPersistent* AOwner, const Glvectorgeometry::TVector &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates3() { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates3(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


class PASCALIMPLEMENTATION TGLCoordinates4 : public TGLCustomCoordinates
{
	typedef TGLCustomCoordinates inherited;
	
__published:
	__property X = {stored=false, index=0, default=0};
	__property Y = {stored=false, index=1, default=0};
	__property Z = {stored=false, index=2, default=0};
	__property W = {stored=false, index=3, default=0};
public:
	/* TGLCustomCoordinates.CreateInitialized */ inline __fastcall TGLCoordinates4(System::Classes::TPersistent* AOwner, const Glvectorgeometry::TVector &AValue, const TGLCoordinatesStyle AStyle) : TGLCustomCoordinates(AOwner, AValue, AStyle) { }
	/* TGLCustomCoordinates.Destroy */ inline __fastcall virtual ~TGLCoordinates4() { }
	
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLCoordinates4(System::Classes::TPersistent* AOwner) : TGLCustomCoordinates(AOwner) { }
	
};


typedef TGLCoordinates3 TGLCoordinates;

__interface  INTERFACE_UUID("{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}") IGLCoordinatesUpdateAble  : public System::IInterface 
{
	virtual void __fastcall CoordinateChanged(TGLCustomCoordinates* Sender) = 0 ;
};

class PASCALIMPLEMENTATION TGLCoordinatesUpdateAbleComponent : public Glbaseclasses::TGLUpdateAbleComponent
{
	typedef Glbaseclasses::TGLUpdateAbleComponent inherited;
	
public:
	virtual void __fastcall CoordinateChanged(TGLCustomCoordinates* Sender) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCoordinatesUpdateAbleComponent(System::Classes::TComponent* AOwner) : Glbaseclasses::TGLUpdateAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLCoordinatesUpdateAbleComponent() { }
	
private:
	void *__IGLCoordinatesUpdateAble;	// IGLCoordinatesUpdateAble 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}
	operator _di_IGLCoordinatesUpdateAble()
	{
		_di_IGLCoordinatesUpdateAble intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IGLCoordinatesUpdateAble*(void) { return (IGLCoordinatesUpdateAble*)&__IGLCoordinatesUpdateAble; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool VUseDefaultCoordinateSets;
}	/* namespace Glcoordinates */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOORDINATES)
using namespace Glcoordinates;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcoordinatesHPP
