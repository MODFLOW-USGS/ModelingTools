unit frameMetaDataEditorUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.TreeView, System.ImageList,
  FMX.ImgList, MetaDataTreeViewItems, MetaDataInterfacesUnit, Xml.VerySimple,
  Mobile.OpenURL, FMX.Platform, frmSearchUnit, FMX.Objects;

type
  TframeMetaDataEditor = class(TFrame)
    ilAddButton: TImageList;
    tmr1: TTimer;
    tvMetaData: TTreeView;
    spl1: TSplitter;
    pnl1: TPanel;
    btnDuplicate: TButton;
    btnExpandAll: TButton;
    btnExpandUsed: TButton;
    btnCollapseAll: TButton;
    imgManditory: TImage;
    imgMandIfAppl: TImage;
    imgOptional: TImage;
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnDuplicateClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnExpandUsedClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure tvMetaDataChange(Sender: TObject);
    procedure tvMetaDataChangeCheck(Sender: TObject);
    procedure FrameDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FrameDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
  private
    FRootItem: TCompoundItemMetaDataItem;
    FSelectedItem: TCustomMetaDataItem;
    FXml: TXmlVerySimple;
    FReviewItem: TDateMetaDataItem;
    FChanged: Boolean;
    FRadioButton: TRadioButton;
    procedure ContentChanged(Sender: TObject);
    procedure RadioButtonChanged(Sender: TObject);
    procedure NumberItemPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure SetSelectedItem(const Value: TCustomMetaDataItem);
    function CreateTreeViewItem(Parent: TCustomMetaDataItem): TTreeViewItem;
    // 0
    function CreateMetaDataRoot(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1
    function CreateIdInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.1
    function CreateCitation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.2
    function CreateDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.2.1
    function CreateAbstract(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.2.2
    function CreatePurpose(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.2.3
    function CreateSupplimentalInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.3
    function CreateTimePeriod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.3.1
    function CreateCurrentRef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.4
    function CreateStatus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.4.1
    function CreateProgress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.4.2
    function CreateUpdateFrequency(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.5
    function CreateSpatialDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.5.1
    function CreateBoundingCoordinates(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.5.2
    function CreateGPolygon(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.5.2.1
    function CreateGPolygonOuterRing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.5.2.2
    function CreateGPolygonExclusionRing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6
    function CreateKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.1
    function CreateTheme(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.1.1
    function CreateThemeThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.1.2
    function CreateThemeKeyword(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.2
    function CreatePlace(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.2.1
    function CreatePlaceThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.2.2
    function CreatePlaceKeyword(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.3
    function CreateStratum(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.3.1
    function CreateStratumThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.3.2
    function CreateStratumKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.4
    function CreateTemporal(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.4.1
    function CreateTemporalThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.6.4.2
    function CreateTemporalKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.7
    function CreateAccessConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.8
    function CreateUseConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.9
    function CreatePointOfContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.10 - Repeatable
    function CreateBrowseGraphic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.10.1 -
    function CreateBrowseGraphicFileName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.10.2 -
    function CreateBrowseGraphicFileDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.10.3 -
    function CreateBrowseGraphicFileType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.11
    function CreateDataSetCredit(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.12
    function CreateSecurityInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.12.1
    function CreateSecuritySystem(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.12.2
    function CreateSecurityClassification(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.12.3
    function CreateSecurityHandling(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.13
    function CreateNativeDataSetEnvironment(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 1.14 - Repeatable
    function CreateCrossReference(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2
    function CreateDataQuality(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.1
    function CreateAttributeAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.1.1
    function CreateAttributeAccuracyReport(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.1.2
    function CreateQuantitativeAttributeAccuracyAssessment(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.1.2.1
    function CreateAttributeAccuracyValue(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.1.2.2
    function CreateAttributeAccuracyExplanation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.2
    function CreateLogicalConsistency(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.3
    function CreateCompleteness(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4
    function CreatePositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.1
    function CreateHorizPositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.1.1
    function CreateHorizPositionalAccuracyReport(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.1.2
    function CreateQuantitativeHorizPositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.1.2.1
    function CreateHorizPositionalAccuracyValue(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.1.2.2
    function CreateHorizPositionalAccuracyExplanation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.2
    function CreateVertPositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.2.1
    function CreateVertPositionalAccuracyReport(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.2.2
    function CreateQuantitativeVertPositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.2.2.1
    function CreateVertPositionalAccuracyValue(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.4.2.2.2
    function CreateVertPositionalAccuracyExplanation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5
    function CreateLineage(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1
    function CreateSourceInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.1
    function CreateSourceCiteInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.2
    function CreateSourceScaleDenominator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.3
    function CreateTypeOfSourceMedia(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.4
    function CreateSourceTimePeriod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.4.1
    function CreateSourceCurrentness(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.5
    function CreateSourceCiteAbbr(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.1.6
    function CreateSourceContribution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2
    function CreateProcessStep(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.1
    function CreateProcessDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.2
    function CreateSourceUsedCiteAbbr(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.3
    function CreateProcessDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.4
    function CreateProcessTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.5
    function CreateSourceProducedCiteAbbr(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.5.2.6
    function CreateProcessContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 2.6
    function CreateCloudCover(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3
    function CreateSpatialDataOrgInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.1
    function CreateIndirectSpatialReference(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.2
    function CreateDirectSpatialReferenceMethod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3
    function CreatePointAndVectorObjectInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.1
    function CreateStdsTermsDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.1.1
    function CreateSDTSPointAndVectorObjectType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.1.2
    function CreatePointAndVectorObjectCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.2
    function CreateVpfTermsDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.2.1
    function CreateVpfLevel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.2.2
    function CreateVpfInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.3.2.2.1
    function CreateVpfInfoObjectType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.4
    function CreateRasterInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.4.1
    function CreateRasterObjectType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.4.2
    function CreateRasterRowCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.4.3
    function CreateRasterColumnCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 3.4.4
    function CreateRasterVerticalCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4
    function CreateSpatialReferenceInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1
    function CreateHorizontalCoordSysDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.1
    function CreateHorizontalCoordSysGeographic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.1.1
    function CreateLatitudeResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.1.2
    function CreateLongitudeResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.1.3
    function CreateGeoCoordUnits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2
    function CreatePlanar(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1
    function CreateMapProjection(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.1
    function CreateMapProjectionName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.2
    function CreateAlbersConicalEqualArea(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.3
    function CreateAzimuthEqualArea(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.4
    function CreateEquidistantConic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.5
    function CreateEquirectangular(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.6
    function CreateGenVertNearPersp(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.7
    function CreateGnomonic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.8
    function CreateLamAzEqArea(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.9
   function CreateLambertConformalConic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.10
    function CreateMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.11
    function CreateModSteroAlaska(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.12
    function CreateMillerCylindrical(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.13
    function CreateObliqueMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.14
    function CreateOrthographic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.15
    function CreatePolarStereoGraphic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.16
    function CreatePolyconic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.17
    function CreateRobinson(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.18
    function CreateSinusoidal(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.19
    function CreateSpaceObliqueMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.20
    function CreateStereographic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.21
    function CreateTransverseMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.22
    function CreateVanDerGrinten(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23
    function CreateMapProjParam(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.1 stdparll
    function CreateStandardParallel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.2 longcm
    function CreateLongitudeStrdMeridian(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.3 latprjo
    function CreateLatProjOrigin(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.4 feast
    function CreateFalseEasting(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.5 fnorth
    function CreateFalseNorthing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.6 sfequat
    function CreateScaleFactorAtEquator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.7 heightpt
    function CreateHeightOfPerspectivePoint(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.8 longpc
    function CreateLongPerspCenter(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.9 latprjc
    function CreateLatPerspCenter(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.10 sfctrlin
    function CreateScaleFactorAtCenterLine(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.11 obqlazim
    function CreateObliqueLineAzimuth(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.11.1
    function CreateAzimuthAngle(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.11.2
    function CreateAzimuthMeasPtLong(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.12 obqlpt
    function CreateObliqueLinePoint(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.12.1
    function CreateObliqueLineLatitude(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.12.2
    function CreateObliqueLineLongitude(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.13 svlong
    function CreateStraightVertLineFromPole(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.14 sfprjorg
    function CreateScaleFactOfProjOrigin(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.15 landsat
    function CreateLandsatNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.16 pathnum
    function CreateLandsatPathNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.17 sfctrmer
    function CreateScaleFactorAtCentMerid(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.1.23.18 otherprj
    function CreateOtherProjDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2
    function CreateGridCoordSystem(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.1
    function CreateGridCoordSystemName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.2
    function CreateUTM(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.2.1
    function CreateUTMZoneNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.3
    function CreateUnivPolarStereo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.3.1
    function CreateUnivPolarStereoZoneID(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.4
    function CreateStatePlaneCoordSys(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.4.1
    function CreateStatePlaneCoordSysZoneID(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.5
    function CreateArcCoordSys(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.5.1
    function CreateArcSysZoneID(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.2.6
    function CreateOtherCoordSys(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.3
    function CreateLocalPlanar(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.3.1
    function CreateLocalPlanarDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.3.2
    function CreateLocalPlanarGeoref(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4
    function CreatePlanarCoordInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.1
    function CreatePlanarCoordEncodeMethod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.2
    function CreateCoordRepresentation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.2.1
    function CreateAbscissaResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.2.2
    function CreateOrdinateResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3
    function CreateDistAndBearingRepresentation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3.1
    function CreateDistResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3.2
    function CreateBearingResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3.3
    function CreateBearingUnits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3.4
    function CreateBearingRefDir(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.3.5
    function CreateBearingRefMeridian(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.2.4.4
    function CreatePlanarDistanceUnits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.3
    function CreateLocal(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.3.1
    function CreateLocalDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.3.2
    function CreateLocalGeoref(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.4
    function CreateGeodeticModel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.4.1
    function CreateHorizontalDatumName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.4.2
    function CreateEllipsoidName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.4.3
    function CreateSemiMajorAxis(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.1.4.4
    function CreateDenFlatRatio(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2
    function CreateVertCoordSysDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.1
    function CreateAltitudeSysDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.1.1
    function CreateAltitudeDatumName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.1.2
    function CreateAltitudeResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.1.3
    function CreateAltitudeDistanceUnit(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.1.4
    function CreateAltitudeEncodingMethod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.2
    function CreateDepthSysDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.2.1
    function CreateDepthDatumName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.2.2
    function CreateDepthResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.2.3
    function CreateDepthDistanceUnit(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 4.2.2.4
    function CreateDepthEncodingMethod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5
    function CreateEntityAttributesInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1
    function CreateDetailedDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.1
    function CreateEntityType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.1.1
    function CreateEntityTypeLabel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.1.2
    function CreateEntityTypeDefinition(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.1.3
    function CreateEntityTypeDefinitionSource(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2
    function CreateAttribute(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.1
    function CreateAttributeLabel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.2
    function CreateAttributeDefinition(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.3
    function CreateAttributeDefinitionSource(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4
    function CreateAttributeDomainValue(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.1
    function CreateEnumeratedDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.1.1
    function CreateEnumeratedDomainValue(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.1.2
    function CreateEnumeratedDomainValueDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.1.3
    function CreateEnumeratedDomainValueDefSource(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.2
    function CreateRangeDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.2.1
    function CreateRangeDomainMin(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.2.2
    function CreateRangeDomainMax(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.2.3
    function CreateAttributeUnitsOfMeasure(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.2.4
    function CreateAttributeMeasureRes(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.3
    function CreateCodesetDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.3.1
    function CreateCodesetName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.3.2
    function CreateCodesetSource(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.4.4
    function CreateUnrepresentableDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.5
    function CreateAttBeginDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.6
    function CreateAttEndDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.7
    function CreateAttValueAccuracyInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.7.1
    function CreateAttValueAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.7.2
    function CreateAttValueAccuracyExplanation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.1.2.8
    function CreateAttMeasurementFrequency(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.2
    function CreateOverviewDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.2.1
    function CreateEntityAndAttributeOverviewDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 5.2.2
    function CreateEntityAndAttributeCitation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6
    function CreateDistributionInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.1
    function CreateDistributor(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.2
    function CreateResourceDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.3
    function CreateDistributionLiability(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4
    function CreateStandardOrderProcess(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.1
    function CreateNonDigitalForm(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2
    function CreateDigitalForm(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1
    function CreateDigitalTransferInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.1
    function CreateFormatName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.2
    function CreateFormatVersionNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.3
    function CreateFormatVersionDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.4
    function CreateFormatSpecification(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.5
    function CreateFormatInformationContent(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.6
    function CreateFileDecompTech(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.1.7
    function CreateTransferSize(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2
    function CreateDigitalTransferOption(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1
    function CreateOnlineOption(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1
    function CreateComputerContactInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.1
    function CreateNetworkAddress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.1.1
    function CreateNetworkResourceName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2
    function CreateDialupInstructions(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.1
    function CreateLowestBPS(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.2
    function CreateHighestBPS(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.3
    function CreateNumDataBits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.4
    function CreateNumStopBits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.5
    function CreateParity(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.6
    function CreateCompressionSupport(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.7
    function CreateDialupTel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.1.2.8
    function CreateDialupFileName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.2
    function CreateAccessInst(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.1.3
    function CreateOnlineCompAndOpSys(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2
    function CreateOfflineOption(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.1
    function CreateOfflineMedia(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.2
    function CreateRecordingCapacity(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.2.1
    function CreateRecordingDensity(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.2.2
    function CreateRecordingDensityUnits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.3
    function CreateRecordingFormat(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.2.2.2.4
    function CreateCompatInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.3
    function CreateFees(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.4
    function CreateOrderingInstructions(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.4.5
    function CreateTurnAround(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.5
    function CreateCustomOrderProcess(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.6   { TODO : continue work from here. }
    function CreateTechPrerequisites(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 6.7
    function CreateAvailableTimePeriod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7
    function CreateReferenceInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.1
    function CreateReferenceDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.2
    function CreateMetaDataReviewDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.3
    function CreateMetaDataFutureReviewDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.4
    function CreateContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.5
    function CreateStandardName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.6
    function CreateStandardVersion(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.7
    function CreateMetaTimeConvention(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.8
    function CreateMetaAccessConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.9
    function CreateMetaUseConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.10
    function CreateMetaSecurityInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.10.1
    function CreateMetaSecurityClassSys(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.10.2
    function CreateMetaSecurityClassification(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.10.3
    function CreateMetaSecurityHandlingDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.11
    function CreateMetaExtensions(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.11.1
    function CreateOnlineLinkage(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 7.11.2
    function CreateProfileName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8
    function CreateCiteInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.1
    function CreateOriginator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.2
    function CreatePublicationDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.3
    function CreatePublicationTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.4
    function CreateTitle(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.5
    function CreateEdition(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.6
    function CreateGeospatialForm(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.7
    function CreateSeriesInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.7.1
    function CreateSeriesName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.7.2
    function CreateIssueID(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.8
    function CreatePubInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.8.1
    function CreatePubPlace(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.8.2
    function CreatePublisher(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.9
    function CreateOtherCiteInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.10
    function CreateOnlineCiteLinkage(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 8.11
    function CreateLargerWorkCite(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9
    function CreateTimePeriodInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.1
    function CreateSingleDateTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.1.1
    function CreateCalendarDateTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.1.2
    function CreateTimeOfDay(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.2
    function CreateMultipleDateTimes(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.3
    function CreateRangeOfTimes(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.3.1
    function CreateBeginDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.3.2
    function CreateBeginningTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.3.3
    function CreateEndDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 9.3.4
    function CreateEndTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10
    function CreateContactInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.1
    function CreateContactPersonPrimary(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.1.1
    function CreateContactPerson(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.1.2
    function CreateContactOrganization(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.2
    function CreateContactOrganizationPrimary(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.3
    function CreateContactPosition(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4
    function CreateContactAddress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.1
    function CreateAddressType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.2
    function CreateAddressLine(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.3
    function CreateContactCity(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.4
    function CreateStateOrProvince(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.5
    function CreatePostalCode(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.4.6
    function CreateCountry(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    { Private declarations }
    // 10.5
    function CreateContactTelephone(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.6
    function CreateContactHandicapTelephone(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.7
    function CreateContactFaxTelephone(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.8
    function CreateEMailAddress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.9
    function CreateHoursOfService(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    // 10.10
    function CreateContactInstructions(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    procedure UpdateCiteAbbreviations(Sender: TObject);
    procedure OnLargerCheckChecked(Sender: TObject);
    procedure AddAttributeChildren(MetaDataItem: TCustomMetaDataItem);
    procedure OnAttributeCheck(Sender: TObject);
    procedure CheckRadioButton(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure TextItemPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure PolygonItemPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure ExtentItemPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure InvalidateArchiveTreeView;
    procedure OptionalRadioButtonDoubleClicked(Sender: TObject);
    procedure DuplicateItem(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DrawTextOnImage(AnImage: TImage; AColor: TAlphaColor; TextToShow: string);
    procedure OnAttributeItemSelect(Sender: TObject);
    procedure UpdateChecks(AnItem: TCustomMetaDataItem);
    procedure FixVisible(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
  public
    function CreateUnknownCompound(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    function CreateUnknownText(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
    procedure AssignMetaDataToTreeViewItem(MetaDataItem: TCustomMetaDataItem);
    property SelectedItem: TCustomMetaDataItem read FSelectedItem write SetSelectedItem;
    procedure SaveXml(const FileName: string);
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(FileName: String);
    property Changed: Boolean read FChanged write FChanged;
    procedure DoSearch(SearchTerm: string; SearchItems: TSearchItems;
      SearchDirection: TSearchDirection; SearchRestriction: TSearchRestriction;
      CaseSensitive: Boolean);
  end;

var
  Frame: TframeMetaDataEditor;

implementation

{$R *.fmx}

uses
  System.Generics.Collections, System.IOUtils;

procedure TframeMetaDataEditor.InvalidateArchiveTreeView;
var
  ItemHeight: Single;
begin
  // This attempts to force a repaint of newly visible TTreeViewItems.
  // It doesn't seem to work
  tvMetaData.ScrollBy(0, -tvMetaData.Height);
  tvMetaData.ScrollBy(0, tvMetaData.Height);
  ItemHeight := tvMetaData.ItemHeight;
  tvMetaData.ItemHeight := ItemHeight * 10;
  tvMetaData.InvalidateContentSize;
  tvMetaData.ItemHeight := ItemHeight;
  tvMetaData.InvalidateContentSize;
end;


procedure TframeMetaDataEditor.btnExpandAllClick(Sender: TObject);
begin
  tvMetaData.ExpandAll;
end;

procedure TframeMetaDataEditor.btnCollapseAllClick(Sender: TObject);
begin
  tvMetaData.CollapseAll;
end;

procedure TframeMetaDataEditor.btnDuplicateClick(Sender: TObject);
var
  Item: TTreeViewItem;
  MetaDataItem: TCustomMetaDataItem;
begin
  Item := tvMetaData.Selected;
  if Item = nil  then
  begin
    Exit;
  end;
  MetaDataItem := Item.TagObject as TCustomMetaDataItem;
  if MetaDataItem.MoreThanOneAllowed then
  begin
    MetaDataItem.NewItem(Item.ParentItem.TagObject as TCustomMetaDataItem)
  end;
end;

procedure TframeMetaDataEditor.btnExpandUsedClick(Sender: TObject);
var
  ItemIndex: Integer;
  Item: TTreeViewItem;
  procedure  ExpandUsedItems(AnItem: TTreeViewItem);
  var
    ChildIndex: Integer;
    ChildItem: TTreeViewItem;
//    ItemHeight: Single;
//    FontSize: Single;
  begin
    if AnItem.IsChecked then
    begin
      AnItem.Expand;
      for ChildIndex := 0 to AnItem.Count - 1 do
      begin
        ChildItem := AnItem.Items[ChildIndex];
        ExpandUsedItems(ChildItem);
      end;
    end
    else
    begin
      AnItem.Collapse;
    end;
//    ItemHeight := AnItem.Height;
//    AnItem.Height := ItemHeight * 2;
//    AnItem.Height := ItemHeight;
//    FontSize := AnItem.Font.Size;
//    AnItem.Font.Size := FontSize * 2;
//    AnItem.Font.Size := FontSize;
//    AnItem.Repaint;
  end;
begin
  tvMetaData.BeginUpdate;
  try
    for ItemIndex := 0 to tvMetaData.Count - 1 do
    begin
      Item := tvMetaData.Items[ItemIndex];
      ExpandUsedItems(Item);
    end;
  finally
    tvMetaData.endUpdate;
  end;
  tvMetaData.Repaint;
//  tvMetaData.Selected := tvMetaData.Items[tvMetaData.Count-1];
  InvalidateArchiveTreeView;
end;

procedure TframeMetaDataEditor.SaveXml(const FileName: string);
var
//  MetaData: TXmlVerySimple;
  RootItem: TCustomMetaDataItem;
  Year: Word;
  Month: Word;
  Day: Word;
  StyleNode: TXmlNode;
  TypeAttribute: TXmlAttribute;
  HrefAttribute: TXmlAttribute;
  MetaDataNode: TXmlNode;
  SchemaAttribute: TXmlAttribute;
  SchemaLocationAttribute: TXmlAttribute;
  procedure AddItem(ParentNode: TXmlNode; Item: TCustomMetaDataItem; PriorSibling: TXmlNode);
  var
    NewNode: TXmlNode;
    AnItem: TCustomMetaDataItem;
    Index: Integer;
    UsedItemList: TList<TCustomMetaDataItem>;
    ItemNode: TXmlNode;
    CHildPosition: Integer;
    PriorSiblingIndex: Integer;
  begin
    if Item.Used then
    begin
      try
        if Assigned(Item.XmlNode) then
        begin
          NewNode := Item.XmlNode;
        end
        else
        begin
          if ParentNode = nil then
          begin
            if PriorSibling = nil then
            begin
              NewNode := FXml.AddChild(Item.ShortName);
            end
            else
            begin
              PriorSiblingIndex := FXml.ChildNodes.IndexOf(PriorSibling);
              NewNode := FXml.ChildNodes.Insert(Item.ShortName, Succ(PriorSiblingIndex));
            end;
          end
          else
          begin
            if PriorSibling = nil then
            begin
//              NewNode := ParentNode.AddChild(Item.ShortName);
              NewNode := ParentNode.InsertChild(Item.ShortName, 0);
            end
            else
            begin
              PriorSiblingIndex := ParentNode.ChildNodes.IndexOf(PriorSibling);
              NewNode := ParentNode.InsertChild(Item.ShortName, Succ(PriorSiblingIndex));
            end;
          end;
          Item.XmlNode := NewNode;
        end;
//        PriorSibling := NewNode;
//      NewNode.Name := Item.ShortName;
        NewNode.Text := Item.GetStringContent;
      except
        ShowMessage(Item.ID);
        raise;
      end;
      UsedItemList := TList<TCustomMetaDataItem>.Create;
      try
        PriorSibling := nil;
        for Index := 0 to Item.TreeViewItem.Count - 1 do
        begin
          AnItem := Item.TreeViewItem.Items[Index].TagObject as TCustomMetaDataItem;
          AddItem(NewNode, AnItem, PriorSibling);
          if AnItem.Used then
          begin
            UsedItemList.Add(AnItem);
            PriorSibling := AnItem.XmlNode;
          end;
        end;

        for Index := 0 to UsedItemList.Count - 1 do
        begin
          AnItem := UsedItemList[Index];
          ItemNode := AnItem.XmlNode;
          CHildPosition := NewNode.ChildNodes.IndexOf(ItemNode);
          Assert(CHildPosition >= 0);
          if CHildPosition <> Index then
          begin
            NewNode.ChildNodes.OwnsObjects := False;
            try
              NewNode.ChildNodes.Delete(CHildPosition);
              NewNode.ChildNodes.Insert(Index, ItemNode)
            finally
              NewNode.ChildNodes.OwnsObjects := True;
            end;
          end;
        end;
      finally
        UsedItemList.Free;
      end;
      Item.WriteChildren(NewNode);
    end
    else
    begin
      try
        Item.XmlNode := nil;
      except
        ShowMessage(Item.ID);
        raise;
      end;
    end;
  end;
begin
//  if dlgSaveMetadata.Execute then
  begin
    if FReviewItem <> nil then
    begin
      DecodeDate(Now, Year, Month, Day);
      if (FReviewItem.Year <> Year)
        or (FReviewItem.Month <> Month)
        or (FReviewItem.Day <> Day) then
      begin
        if MessageDlg('Do you want to update the metadata creation date to today?',
          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
        begin
          FReviewItem.Year := Year;
          FReviewItem.Month := Month;
          FReviewItem.Day := Day;
        end;
      end;
    end;

//    Caption := dlgSaveMetadata.FileName + ': FgdcMetaEditor';
    if FXml = nil then
    begin
      FXml := TXmlVerySimple.Create;
    end;

    FXml.NodeIndentStr := #9;

    StyleNode := FXml.ChildNodes.Find('xml-stylesheet', [ntProcessingInstr]);
    if StyleNode = nil then
    begin
      StyleNode := FXml.ChildNodes.Insert('xml-stylesheet', 1, ntProcessingInstr);
    end;
    TypeAttribute := StyleNode.AttributeList.Find('type');
    if TypeAttribute = nil then
    begin
      TypeAttribute := StyleNode.AttributeList.Add('type');
    end;
    TypeAttribute.Value := 'text/xsl';

    HrefAttribute := StyleNode.AttributeList.Find('href');
    if HrefAttribute = nil then
    begin
      HrefAttribute := StyleNode.AttributeList.Add('href');
    end;
    HrefAttribute.Value := 'fgdc_classic.xsl';


    RootItem := tvMetaData.Items[0].TagObject as TCustomMetaDataItem;

    AddItem(nil, RootItem, StyleNode);

    MetaDataNode := FXml.ChildNodes.Find('metadata');
    Assert(MetaDataNode <> nil);
    SchemaAttribute := MetaDataNode.AttributeList.Find('xmlns:xsi');
    if SchemaAttribute = nil then
    begin
      SchemaAttribute := MetaDataNode.AttributeList.Add('xmlns:xsi');
    end;
    SchemaAttribute.Value := 'http://www.w3.org/2001/XMLSchema-instance';

    SchemaLocationAttribute := MetaDataNode.AttributeList.Find(
      'xsi:noNamespaceSchemaLocation');
    if SchemaLocationAttribute = nil then
    begin
      SchemaLocationAttribute := MetaDataNode.AttributeList.Add('xsi:noNamespaceSchemaLocation');
    end;
    SchemaLocationAttribute.Value :=
      'https://water.usgs.gov/GIS/metadata/usgswrd/fgdc-std-001-1998.xsd';

    FXml.SaveToFile(FileName);
    FChanged := False;
  end;
end;

{procedure TframeMetaDataEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FChanged then
  begin
    case MessageDlg('Do you want to save the changes to your metadata file?',
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo,
        TMsgDlgBtn.mbCancel], 0) of
      mrYes:
        begin
          btnSaveXmlClick(nil);
          if FChanged then
          begin
            CanClose := False;
          end;
        end;
      mrNo:
        begin
          CanClose := True;
        end
     else
        begin
          CanClose := False;
        end;
    end;
  end;
end;               }

constructor TframeMetaDataEditor.Create(Owner: TComponent);
begin
  inherited;
  Frame := Self;
  tvMetaData.BeginUpdate;
  CreateMetaDataRoot(nil);
  tvMetaData.EndUpdate;
  FChanged := False;
  DrawTextOnImage(imgManditory, TAlphaColors.Yellow, 'Mandatory');
  DrawTextOnImage(imgMandIfAppl, TAlphaColors.Lime, 'Mandatory if Applicable');
  DrawTextOnImage(imgOptional, TAlphaColors.Cyan, 'Optional');
end;

destructor TframeMetaDataEditor.Destroy;
begin
  FXml.Free;
  Frame := nil;
  inherited;
end;

procedure TframeMetaDataEditor.DoSearch(SearchTerm: string;
  SearchItems: TSearchItems; SearchDirection: TSearchDirection;
  SearchRestriction: TSearchRestriction; CaseSensitive: Boolean);
var
  SelectedTreeViewItem: TTreeViewItem;
  ItemList: TList<TTreeViewItem>;
  ItemIndex: Integer;
  AnItem: TTreeViewItem;
  SelectedMetaDataItem: TCustomMetaDataItem;
  ParentItem: TTreeViewItem;
  procedure AddItemToList(AnItem: TTreeViewItem);
  var
    ChildIndex: Integer;
  begin
    ItemList.Add(AnItem);
    for ChildIndex := 0 to AnItem.Count - 1 do
    begin
      AddItemToList(AnItem.Items[ChildIndex]);
    end;
  end;
  function IsAMatch(MetaDataItem: TCustomMetaDataItem): Boolean;
  var
    SearchItem: TSearchItem;
    SearchedText: string;
  begin
    result := False;
    case SearchRestriction of
      srAll: ; // do nothing
      srUsed:
        begin
          if not MetaDataItem.FullUsed then
          begin
            Exit;
          end;
        end;
      srUnused:
        begin
          if MetaDataItem.FullUsed then
          begin
            Exit;
          end;
        end;
    end;
    if not CaseSensitive then
    begin
      SearchTerm := UpperCase(SearchTerm)
    end;
    for SearchItem in SearchItems do
    begin
      case SearchItem of
        siValue:
          begin
            SearchedText := MetaDataItem.GetStringContent;
          end;
        siDescription:
          begin
            SearchedText := MetaDataItem.Description;
          end;
        siName:
          begin
            SearchedText := MetaDataItem.LongName;
          end;
        siAbbreviation:
          begin
            SearchedText := MetaDataItem.ShortName;
          end;
      end;
      if not CaseSensitive then
      begin
        SearchedText := UpperCase(SearchedText)
      end;
      Result := Pos(SearchTerm, SearchedText) > 0;
      if result then
      begin
        Break;
      end;
    end;
  end;
begin
  if SearchItems = [] then
  begin
    Exit;
  end;
  ItemList := TList<TTreeViewItem>.Create;
  try
    for ItemIndex := 0 to tvMetaData.Count - 1 do
    begin
      AnItem := tvMetaData.Items[ItemIndex];
      AddItemToList(AnItem);
    end;

    if tvMetaData.Selected <> nil then
    begin
      SelectedTreeViewItem := tvMetaData.Selected;
      ItemIndex := ItemList.IndexOf(SelectedTreeViewItem);
      case SearchDirection of
        sdForward:
          begin
            Inc(ItemIndex);
            if ItemIndex < ItemList.Count then
            begin
              SelectedTreeViewItem := ItemList[ItemIndex];
            end
            else
            begin
              SelectedTreeViewItem := nil;
            end;
          end;
        sdBackward:
          begin
            Dec(ItemIndex);
            if ItemIndex > 0 then
            begin
              SelectedTreeViewItem := ItemList[ItemIndex];
            end
            else
            begin
              SelectedTreeViewItem := nil;
            end;
          end;
      end;
    end
    else
    begin
      if SearchDirection = sdForward then
      begin
        ItemIndex := 0;
      end
      else
      begin
        ItemIndex := ItemList.Count-1;
      end;
      SelectedTreeViewItem := tvMetaData.Items[ItemIndex];
    end;
    if SelectedTreeViewItem <> nil then
    begin
      repeat
        SelectedMetaDataItem := SelectedTreeViewItem.
          TagObject as TCustomMetaDataItem;
        if IsAMatch(SelectedMetaDataItem) then
        begin
          ParentItem := SelectedTreeViewItem.ParentItem;
          while ParentItem <> nil do
          begin
            ParentItem.IsExpanded := True;
            ParentItem := ParentItem.ParentItem;
          end;
          tvMetaData.Selected := SelectedTreeViewItem;
          break;
        end;
        case SearchDirection of
          sdForward: Inc(ItemIndex);
          sdBackward: Dec(ItemIndex);
        end;
        if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
        begin
          SelectedTreeViewItem := ItemList[ItemIndex];
        end
        else
        begin
          SelectedTreeViewItem := nil;
        end;
      until SelectedTreeViewItem = nil;
      if SelectedTreeViewItem = nil then
      begin
        MessageDlg('No match found', TMsgDlgType.mtInformation,
          [TMsgDlgBtn.mbOK], 0)
      end;
    end;

  finally
    ItemList.Free;
  end;

end;

procedure TframeMetaDataEditor.DrawTextOnImage(AnImage: TImage; AColor: TAlphaColor; TextToShow: string);
var
  ARectF: TRectF;
begin
  AnImage.Bitmap.SetSize(Round(AnImage.Width), Round(AnImage.Height));
  AnImage.Bitmap.Clear(AColor);
  AnImage.Bitmap.Canvas.BeginScene;
  AnImage.Bitmap.Canvas.Fill.Color := TAlphaColors.Black;
  ARectF := TRectF.Create(0, 0, AnImage.Bitmap.Width, AnImage.Bitmap.Height);
  AnImage.Bitmap.Canvas.FillText(ARectF, TextToShow, False, 1, [], TTextAlign.Center);
  AnImage.Bitmap.Canvas.EndScene;
end;

procedure TframeMetaDataEditor.FixVisible(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  AnItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
  StoredChanged: Boolean;
begin
  AnItem := Sender as TTreeViewItem;
  MetaData := AnItem.TagObject as TCustomMetaDataItem;
  StoredChanged := FChanged;
  try
    UpdateChecks(MetaData);
  finally
    FChanged := StoredChanged;
  end;
end;

procedure TframeMetaDataEditor.UpdateChecks(AnItem: TCustomMetaDataItem);
var
  ItemUsed: Boolean;
  ChildItem: TCustomMetaDataItem;
  ChildIndex: Integer;
begin
  ItemUsed := AnItem.Used;
  case AnItem.CheckType of
    ctCheckBox:
      begin
        if ((AnItem.ShortName <> 'attr') and (AnItem.ShortName <> 'lworkcit'))
          or ItemUsed then
        begin
          AnItem.Used := not ItemUsed;
          AnItem.Used := ItemUsed;
        end;
      end;
    ctRadioButton:
      begin
        if ItemUsed then
        begin
          AnItem.Used := not ItemUsed;
          AnItem.Used := ItemUsed;
        end;
      end;
    else Assert(False);
  end;
  for ChildIndex := 0 to AnItem.TreeViewItem.Count - 1 do
  begin
    ChildItem :=AnItem.TreeViewItem.Items[ChildIndex].TagObject as TCustomMetaDataItem;
    UpdateChecks(ChildItem);
  end;
end;


procedure TframeMetaDataEditor.OpenFile(FileName: String);
var
  ChildIndex: Integer;
  ANode: TXmlNode;
  CS: IFMXCursorService;
  ACursor: Integer;
begin
//  if dlgOpenMetaData.Execute then
//  begin
//    dlgSaveMetadata.FileName := dlgOpenMetaData.FileName;
    if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    begin
      CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
    end;
    ACursor := 0;
    if Assigned(CS) then
    begin
      ACursor := CS.GetCursor;
      CS.SetCursor(crHourGlass);
    end;
    try
      SelectedItem := nil;
      FReviewItem := nil;
      FRootItem.TreeViewItem.Free;
      tvMetaData.BeginUpdate;
      try
        CreateMetaDataRoot(nil);
      finally
        tvMetaData.EndUpdate;
      end;

      FXml.Free;
      FXml := TXmlVerySimple.Create;
      FXml.LoadFromFile(FileName);
      tvMetaData.BeginUpdate;
      try
  //      Caption := dlgOpenMetaData.FileName + ': FgdcMetaEditor';
        for ChildIndex := 0 to FXml.ChildNodes.Count - 1 do
        begin
          ANode := FXml.ChildNodes[ChildIndex];
          if LowerCase(ANode.Name) = 'metadata' then
          begin
            FRootItem.AssignValuesFromXML(ANode);
            break;
          end;
        end;


        Self.btnExpandUsedClick(nil);

        UpdateChecks(FRootItem);

        tvMetaData.Selected := nil;
      finally
        tvMetaData.EndUpdate;
      end;
//      InvalidateArchiveTreeView;
    finally
      if Assigned(CS) then
      begin
        CS.SetCursor(ACursor);
      end;
      FChanged := False;
    end;
//  end;
end;

{procedure TframeMetaDataEditor.miValidationServiceClick(Sender: TObject);
var
  CS: IFMXCursorService;
  ACursor: Integer;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    ACursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;
  try
    OpenURL('https://mrdata.usgs.gov/validation/');
  finally
    if Assigned(CS) then
    begin
      CS.SetCursor(ACursor);
    end;
  end;
end;}

procedure TframeMetaDataEditor.TextItemPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  TreeViewItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
begin
  FixVisible(Sender, Canvas, ARect);
  TreeViewItem := Sender as TTreeViewItem;
  MetaData := TreeViewItem.TagObject as TCustomMetaDataItem;
  if MetaData.FullUsed and (MetaData.GetStringContent = '') then
  begin
    Canvas.Fill.Color := TAlphaColorRec.Blue;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end
end;

procedure TframeMetaDataEditor.ExtentItemPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  TreeViewItem: TTreeViewItem;
  MetaData: TExtentDataItem;
begin
  FixVisible(Sender, Canvas, ARect);
  TreeViewItem := Sender as TTreeViewItem;
  MetaData := TreeViewItem.TagObject as TExtentDataItem;
  if MetaData.FullUsed then
  begin
    if (MetaData.West > MetaData.East)
      or (MetaData.North < MetaData.South)
      or (MetaData.West = 180) then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end;
  end;
end;

procedure TframeMetaDataEditor.FrameDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  if (Length(Data.Files) > 0) and TFile.Exists(Data.Files[0]) then
  begin
    OpenFile(Data.Files[0]);
  end;
end;

procedure TframeMetaDataEditor.FrameDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if (Length(Data.Files) > 0) and TFile.Exists(Data.Files[0]) then
  begin
    Operation := TDragOperation.Link;
//    OpenFile(Data.Files[0]);
  end;
end;

procedure TframeMetaDataEditor.PolygonItemPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  TreeViewItem: TTreeViewItem;
  MetaData: TPolygonMetaDataItem;
  PFirst: TPolyPoint;
  PLast: TPolyPoint;
  APoint: TPolyPoint;
  PointIndex: Integer;
begin
  FixVisible(Sender, Canvas, ARect);
  TreeViewItem := Sender as TTreeViewItem;
  MetaData := TreeViewItem.TagObject as TPolygonMetaDataItem;
  if MetaData.FullUsed then
  begin
    if MetaData.Polygon.Count < 4 then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Blue;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end
    else
    begin
      PFirst := MetaData.Polygon[0];
      PLast := MetaData.Polygon[MetaData.Polygon.Count-1];
      if (PFirst.Longitude <> PLast.Longitude)
        or (PFirst.Latitude <> PLast.Latitude) then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
      end
      else
      begin
        for PointIndex := 0 to MetaData.Polygon.Count - 1 do
        begin
          APoint := MetaData.Polygon[PointIndex];
          if APoint.Longitude >= 180 then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Red;
            Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
            Break;
          end;
        end;
      end;
    end;
  end
end;

procedure TframeMetaDataEditor.NumberItemPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  TreeViewItem: TTreeViewItem;
  NumData: TNumericMetaDataItem;
begin
  FixVisible(Sender, Canvas, ARect);

  TreeViewItem := Sender as TTreeViewItem;
  NumData := TreeViewItem.TagObject as TNumericMetaDataItem;
  if NumData.FullUsed then
  begin
    if (NumData.MaxLimitType = mltLess) and (NumData.Content = NumData.MaxValue) then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end
    else if (NumData.MinLimitType = mltGreater) and (NumData.Content = NumData.MinValue) then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end;
  end;
end;

procedure TframeMetaDataEditor.RadioButtonChanged(Sender: TObject);
var
  MetaData: TCustomMetaDataItem;
  ParentItem: TTreeViewItem;
  ChildIndex: Integer;
  ChildItem: TTreeViewItem;
begin
  MetaData := Sender as TCustomMetaDataItem;
  ParentItem := MetaData.TreeViewItem.ParentItem;
  for ChildIndex := 0 to ParentItem.Count - 1 do
  begin
    ChildItem := ParentItem.Items[ChildIndex];
    ChildItem.Repaint;
  end;
end;

procedure TframeMetaDataEditor.SetSelectedItem(const Value: TCustomMetaDataItem);
begin
  if FSelectedItem <> Value then
  begin
    if FSelectedItem <> nil then
    begin
      FSelectedItem.HideFrame;
    end;
    FSelectedItem := Value;
    if FSelectedItem <> nil then
    begin
      FSelectedItem.ShowFrame;
    end;
  end;
end;

procedure TframeMetaDataEditor.OptionalRadioButtonDoubleClicked(Sender: TObject);
begin
  if (Sender is TRadioButton) and TRadioButton(Sender).IsChecked then
  begin
    FRadioButton := TRadioButton(Sender);
    tmr1.Enabled := True;
  end;
end;

procedure TframeMetaDataEditor.CheckRadioButton(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  AnItem: TTreeViewItem;
  ParentItem: TTreeViewItem;
  ChildIndex: Integer;
  ChildItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
  ItemCount: Integer;
  Used: Boolean;
  ID: string;
begin
  FixVisible(Sender, Canvas, ARect);
  AnItem := Sender as TTreeViewItem;
  MetaData := AnItem.TagObject as TCustomMetaDataItem;
  ID := MetaData.FullRadioGroupName;
  Used := MetaData.Used;

  ParentItem := AnItem.ParentItem;

  if (ParentItem.TagObject as TCustomMetaDataItem).FullUsed then
  begin
    ItemCount := 0;
    for ChildIndex := 0 to ParentItem.Count - 1 do
    begin
      ChildItem := ParentItem.Items[ChildIndex];
      MetaData := ChildItem.TagObject as TCustomMetaDataItem;
      if (MetaData.CheckType = ctRadioButton) and (ID = MetaData.FullRadioGroupName) then
      begin

        if MetaData.Used then
        begin
            Inc(ItemCount);
            if ItemCount >= 2 then
            begin
              Break;
            end;
        end;
      end;
    end;
    if (not Used) and (ItemCount = 0) then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Yellow;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end
    else if Used and (ItemCount > 1) then
    begin
      Canvas.Fill.Color := TAlphaColorRec.Red;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
    end;
  end;
end;

procedure TframeMetaDataEditor.ContentChanged(Sender: TObject);
begin
  FChanged := True;
end;

function TframeMetaDataEditor.CreateAbscissaResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Abscissa Resolution -- the (nominal) minimum distance between the "x" or column values of two adjacent points, expressed in Planar Distance Units of measure.';
  MetaDataItem.ShortName := 'absres';
  MetaDataItem.LongName := 'Abscissa Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.2.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateAbscissaResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAbstract(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Abstract -- a brief narrative summary of the data set.';
  MetaDataItem.ShortName := 'abstract';
  MetaDataItem.LongName := 'Abstract';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.2.1';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateAbstract;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAccessConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Access Constraints -- restrictions and legal '
    + 'prerequisites for accessing the data set. These include any access '
    + 'constraints applied to assure the protection of privacy or intellectual '
    + 'property, and any special restrictions or limitations on obtaining the '
    + 'data set.'
    + sLineBreak
    + '(USGS Best Practice) Example: “No access constraints as all data is '
    + 'open and non-proprietary”';
  MetaDataItem.ShortName := 'accconst';
  MetaDataItem.LongName := 'Access Constraints';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.7';
  MetaDataItem.TextHeight := 220;

  MetaDataItem.CreateMethod := CreateAccessConstraints;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAccessInst(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Access Instructions -- instructions on the steps required to access the data set.';
  MetaDataItem.ShortName := 'accinstr';
  MetaDataItem.LongName := 'Access Instructions';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.2.2.1.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateAccessInst;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAddressLine(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Address -- an address line for the address.'
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "445 National Center".';
  MetaDataItem.ShortName := 'address';
  MetaDataItem.LongName := 'Address';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '10.4.2';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateAddressLine;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAddressType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Address Type -- the information provided by the address.';
  MetaDataItem.ShortName := 'addrtype';
  MetaDataItem.LongName := 'Address Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.4.1';
  MetaDataItem.Choices.Add('mailing');
  MetaDataItem.Choices.Add('physical');
  MetaDataItem.Choices.Add('mailing and physical');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateAddressType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAlbersConicalEqualArea(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Albers Conical Equal Area -- contains parameters for the Albers Conical Equal Area projection.';
  MetaDataItem.ShortName := 'albers';
  MetaDataItem.LongName := 'Albers Conical Equal Area';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.2';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CreateMethod := CreateAlbersConicalEqualArea;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateStandardParallel(MetaDataItem);
  MChild := CreateStandardParallel(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);


  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateAltitudeDatumName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Altitude Datum Name -- the identification given to the surface taken as the surface of reference from which altitudes are measured.';
  MetaDataItem.ShortName := 'altdatum';
  MetaDataItem.LongName := 'Altitude Datum Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.1.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Choices.Add('National Geodetic Vertical Datum of 1929');
  MetaDataItem.Choices.Add('North American Vertical Datum of 1988');

  MetaDataItem.CreateMethod := CreateAltitudeDatumName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAltitudeDistanceUnit(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Altitude Distance Units -- units in which altitudes are recorded.';
  MetaDataItem.ShortName := 'altunits';
  MetaDataItem.LongName := 'Altitude Distance Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.1.3';

  MetaDataItem.Choices.Add('meters');
  MetaDataItem.Choices.Add('feet');

  MetaDataItem.CreateMethod := CreateAltitudeDistanceUnit;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAltitudeEncodingMethod(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Altitude Encoding Method -- the means used to encode the altitudes.';
  MetaDataItem.ShortName := 'altenc';
  MetaDataItem.LongName := 'Altitude Encoding Method';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.1.4';

  MetaDataItem.Choices.Add('Explicit elevation coordinate included with horizontal coordinates');
  MetaDataItem.Choices.Add('Implicit coordinate');
  MetaDataItem.Choices.Add('Attribute value');

  MetaDataItem.CreateMethod := CreateAltitudeEncodingMethod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAltitudeResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Altitude Resolution -- the minimum distance possible between two adjacent altitude values, expressed in Altitude Distance Units of measure.';
  MetaDataItem.ShortName := 'altres';
  MetaDataItem.LongName := 'Altitude Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.1.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateAltitudeResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAltitudeSysDef(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Altitude System Definition -- the reference frame or system from which altitudes (elevations) are measured. The'
    + ' term "altitude" is used instead of the common term "elevation" to conform to the terminology in Federal'
    + ' Information Processing Standards 70-1 and 173.';
  MetaDataItem.ShortName := 'altsys';
  MetaDataItem.LongName := 'Altitude System Definition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.2.1';
  MetaDataItem.TextHeight := 150;

  MetaDataItem.CreateMethod := CreateAltitudeSysDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAltitudeDatumName(MetaDataItem);
  CreateAltitudeResolution(MetaDataItem);
  CreateAltitudeDistanceUnit(MetaDataItem);
  CreateAltitudeEncodingMethod(MetaDataItem);
end;

function TframeMetaDataEditor.CreateArcCoordSys(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'ARC Coordinate System -- the Equal Arc-second Coordinate System, a plane-rectangular coordinate system'
    + ' established in Department of Defense, 1990, Military specification ARC Digitized Raster Graphics (ADRG)'
    + ' (MIL-A-89007): Philadelphia, Department of Defense, Defense Printing Service Detachment Office.';
  MetaDataItem.ShortName := 'arcsys';
  MetaDataItem.LongName := 'ARC Coordinate System';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.5';
  MetaDataItem.TextHeight := 175;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateArcCoordSys;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateArcSysZoneID(MetaDataItem);

  MChild := CreateEquirectangular(MetaDataItem);
  MChild.RadioGroupName := 'arcsys';
  AssignMetaDataToTreeViewItem(MChild);

  MChild := CreateAzimuthEqualArea(MetaDataItem);
  MChild.RadioGroupName := 'arcsys';
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateArcSysZoneID(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'ARC System Zone Identifier -- identifier for the ARC Coordinate System Zone.';
  MetaDataItem.ShortName := 'arczone';
  MetaDataItem.LongName := 'ARC System Zone Identifier';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.5.1';

  MetaDataItem.Content := 1;
  MetaDataItem.MinValue := 1;
  MetaDataItem.MaxValue := 18;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateArcSysZoneID;
  AssignMetaDataToTreeViewItem(MetaDataItem);

//  CreateStatePlaneCoordSysZoneID(MetaDataItem);
//
//  MChild := CreateLambertConformalConic(MetaDataItem);
//  MChild.RadioGroupName := 'spcs';
//  AssignMetaDataToTreeViewItem(MChild);
//
//  MChild := CreateTransverseMercator(MetaDataItem);
//  MChild.RadioGroupName := 'spcs';
//  AssignMetaDataToTreeViewItem(MChild);
//
//  MChild := CreateObliqueMercator(MetaDataItem);
//  MChild.RadioGroupName := 'spcs';
//  AssignMetaDataToTreeViewItem(MChild);
//
//  MChild := CreatePolyconic(MetaDataItem);
//  MChild.RadioGroupName := 'spcs';
//  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttBeginDate(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Beginning Date of Attribute Values -- earliest or only date for which the attribute'
    + ' values are current. In cases when a range of dates are provided, this is the earliest date'
    + ' for which the information is valid.';
  MetaDataItem.ShortName := 'begdatea';
  MetaDataItem.LongName := 'Beginning Date of Attribute Values';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '5.1.2.5';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateAttBeginDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttEndDate(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ending Date of Attribute Values -- latest date for which the information is current. Used in cases when a range of dates are provided.';
  MetaDataItem.ShortName := 'enddatea';
  MetaDataItem.LongName := 'Ending Date of Attribute Values';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '5.1.2.6';

//  MetaDataItem.Min := 0;

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateAttEndDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttMeasurementFrequency(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Measurement Frequency -- the frequency with which attribute values are added.';
  MetaDataItem.ShortName := 'attrmfrq';
  MetaDataItem.LongName := 'Attribute Measurement Frequency';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '5.1.2.8';

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choices.Add('As needed');
  MetaDataItem.Choices.Add('Irregular');
  MetaDataItem.Choices.Add('None planned');

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateAttMeasurementFrequency;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

procedure TframeMetaDataEditor.OnAttributeCheck(Sender: TObject);
var
  CheckBox: TCheckBox;
  TreeViewItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
begin
  CheckBox := Sender as TCheckBox;
  TreeViewItem := CheckBox.Parent as TTreeViewItem;
  MetaData := TreeViewItem.TagObject as TCustomMetaDataItem;
  MetaData.CheckBoxChecked(Sender);
  if CheckBox.IsChecked then
  begin
    if TreeViewItem.Count = 0 then
    begin
      AddAttributeChildren(MetaData);
    end;
  end;
end;

procedure TframeMetaDataEditor.OnAttributeItemSelect(Sender: TObject);
var
  MetaData: TCustomMetaDataItem;
begin
  MetaData := Sender as TCustomMetaDataItem;
  if MetaData.Used and (MetaData.TreeViewItem.Count = 0) then
  begin
    AddAttributeChildren(MetaData);
  end;
end;


function TframeMetaDataEditor.CreateAttribute(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute -- a defined characteristic of an entity.';
  MetaDataItem.ShortName := 'attr';
  MetaDataItem.LongName := 'Attribute';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '5.1.2';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateAttribute;
  AssignMetaDataToTreeViewItem(MetaDataItem);
  MetaDataItem.CheckBox.OnChange := OnAttributeCheck;
  MetaDataItem.OnUsedChanged := OnAttributeItemSelect;

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateAttributeAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Accuracy -- an assessment of the accuracy of the identification of entities and assignment of attribute values in the data set.';
  MetaDataItem.ShortName := 'attracc';
  MetaDataItem.LongName := 'Attribute Accuracy';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateAttributeAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAttributeAccuracyReport(MetaDataItem);
  CreateQuantitativeAttributeAccuracyAssessment(MetaDataItem);
end;

function TframeMetaDataEditor.CreateAttributeAccuracyExplanation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Accuracy Explanation -- the identification of the test that yielded the Attribute Accuracy Value.';
  MetaDataItem.ShortName := 'attracce';
  MetaDataItem.LongName := 'Attribute Accuracy Explanation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.1.2.2';

  MetaDataItem.CreateMethod := CreateAttributeAccuracyExplanation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateAttributeAccuracyReport(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Accuracy Report -- an explanation of the accuracy of the identification of the entities and assignments of values in the data set and a description of the tests used.';
  MetaDataItem.ShortName := 'attraccr';
  MetaDataItem.LongName := 'Attribute Accuracy Report';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.1.1';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateAttributeAccuracyReport;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateAttributeAccuracyValue(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Accuracy Value -- an estimate of the accuracy of the identification of the entities and assignments of attribute values in the data set.';
  MetaDataItem.ShortName := 'attraccv';
  MetaDataItem.LongName := 'Attribute Accuracy Value';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.1.2.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choice := MetaDataItem.Choices[0];

  MetaDataItem.CreateMethod := CreateAttributeAccuracyValue;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateAttributeDefinition(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Definition -- the description of the attribute.';
  MetaDataItem.ShortName := 'attrdef';
  MetaDataItem.LongName := 'Attribute Definition';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.2';

  MetaDataItem.CreateMethod := CreateAttributeDefinition;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttributeDefinitionSource(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Definition Source -- the authority of the definition.';
  MetaDataItem.ShortName := 'attrdefs';
  MetaDataItem.LongName := 'Attribute Definition Source';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.3';

  MetaDataItem.CreateMethod := CreateAttributeDefinitionSource;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttributeDomainValue(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Domain Values -- the valid values that can be assigned for an attribute.';
  MetaDataItem.ShortName := 'attrdomv';
  MetaDataItem.LongName := 'Attribute Domain Values';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateAttributeDomainValue;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateEnumeratedDomain(MetaDataItem);
  CreateRangeDomain(MetaDataItem);
  CreateCodesetDomain(MetaDataItem);
  CreateUnrepresentableDomain(MetaDataItem);
end;

function TframeMetaDataEditor.CreateAttributeLabel(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Label -- the name of the attribute.';
  MetaDataItem.ShortName := 'attrlabl';
  MetaDataItem.LongName := 'Attribute Label';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.1';

  MetaDataItem.CreateMethod := CreateAttributeLabel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttributeMeasureRes(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Measurement Resolution -- the smallest unit increment to which an attribute value is measured.';
  MetaDataItem.ShortName := 'attrmres';
  MetaDataItem.LongName := 'Attribute Measurement Resolution';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '5.1.2.4.2.4';

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateAttributeMeasureRes;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttributeUnitsOfMeasure(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Units of Measure -- the standard of measurement for an attribute value.';
  MetaDataItem.ShortName := 'attrunit';
  MetaDataItem.LongName := 'Attribute Units of Measure';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '5.1.2.4.2.3';

  MetaDataItem.MoreThanOneAllowed := False;

  MetaDataItem.CreateMethod := CreateAttributeUnitsOfMeasure;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttValueAccuracy(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Value Accuracy -- an estimate of the accuracy of the assignment of attribute values.';
  MetaDataItem.ShortName := 'attrva';
  MetaDataItem.LongName := 'Attribute Value Accuracy';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.7.1';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateAttValueAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttValueAccuracyExplanation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Value Accuracy Explanation -- the definition of the Attribute Value Accuracy measure and units, and a description of how the estimate was derived.';
  MetaDataItem.ShortName := 'attrvae';
  MetaDataItem.LongName := 'Attribute Value Accuracy Explanation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.7.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateAttValueAccuracyExplanation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAttValueAccuracyInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Attribute Value Accuracy Information -- an assessment of the accuracy of the assignment of attribute values.';
  MetaDataItem.ShortName := 'attrvai';
  MetaDataItem.LongName := 'Attribute Value Accuracy Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '5.1.2.7';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateAttValueAccuracyInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAttValueAccuracy(MetaDataItem);
  CreateAttValueAccuracyExplanation(MetaDataItem);
end;

function TframeMetaDataEditor.CreateAvailableTimePeriod(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Available Time Period -- the time period when the data set will be available from the distributor.';
  MetaDataItem.ShortName := 'availabl';
  MetaDataItem.LongName := 'Available Time Period';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.7';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateAvailableTimePeriod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  ChildItem := CreateTimePeriodInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
end;

function TframeMetaDataEditor.CreateAzimuthAngle(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Azimuthal Angle -- angle measured clockwise from north, and expressed in degrees.';
  MetaDataItem.ShortName := 'azimangl';
  MetaDataItem.LongName := 'Azimuthal Angle';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.11.1';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MaxValue := 360;
  MetaDataItem.MaxLimitType := mltLess;

  MetaDataItem.CreateMethod := CreateAzimuthAngle;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateAzimuthEqualArea(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Azimuthal Equidistant -- contains parameters for the Azimuthal Equidistant projection.';
  MetaDataItem.ShortName := 'azimequi';
  MetaDataItem.LongName := 'Azimuthal Equidistant';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.3';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CreateMethod := CreateAzimuthEqualArea;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateAzimuthMeasPtLong(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Azimuth Measure Point Longitude -- longitude of the map projection origin.';
  MetaDataItem.ShortName := 'azimptl';
  MetaDataItem.LongName := 'Azimuth Measure Point Longitude';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.11.2';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := -180;
  MetaDataItem.MaxValue := 180;
  MetaDataItem.MaxLimitType := mltLess;

  MetaDataItem.CreateMethod := CreateAzimuthMeasPtLong;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateBearingRefDir(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Bearing Reference Direction -- direction from which the bearing is measured.';
  MetaDataItem.ShortName := 'bearrefd';
  MetaDataItem.LongName := 'Bearing Reference Direction';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3.4';

  MetaDataItem.Choices.Add('North');
  MetaDataItem.Choices.Add('South');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateBearingRefDir;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateBearingRefMeridian(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Bearing Reference Meridian -- axis from which the bearing is measured.';
  MetaDataItem.ShortName := 'bearrefm';
  MetaDataItem.LongName := 'Bearing Reference Meridian';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3.5';

  MetaDataItem.Choices.Add('Assumed');
  MetaDataItem.Choices.Add('Grid');
  MetaDataItem.Choices.Add('Magnetic');
  MetaDataItem.Choices.Add('Astronomic');
  MetaDataItem.Choices.Add('Geodetic');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateBearingRefMeridian;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateBearingResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Bearing Resolution -- the minimum angle measurable between two points, expressed in Bearing Units of measure.';
  MetaDataItem.ShortName := 'bearres';
  MetaDataItem.LongName := 'Bearing Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3.2';

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateBearingResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateBearingUnits(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Bearing Units -- units of measure used for angles.';
  MetaDataItem.ShortName := 'bearunit';
  MetaDataItem.LongName := 'Bearing Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3.3';

  MetaDataItem.Choices.Add('Decimal degrees');
  MetaDataItem.Choices.Add('Decimal minutes');
  MetaDataItem.Choices.Add('Decimal seconds');
  MetaDataItem.Choices.Add('Degrees and decimal minutes');
  MetaDataItem.Choices.Add('Degrees, minutes, and decimal seconds');
  MetaDataItem.Choices.Add('Radians');
  MetaDataItem.Choices.Add('Grads');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateBearingUnits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateBeginDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Beginning Date -- the first year (and optionally month, or month and day) of the event.';
  MetaDataItem.ShortName := 'begdate';
  MetaDataItem.LongName := 'Beginning Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.3.1';

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateBeginDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateBeginningTime(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusTimeDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusTimeDataItem.Create(AnItem);
  MetaDataItem.Description := 'Beginning Time -- the first hour (and optionally minute, or minute and second) of the day for the event.';
  MetaDataItem.ShortName := 'begtime';
  MetaDataItem.LongName := 'Beginning Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '9.3.2';

  MetaDataItem.Choices.Add('Unknown');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateBeginningTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateBoundingCoordinates(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TExtentDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TExtentDataItem.Create(AnItem);
  MetaDataItem.Description := 'Bounding Coordinates - the limits of coverage '
    + 'of a data set expressed by latitude and longitude values in the order '
    + 'western-most, eastern-most, northern-most, and southern-most. For data '
    + 'sets that include a complete band of latitude around the earth, the '
    + 'West Bounding Coordinate shall be assigned the value -180.0, and the '
    + 'East Bounding Coordinate shall be assigned the value 180.0';
  MetaDataItem.ShortName := 'bounding';
  MetaDataItem.LongName := 'Bounding Coordinates';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.5.1';
  MetaDataItem.TextHeight := 220;

  MetaDataItem.CreateMethod := CreateBoundingCoordinates;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateBrowseGraphic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Browse Graphic -- a graphic that provides an illustration of the data set. The graphic should include a legend for interpreting the graphic.'
    + sLineBreak
    + 'The WRD Node keeps all browse graphics in one directory. Please prefix browse graphics files with the name of your dataset, as ofr02-338_bedrock_map1.gif. Browse graphics can be referenced as the example below:'
    + sLineBreak
    + sLineBreak
    + '<browse>'
    + sLineBreak
    + '  <browsen>https://water.usgs.gov/GIS/browse/ofr02-338_bedrock_map1.gif</browsen>'
    + sLineBreak
    + '  <browsed>Illustration of the dataset.</browsed>'
    + sLineBreak
    + '  <browset>GIF</browset>'
    + sLineBreak
    + '</browse>';

  MetaDataItem.ShortName := 'browse';
  MetaDataItem.LongName := 'Browse Graphic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.10';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 400;

  MetaDataItem.CreateMethod := CreateBrowseGraphic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateBrowseGraphicFileName(MetaDataItem);
  CreateBrowseGraphicFileDescription(MetaDataItem);
  CreateBrowseGraphicFileType(MetaDataItem);

end;

function TframeMetaDataEditor.CreateBrowseGraphicFileDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Browse Graphic File Description -- a text description of the illustration.';
  MetaDataItem.ShortName := 'browsed';
  MetaDataItem.LongName := 'Browse Graphic File Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.10.2';

  MetaDataItem.CreateMethod := CreateBrowseGraphicFileDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateBrowseGraphicFileName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Browse Graphic File Name -- name of a related graphic file that provides an illustration of '
    + 'the data set.'
    + sLineBreak
    + 'For USGS data releases, the file name can be preceeded by '
    + '"https://water.usgs.gov/GIS/browse/".';
  MetaDataItem.ShortName := 'browsen';
  MetaDataItem.LongName := 'Browse Graphic File Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.10.1';
  MetaDataItem.TextHeight := 130;

  MetaDataItem.CreateMethod := CreateBrowseGraphicFileName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateBrowseGraphicFileType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Browse Graphic File Type -- graphic file type of a related graphic file.';
  MetaDataItem.ShortName := 'browset';
  MetaDataItem.LongName := 'Browse Graphic File Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.10.3';
  MetaDataItem.Choices.Add('CGM');
  MetaDataItem.Choices.Add('EPS');
  MetaDataItem.Choices.Add('EMF');
  MetaDataItem.Choices.Add('GIF');
  MetaDataItem.Choices.Add('JPEG');
  MetaDataItem.Choices.Add('PBM');
  MetaDataItem.Choices.Add('PS');
  MetaDataItem.Choices.Add('TIFF');
  MetaDataItem.Choices.Add('WMF');
  MetaDataItem.Choices.Add('XWD');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateBrowseGraphicFileType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCalendarDateTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Calendar Date -- the year (and optionally month, or month and day).';
  MetaDataItem.ShortName := 'caldate';
  MetaDataItem.LongName := 'Calendar Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.1.1';

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateCalendarDateTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCitation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Citation -- information to be used to reference the data set. ';
  MetaDataItem.ShortName := 'citation';
  MetaDataItem.LongName := 'Citation';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.1';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateCitation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  ChildItem := CreateCiteInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
end;

function TframeMetaDataEditor.CreateCiteInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Citation Information -- the recommended reference to be used for the data set. ';
  MetaDataItem.ShortName := 'citeinfo';
  MetaDataItem.LongName := 'Citation Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8';

  MetaDataItem.CreateMethod := CreateCiteInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateOriginator(MetaDataItem);
  CreatePublicationDate(MetaDataItem);
  CreatePublicationTime(MetaDataItem);
  CreateTitle(MetaDataItem);
  CreateEdition(MetaDataItem);
  CreateGeospatialForm(MetaDataItem);
  CreateSeriesInformation(MetaDataItem);
  CreatePubInfo(MetaDataItem);
  CreateOtherCiteInfo(MetaDataItem);
  CreateOnlineCiteLinkage(MetaDataItem);
  CreateLargerWorkCite(MetaDataItem);
end;

function TframeMetaDataEditor.CreateCloudCover(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Cloud Cover -- area of a data set obstructed by clouds, expressed as a percentage of the spatial extent.';
  MetaDataItem.ShortName := 'cloud';
  MetaDataItem.LongName := 'Cloud Cover';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.6';
  MetaDataItem.MinValue := 0;
  MetaDataItem.MaxValue := 100;
  MetaDataItem.Content := 0;

  MetaDataItem.CreateMethod := CreateCloudCover;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCodesetDomain(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Codeset Domain -- reference to a standard or list which contains the members of an established set of valid values.';
  MetaDataItem.ShortName := 'codesetd';
  MetaDataItem.LongName := 'Codeset Domain';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.3';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'attrdomv';

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CreateMethod := CreateCodesetDomain;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateCodesetName(MetaDataItem);
  CreateCodesetSource(MetaDataItem);
end;

function TframeMetaDataEditor.CreateCodesetName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Codeset Name -- the title of the codeset.';
  MetaDataItem.ShortName := 'codesetn';
  MetaDataItem.LongName := 'Codeset Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.3.1';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateCodesetName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateCodesetSource(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Codeset Source -- the authority for the codeset.';
  MetaDataItem.ShortName := 'codesets';
  MetaDataItem.LongName := 'Codeset Source';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.3.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateCodesetSource;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateCompatInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Compatibility Information -- description of other limitations or requirements for using the medium.';
  MetaDataItem.ShortName := 'compat';
  MetaDataItem.LongName := 'Compatibility Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4.2.2.2.4';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateCompatInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateCompleteness(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Completeness Report -- information about omissions, selection criteria, generalization, definitions used, and other rules used to derive the data set.';
  MetaDataItem.ShortName := 'complete';
  MetaDataItem.LongName := 'Completeness Report';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.3';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreateCompleteness;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCompressionSupport(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Compression Support -- data compression available through the modem service to speed data transfer.';
  MetaDataItem.ShortName := 'compress';
  MetaDataItem.LongName := 'Compression Support';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4.2.2.1.1.2.6';

  MetaDataItem.Choices.Add('V.32');
  MetaDataItem.Choices.Add('V.32bis');
  MetaDataItem.Choices.Add('V.42');
  MetaDataItem.Choices.Add('V.42bis');

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateCompressionSupport;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateComputerContactInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Computer Contact Information -- instructions for establishing communications with the distribution computer.';
  MetaDataItem.ShortName := 'computer';
  MetaDataItem.LongName := 'Computer Contact Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1';

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateComputerContactInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateNetworkAddress(MetaDataItem);
  CreateDialupInstructions(MetaDataItem);
end;

function TframeMetaDataEditor.CreateContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Contact -- the party responsible for the metadata information. '
    + sLineBreak
    + '(USGS Best Practice) It is preferred to use the position or role '
    + 'contact information rather than a specific person. If a person must be '
    + 'listed, include Contact Organization.';
  MetaDataItem.ShortName := 'metc';
  MetaDataItem.LongName := 'Metadata Contact';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.4';
  MetaDataItem.TextHeight := 140;

  MetaDataItem.CreateMethod := CreateContact;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  ChildItem := CreateContactInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
end;

function TframeMetaDataEditor.CreateContactAddress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Address -- the address for the organization or individual.';
  MetaDataItem.ShortName := 'cntaddr';
  MetaDataItem.LongName := 'Contact Address';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.4';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactAddress;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAddressType(MetaDataItem);
  CreateAddressLine(MetaDataItem);
  CreateContactCity(MetaDataItem);
  CreateStateOrProvince(MetaDataItem);
  CreatePostalCode(MetaDataItem);
  CreateCountry(MetaDataItem);
end;

function TframeMetaDataEditor.CreateContactCity(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'City -- the city of the address. '
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "Reston".';
  MetaDataItem.ShortName := 'city';
  MetaDataItem.LongName := 'City';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.4.3';

  MetaDataItem.CreateMethod := CreateContactCity;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateContactFaxTelephone(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Facsimile Telephone -- the telephone number of a facsimile machine of the organization or individual.';
  MetaDataItem.ShortName := 'cntfax';
  MetaDataItem.LongName := 'Contact Facsimile Telephone';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.7';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactFaxTelephone;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateContactHandicapTelephone(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact TDD/TTY Telephone -- the telephone number by which hearing-impaired individuals can contact the organization or individual.';
  MetaDataItem.ShortName := 'cnttdd';
  MetaDataItem.LongName := 'Contact TDD/TTY Telephone';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.6';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactHandicapTelephone;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateContactInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Information -- Identity of, and means to communicate with, person(s) and organization(s) associated with the data set.';
  MetaDataItem.ShortName := 'cntinfo';
  MetaDataItem.LongName := 'Contact Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10';

  MetaDataItem.CreateMethod := CreateContactInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateContactPersonPrimary(MetaDataItem);
  CreateContactOrganizationPrimary(MetaDataItem);
  CreateContactPosition(MetaDataItem);
  CreateContactAddress(MetaDataItem);
  CreateContactTelephone(MetaDataItem);
  CreateContactHandicapTelephone(MetaDataItem);
  CreateContactFaxTelephone(MetaDataItem);
  CreateEMailAddress(MetaDataItem);
  CreateHoursOfService(MetaDataItem);
  CreateContactInstructions(MetaDataItem);
end;

function TframeMetaDataEditor.CreateContactInstructions(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Instructions -- supplemental instructions on how or when to contact the individual or organization.';
  MetaDataItem.ShortName := 'cntinst';
  MetaDataItem.LongName := 'Contact Instructions';
  MetaDataItem.Required := False;
  MetaDataItem.ID := '10.10';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactInstructions;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateContactOrganization(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Organization -- the name of the organization to which the contact type applies.'
    + sLineBreak
    + 'USGS data sets must list the contact as "U.S. Geological Survey".';
  MetaDataItem.ShortName := 'cntorg';
  MetaDataItem.LongName := 'Contact Organization';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.1.2';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateContactOrganization;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateContactOrganizationPrimary(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Organization Primary -- the '
    + 'organization, and the member of the organization, associated with the '
    + 'data set. Used in cases where the association of the organization to '
    + 'the data set is more significant than the association of the person to '
    + 'the data set. '

    + sLineBreak
    + 'For Metadata Reference Information, USGS data sets must choose this option and list the contact as "U.S. Geological Survey".';

  MetaDataItem.ShortName := 'cntorgp';
  MetaDataItem.LongName := 'Contact Organization Primary';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.2';
  MetaDataItem.TextHeight := 240;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'cntinfo';

  MetaDataItem.CreateMethod := CreateContactOrganizationPrimary;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MChild := CreateContactOrganization(MetaDataItem);
  MChild.Required := True;
  MChild.RequiredType := rtManditory;
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;

  ChildItem := CreateContactPerson(MetaDataItem);
  ChildItem.RequiredType := rtOptional;
end;

function TframeMetaDataEditor.CreateContactPerson(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Person -- the name of the individual to which the contact type applies.';
  MetaDataItem.ShortName := 'cntper';
  MetaDataItem.LongName := 'Contact Person';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.1.1';

  MetaDataItem.CreateMethod := CreateContactPerson;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateContactPersonPrimary(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Person Primary -- the person, and the affiliation of the person, associated with the data set. Used in cases where the association of the person to the data set is more significant than the association of the organization to the data set.';
  MetaDataItem.ShortName := 'cntperp';
  MetaDataItem.LongName := 'Contact Person Primary';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.1';
  MetaDataItem.TextHeight := 140;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'cntinfo';

  MetaDataItem.CreateMethod := CreateContactPersonPrimary;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MChild := CreateContactPerson(MetaDataItem);
  MChild.Required := True;
  MChild.RequiredType := rtManditory;
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;

  ChildItem := CreateContactOrganization(MetaDataItem);
  ChildItem.RequiredType := rtOptional;
end;

function TframeMetaDataEditor.CreateContactPosition(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Position -- the title of individual.'
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "Ask USGS - Water Webserver Team".';
  MetaDataItem.ShortName := 'cntpos';
  MetaDataItem.LongName := 'Contact Position';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.3';
  MetaDataItem.TextHeight := 115;

//  MetaDataItem.Choices.Add('Unknown');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactPosition;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateContactTelephone(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Voice Telephone -- the telephone number by which individuals can speak to the organization or individual.'
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "1-888-275-8747 (1-888-ASK-USGS)".';
  MetaDataItem.ShortName := 'cntvoice';
  MetaDataItem.LongName := 'Contact Voice Telephone';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.5';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateContactTelephone;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCoordRepresentation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Coordinate Representation -- the method of encoding the position of a point by measuring its distance from perpendicular reference axes (the "coordinate pair" and "row and column" methods).';
  MetaDataItem.ShortName := 'coordrep';
  MetaDataItem.LongName := 'Coordinate Representation';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.2';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'planci';

  MetaDataItem.CreateMethod := CreateCoordRepresentation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateAbscissaResolution(MetaDataItem);
  CreateOrdinateResolution(MetaDataItem);
end;

function TframeMetaDataEditor.CreateCountry(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Country -- the country of the address.';
  MetaDataItem.ShortName := 'country';
  MetaDataItem.LongName := 'Country';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.4.6';

  MetaDataItem.CreateMethod := CreateCountry;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

procedure TframeMetaDataEditor.UpdateCiteAbbreviations(Sender: TObject);
var
  MetaDataItem: TChoiceMetaDataItem;
  ParentItem: TCustomMetaDataItem;
  LineageTreeViewItem: TTreeViewItem;
  LineageMetaData: TCustomMetaDataItem;
  ParentTreeViewItem: TTreeViewItem;
  ChildIndex: Integer;
  ChildItem: TTreeViewItem;
  ChildMetaData: TCustomMetaDataItem;
  GrandChildIndex: Integer;
  GrandChildItem: TTreeViewItem;
  GrandChildMetaData: TCustomMetaDataItem;
begin
//  Beep;
//  ShowMessage(Sender.ClassName);
  MetaDataItem := Sender as TChoiceMetaDataItem;
  MetaDataItem.Choices.Clear;
//  MetaDataItem.Choices.Add('a');
  ParentTreeViewItem := MetaDataItem.TreeViewItem.ParentItem as TTreeViewItem;
  ParentItem := ParentTreeViewItem.TagObject as TCustomMetaDataItem;

  LineageTreeViewItem := ParentItem.TreeViewItem.ParentItem as TTreeViewItem;
  LineageMetaData := LineageTreeViewItem.TagObject as TCustomMetaDataItem;
  Assert(LineageMetaData.ShortName = 'lineage');
  for ChildIndex := 0 to LineageTreeViewItem.Count - 1 do
  begin
    ChildItem := LineageTreeViewItem.Items[ChildIndex];
    if ChildItem.IsChecked then
    begin
      ChildMetaData := ChildItem.TagObject as TCustomMetaDataItem;
      // 2.5.1
      if ChildMetaData.ShortName = 'srcinfo' then
      begin
        for GrandChildIndex := 0 to ChildItem.Count - 1 do
        begin
          GrandChildItem := ChildItem[GrandChildIndex];
          if GrandChildItem.IsChecked then
          begin
            GrandChildMetaData := GrandChildItem.TagObject as TCustomMetaDataItem;
            // 2.5.1.6
            if GrandChildMetaData.ShortName = 'srccitea' then
            begin
              MetaDataItem.Choices.Add(GrandChildMetaData.GetStringContent);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TframeMetaDataEditor.CreateCrossReference(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Cross Reference -- information about other, related data sets that are likely to be of interest.';
  MetaDataItem.ShortName := 'crossref';
  MetaDataItem.LongName := 'Cross Reference';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.14';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateCrossReference;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  ChildItem := CreateCiteInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
end;

function TframeMetaDataEditor.CreateCurrentRef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Currentness Reference -- the basis on which the time period of content information is determined.'
    + sLineBreak
    + '(USGS Best Practice) For a system, enter “publication date” which is '
    + 'the date when data was compiled and delivered.';
  MetaDataItem.ShortName := 'current';
  MetaDataItem.LongName := 'Currentness Reference';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.3.1';
  MetaDataItem.Choices.Add('ground condition');
  MetaDataItem.Choices.Add('publication date');
  MetaDataItem.Choice := '';
  MetaDataItem.TextHeight := 130;
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateCurrentRef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateCustomOrderProcess(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Custom Order Process -- description of custom distribution services available, and the terms and conditions for obtaining these services.';
  MetaDataItem.ShortName := 'custom';
  MetaDataItem.LongName := 'Custom Order Process';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.5';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateCustomOrderProcess;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateIdInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Identification Information -- basic information about the data set.';
  MetaDataItem.ShortName := 'idinfo';
  MetaDataItem.LongName := 'Identification Information';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateIdInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateCitation(MetaDataItem);
  CreateDescription(MetaDataItem);
  CreateTimePeriod(MetaDataItem);
  CreateStatus(MetaDataItem);
  CreateSpatialDomain(MetaDataItem);
  CreateKeywords(MetaDataItem);
  CreateAccessConstraints(MetaDataItem);
  CreateUseConstraints(MetaDataItem);

  CreatePointOfContact(MetaDataItem);
  CreateBrowseGraphic(MetaDataItem);
  CreateDataSetCredit(MetaDataItem);
  CreateSecurityInformation(MetaDataItem);
  CreateNativeDataSetEnvironment(MetaDataItem);
  CreateCrossReference(MetaDataItem);

end;

function TframeMetaDataEditor.CreateIndirectSpatialReference(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Indirect Spatial Reference -- name of types of geographic features, addressing schemes, or other means through which locations are referenced in the data set.';
  MetaDataItem.ShortName := 'indspref';
  MetaDataItem.LongName := 'Indirect Spatial Reference';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '3.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateIndirectSpatialReference;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Keywords -- words or phrases summarizing an aspect of the data set.'
    + sLineBreak
    + '(USGS Best Practice) Keywords are divided by type (Theme, Place etc.). '
    + 'All keywords should come from an authoritative source such as a '
    + 'thesaurus relevant to your discipline.'
    + sLineBreak
    + '*Example authoritative sources for theme keywords include the USGS '
    + 'Thesaurus, USGS Biocomplexity Thesaurus, and the ISO 19115 Topic '
    + 'Category.'
    + sLineBreak
    + '*Example authoritative sources for place keywords include Geographic '
    + 'Names Information System. ';
  MetaDataItem.ShortName := 'keywords';
  MetaDataItem.LongName := 'Keywords';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6';
  MetaDataItem.TextHeight := 300;

  MetaDataItem.CreateMethod := CreateKeywords;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateTheme(MetaDataItem);
  CreatePlace(MetaDataItem);
  CreateStratum(MetaDataItem);
  CreateTemporal(MetaDataItem);
end;

function TframeMetaDataEditor.CreateLamAzEqArea(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Lambert Azimuthal Equal Area -- contains parameters for the Lambert Azimuthal Equal Area projection.';
  MetaDataItem.ShortName := 'lamberta';
  MetaDataItem.LongName := 'Lambert Azimuthal Equal Area';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.8';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateLamAzEqArea;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateLongPerspCenter(MetaDataItem);
  CreateLatPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateLambertConformalConic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Lambert Conformal Conic -- contains parameters for the Lambert Conformal Conic projection.';
  MetaDataItem.ShortName := 'lambertc';
  MetaDataItem.LongName := 'Lambert Conformal Conic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.9';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateLambertConformalConic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateStandardParallel(MetaDataItem);

  MChild := CreateStandardParallel(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLandsatNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Landsat Number -- number of the Landsat satellite. (Note: This data element exists solely to provide a'
    + ' parameter needed to define the space oblique mercator projection. It is not used to identify data'
    + ' originating from a remote sensing vehicle.)';
  MetaDataItem.ShortName := 'landsat';
  MetaDataItem.LongName := 'Landsat Number';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.15';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.Content := 0;

  MetaDataItem.CreateMethod := CreateLandsatNumber;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLandsatPathNumber(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Path Number -- number of the orbit of the Landsat satellite. (Note: This data element exists solely to'
    + ' provide a parameter needed to define the space oblique mercator projection. It is not used to identify'
    + ' data originating from a remote sensing vehicle.)';
  MetaDataItem.ShortName := 'pathnum';
  MetaDataItem.LongName := 'Path Number';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.16';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;

  MetaDataItem.CreateMethod := CreateLandsatPathNumber;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

procedure TframeMetaDataEditor.OnLargerCheckChecked(Sender: TObject);
var
  CheckBox: TCheckBox;
  TreeViewItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
begin
  CheckBox := Sender as TCheckBox;
  TreeViewItem := CheckBox.Parent as TTreeViewItem;
  MetaData := TreeViewItem.TagObject as TCustomMetaDataItem;
  MetaData.CheckBoxChecked(Sender);
  if CheckBox.IsChecked then
  begin
    if TreeViewItem.Count = 0 then
    begin
      CreateCiteInfo(MetaData);
    end;
  end;
end;

function TframeMetaDataEditor.CreateLargerWorkCite(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Larger Work Citation -- the information identifying a larger work in which the data set is included.';
  MetaDataItem.ShortName := 'lworkcit';
  MetaDataItem.LongName := 'Larger Work Citation';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.11';

  MetaDataItem.CreateMethod := CreateLargerWorkCite;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  MetaDataItem.CheckBox.OnChange := OnLargerCheckChecked;

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateLatitudeResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Latitude Resolution -- the minimum difference between two adjacent latitude values expressed in Geographic Coordinate Units of measure.';
  MetaDataItem.ShortName := 'latres';
  MetaDataItem.LongName := 'Latitude Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.1.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 1;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateLatitudeResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateLatPerspCenter(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Latitude of Projection Center -- latitude of the point of projection for azimuthal projections.';
  MetaDataItem.ShortName := 'latprjc';
  MetaDataItem.LongName := 'Latitude of Projection Center';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.9';

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 90;
  MetaDataItem.MinValue := -90;

  MetaDataItem.CreateMethod := CreateLatPerspCenter;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLatProjOrigin(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Latitude of Projection Origin -- latitude chosen as the origin of rectangular coordinates for a map projection.';
  MetaDataItem.ShortName := 'latprjo';
  MetaDataItem.LongName := 'Latitude of Projection Origin';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.3';

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 90;
  MetaDataItem.MinValue := -90;

  MetaDataItem.CreateMethod := CreateLatProjOrigin;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLineage(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Lineage -- information about the events, parameters, and source data which constructed the data set, and information about the responsible parties.';
  MetaDataItem.ShortName := 'lineage';
  MetaDataItem.LongName := 'Lineage';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreateLineage;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateSourceInformation(MetaDataItem);
  CreateProcessStep(MetaDataItem);
end;

function TframeMetaDataEditor.CreateLocal(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local -- a description of any coordinate system that is not aligned with the surface of the Earth.';
  MetaDataItem.ShortName := 'local';
  MetaDataItem.LongName := 'Local';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.3';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'horizsys';

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CreateMethod := CreateLocal;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateLocalDescription(MetaDataItem);
  CreateLocalGeoref(MetaDataItem);
end;

function TframeMetaDataEditor.CreateLocalDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local Description -- a description of the coordinate system and its orientation to the surface of the Earth.';
  MetaDataItem.ShortName := 'localdes';
  MetaDataItem.LongName := 'Local Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.3.1';

  MetaDataItem.CreateMethod := CreateLocalDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLocalGeoref(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local Georeference Information -- a description of the information provided to register the local system to the'
    + ' Earth (e.g. control points, satellite ephemeral data, inertial'
    + ' navigation data).';
  MetaDataItem.ShortName := 'localgeo';
  MetaDataItem.LongName := 'Local Georeference Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.3.2';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateLocalGeoref;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLocalPlanar(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local Planar -- any right-handed planar coordinate system of which the z-axis coincides with a plumb line through the origin that locally is aligned with the surface of the Earth.';
  MetaDataItem.ShortName := 'localp';
  MetaDataItem.LongName := 'Local Planar';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.3';
  MetaDataItem.TextHeight := 110;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'planar';

  MetaDataItem.CreateMethod := CreateLocalPlanar;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLocalPlanarDescription(MetaDataItem);
  CreateLocalPlanarGeoref(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLocalPlanarDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local Planar Description -- a description of the local planar system.';
  MetaDataItem.ShortName := 'localpd';
  MetaDataItem.LongName := 'Local Planar Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.3.1';

  MetaDataItem.CreateMethod := CreateLocalPlanarDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLocalPlanarGeoref(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Local Planar Georeference Information -- a description of the information provided to register the local planar system to the Earth (e.g. control points, satellite ephemeral data, inertial navigation data).';
  MetaDataItem.ShortName := 'localpgi';
  MetaDataItem.LongName := 'Local Planar Georeference Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.3.2';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.CreateMethod := CreateLocalPlanarGeoref;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLogicalConsistency(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Logical Consistency Report -- an explanation of the fidelity of relationships in the data set and tests used.';
  MetaDataItem.ShortName := 'logic';
  MetaDataItem.LongName := 'Logical Consistency Report';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.2';

  MetaDataItem.CreateMethod := CreateLogicalConsistency;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateLongitudeResolution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Longitude Resolution -- the minimum difference between two adjacent longitude values expressed in Geographic Coordinate Units of measure.';
  MetaDataItem.ShortName := 'longres';
  MetaDataItem.LongName := 'Longitude Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.1.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 1;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateLongitudeResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateLongitudeStrdMeridian(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Longitude of Central Meridian -- the line of longitude at the center of a map projection generally used as the basis for constructing the projection.';
  MetaDataItem.ShortName := 'longcm';
  MetaDataItem.LongName := 'Longitude of Central Meridian';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 180;
  MetaDataItem.MaxLimitType := mltLess;
  MetaDataItem.MinValue := -180;

  MetaDataItem.CreateMethod := CreateLongitudeStrdMeridian;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateLongPerspCenter(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Longitude of Projection Center -- longitude of the point of projection for azimuthal projections.';
  MetaDataItem.ShortName := 'longpc';
  MetaDataItem.LongName := 'Longitude of Projection Center';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.8';

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 180;
  MetaDataItem.MinValue := -180;
  MetaDataItem.MaxLimitType := mltLess;

  MetaDataItem.CreateMethod := CreateLongPerspCenter;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateLowestBPS(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Lowest BPS -- lowest or only speed for the connection''s communication, expressed in bits per second.';
  MetaDataItem.ShortName := 'lowbps';
  MetaDataItem.LongName := 'Lowest BPS';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.1';

  MetaDataItem.Content := 110;
  MetaDataItem.MinValue := 110;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateLowestBPS;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateMapProjection(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Map Projection -- the systematic representation of all or part of the surface of the Earth on a plane or developable surface.';
  MetaDataItem.ShortName := 'mapproj';
  MetaDataItem.LongName := 'Map Projection';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'planar';

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CreateMethod := CreateMapProjection;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateMapProjectionName(MetaDataItem);
  CreateAlbersConicalEqualArea(MetaDataItem);
  CreateAzimuthEqualArea(MetaDataItem);
  CreateEquidistantConic(MetaDataItem);
  CreateEquirectangular(MetaDataItem);
  CreateGenVertNearPersp(MetaDataItem);
  CreateGnomonic(MetaDataItem);
  CreateLamAzEqArea(MetaDataItem);
  CreateLambertConformalConic(MetaDataItem);
  CreateMercator(MetaDataItem);
  CreateModSteroAlaska(MetaDataItem);
  CreateMillerCylindrical(MetaDataItem);
  CreateObliqueMercator(MetaDataItem);
  CreateOrthographic(MetaDataItem);
  CreatePolarStereoGraphic(MetaDataItem);
  CreatePolyconic(MetaDataItem);
  CreateRobinson(MetaDataItem);
  CreateSinusoidal(MetaDataItem);
  CreateSpaceObliqueMercator(MetaDataItem);
  CreateStereographic(MetaDataItem);
  CreateTransverseMercator(MetaDataItem);
  CreateVanDerGrinten(MetaDataItem);
  CreateMapProjParam(MetaDataItem);
end;

function TframeMetaDataEditor.CreateMapProjectionName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Map Projection Name -- name of the map projection.';
  MetaDataItem.ShortName := 'mapprojn';
  MetaDataItem.LongName := 'Map Projection Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.1';

  MetaDataItem.Choices.Add('Albers Conical Equal Area');
  MetaDataItem.Choices.Add('Azimuthal Equidistant');
  MetaDataItem.Choices.Add('Equidistant Conic');
  MetaDataItem.Choices.Add('Equirectangular');
  MetaDataItem.Choices.Add('General Vertical Near-sided Perspective');
  MetaDataItem.Choices.Add('Gnomonic');
  MetaDataItem.Choices.Add('Lambert Azimuthal Equal Area');
  MetaDataItem.Choices.Add('Lambert Conformal Conic');
  MetaDataItem.Choices.Add('Mercator');
  MetaDataItem.Choices.Add('Modified Stereographic for Alaska');
  MetaDataItem.Choices.Add('Miller Cylindrical');
  MetaDataItem.Choices.Add('Oblique Mercator');
  MetaDataItem.Choices.Add('Orthographic');
  MetaDataItem.Choices.Add('Polar Stereographic');
  MetaDataItem.Choices.Add('Polyconic');
  MetaDataItem.Choices.Add('Robinson');
  MetaDataItem.Choices.Add('Sinusoidal');
  MetaDataItem.Choices.Add('Space Oblique Mercator');
  MetaDataItem.Choices.Add('Stereographic');
  MetaDataItem.Choices.Add('Transverse Mercator');
  MetaDataItem.Choices.Add('van der Grinten');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateMapProjectionName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateMapProjParam(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Map Projection Parameters -- a complete parameter set of the projection that was used for the data set. The'
    + ' information provided shall include the names of the parameters and values used for the data set that'
    + ' describe the mathematical relationship between the Earth and the plane or developable surface for the'
    + ' projection.';
  MetaDataItem.ShortName := 'mapprojp';
  MetaDataItem.LongName := 'Map Projection Parameters';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23';
  MetaDataItem.TextHeight := 175;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateMapProjParam;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  // feast
  MChild := CreateFalseEasting(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // fnorth
  MChild := CreateFalseNorthing(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // heightpt
  MChild := CreateHeightOfPerspectivePoint(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // landsat
  MChild := CreateLandsatNumber(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // latprjc
  MChild := CreateLatPerspCenter(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // latprjo
  MChild := CreateLatProjOrigin(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // longcm
  MChild := CreateLongitudeStrdMeridian(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // longpc
  MChild := CreateLongPerspCenter(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // pathnum
  MChild := CreateLandsatPathNumber(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // sfctrlin
  MChild := CreateScaleFactorAtCenterLine(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // sfctrmer
  MChild := CreateScaleFactorAtCentMerid(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // sfequat
  MChild := CreateScaleFactorAtEquator(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // sfprjorg
  MChild := CreateScaleFactOfProjOrigin(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // stdparll
  MChild := CreateStandardParallel(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // stdparll
  MChild := CreateStandardParallel(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // svlong
  MChild := CreateStraightVertLineFromPole(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // otherprj
  MChild := CreateOtherProjDef(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // obqlazim
  MChild := CreateObliqueLineAzimuth(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  // obqlpt
  MChild := CreateObliqueLinePoint(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  MChild.RequiredType := rtManditoryIfApplicable;
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Mercator -- contains parameters for the Mercator projection';
  MetaDataItem.ShortName := 'mercator';
  MetaDataItem.LongName := 'Mercator';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.10';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateMercator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MChild := CreateStandardParallel(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'mercator';
  MChild.Required := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

  MChild := CreateScaleFactorAtEquator(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'mercator';
  MChild.Required := False;
  MChild.Used := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateMetaAccessConstraints(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Access Constraints -- restrictions and legal prerequisites for accessing the metadata. These include'
    + ' any access constraints applied to assure the protection of privacy or intellectual property, and any special'
    + ' restrictions or limitations on obtaining the metadata.';
  MetaDataItem.ShortName := 'metac';
  MetaDataItem.LongName := 'Metadata Access Constraints';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '7.8';
  MetaDataItem.TextHeight := 150;

  MetaDataItem.CreateMethod := CreateMetaAccessConstraints;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaDataFutureReviewDate(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Future Review Date -- the date by which the metadata entry should be reviewed.';
  MetaDataItem.ShortName := 'metfrd';
  MetaDataItem.LongName := 'Metadata Future Review Date';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '7.3';

  MetaDataItem.CreateMethod := CreateMetaDataFutureReviewDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaDataReviewDate(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Review Date -- the date of the latest review of the metadata entry.';
  MetaDataItem.ShortName := 'metrd';
  MetaDataItem.LongName := 'Metadata Review Date';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '7.2';

  MetaDataItem.CreateMethod := CreateMetaDataReviewDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaDataRoot(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  MetaDataRootItem: TTreeViewItem;
begin
  Assert(Parent = nil);
  MetaDataRootItem := TTreeViewItem.Create(self);
  MetaDataRootItem.Parent := tvMetaData;
  MetaDataRootItem.Visible := True;
//  MetaDataRootItem.Font.Size := 32;

  FRootItem := TCompoundItemMetaDataItem.Create(MetaDataRootItem);
  FRootItem.Description := 'Metadata -- data about the content, quality, condition, and other characteristics of data. ';
  FRootItem.ShortName := 'metadata';
  FRootItem.LongName := 'Metadata';
  FRootItem.Required := True;
  FRootItem.ID := '0';
  FRootItem.MoreThanOneAllowed := False;
  FRootItem.RequiredType := rtManditory;

  FRootItem.CreateMethod := CreateMetaDataRoot;
  AssignMetaDataToTreeViewItem(FRootItem);

  result := FRootItem;

  CreateIdInfo(FRootItem);
  CreateDataQuality(FRootItem);
  CreateSpatialDataOrgInfo(FRootItem);
  CreateSpatialReferenceInfo(FRootItem);
  CreateEntityAttributesInfo(FRootItem);
  CreateDistributionInfo(FRootItem);
  CreateReferenceInfo(FRootItem);
end;

function TframeMetaDataEditor.CreateMetaExtensions(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Extensions -- a reference to extended elements to the standard which may be defined by a metadata'
    + ' producer or a user community. Extended elements are elements outside the Standard, but needed by the'
    + ' metadata producer. If extended elements are created, they must follow the guidelines in Appendix D, Guidelines'
    + ' for Creating Extended Elements to the Content Standard for Digital Geospatial Metadata.';
  MetaDataItem.ShortName := 'metextns';
  MetaDataItem.LongName := 'Metadata Extensions';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '7.11';
  MetaDataItem.TextHeight := 220;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateMetaExtensions;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateOnlineLinkage(MetaDataItem);
  CreateProfileName(MetaDataItem);
end;

function TframeMetaDataEditor.CreateMetaSecurityClassification(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Security Classification -- name of the handling restrictions on the metadata.';
  MetaDataItem.ShortName := 'metsc';
  MetaDataItem.LongName := 'Metadata Security Classification';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.10.2';

  MetaDataItem.Choices.Add('Top secret');
  MetaDataItem.Choices.Add('Secret');
  MetaDataItem.Choices.Add('Confidential');
  MetaDataItem.Choices.Add('Restricted');
  MetaDataItem.Choices.Add('Unclassified');
  MetaDataItem.Choices.Add('Sensitive');

  MetaDataItem.CreateMethod := CreateMetaSecurityClassification;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaSecurityClassSys(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Security Classification System -- name of the classification system for the metadata.';
  MetaDataItem.ShortName := 'metscs';
  MetaDataItem.LongName := 'Metadata Security Classification System';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.10.1';

  MetaDataItem.CreateMethod := CreateMetaSecurityClassSys;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaSecurityHandlingDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Security Handling Description -- additional information about the restrictions on handling the metadata.';
  MetaDataItem.ShortName := 'metshd';
  MetaDataItem.LongName := 'Metadata Security Handling Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.10.3';

  MetaDataItem.CreateMethod := CreateMetaSecurityHandlingDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaSecurityInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Security Information -- handling restrictions imposed on the metadata because of national security, privacy, or other concerns.';
  MetaDataItem.ShortName := 'metsi';
  MetaDataItem.LongName := 'Metadata Security Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '7.10';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateMetaSecurityInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateMetaSecurityClassSys(MetaDataItem);
  CreateMetaSecurityClassification(MetaDataItem);
  CreateMetaSecurityHandlingDescription(MetaDataItem);
end;

function TframeMetaDataEditor.CreateMetaTimeConvention(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Time Convention -- form used to convey time of day information in the metadata entry. Used if time of day information is included in the metadata for a data set.';
  MetaDataItem.ShortName := 'mettc';
  MetaDataItem.LongName := 'Metadata Time Convention';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '7.7';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.Choices.Add('local time');
  MetaDataItem.Choices.Add('local time with time differential factor');
  MetaDataItem.Choices.Add('universal time');

  MetaDataItem.CreateMethod := CreateMetaTimeConvention;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMetaUseConstraints(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Use Constraints -- restrictions and legal prerequisites for using the metadata after access is granted.'
    + ' These include any metadata use constraints applied to assure the protection of privacy or intellectual property,'
    + ' and any special restrictions or limitations on using the metadata.';
  MetaDataItem.ShortName := 'metuc';
  MetaDataItem.LongName := 'Metadata Use Constraints';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '7.9';
  MetaDataItem.TextHeight := 180;

  MetaDataItem.CreateMethod := CreateMetaUseConstraints;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateMillerCylindrical(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Miller Cylindrical -- contains parameters for the Miller Cylindrical projection.';
  MetaDataItem.ShortName := 'miller';
  MetaDataItem.LongName := 'Miller Cylindrical';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.12';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateMillerCylindrical;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateModSteroAlaska(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Modified Stereographic for Alaska -- contains parameters for the Modified Stereographic for Alaska projection.';
  MetaDataItem.ShortName := 'modsak';
  MetaDataItem.LongName := 'Modified Stereographic for Alaska';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.11';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateModSteroAlaska;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateMultipleDateTimes(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Multiple Dates/Times -- means of encoding multiple individual dates and times.';
  MetaDataItem.ShortName := 'mdattim';
  MetaDataItem.LongName := 'Multiple Dates/Times';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.2';

  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'timeinfo';

  MetaDataItem.CreateMethod := CreateMultipleDateTimes;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  MChild := CreateSingleDateTime(MetaDataItem);
  MChild.MoreThanOneAllowed := True;
  MChild.Used := True;
  MChild.CheckType := ctCheckBox;
  AssignMetaDataToTreeViewItem(MChild);

  MChild := CreateSingleDateTime(MetaDataItem);
  MChild.MoreThanOneAllowed := True;
  MChild.Used := True;
  MChild.CheckType := ctCheckBox;
  AssignMetaDataToTreeViewItem(MChild);
end;

function TframeMetaDataEditor.CreateNativeDataSetEnvironment(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Native Data Set Environment -- a description of the data set in the producer''s processing'
    + ' environment, including items such as the name of the software (including version), the'
    + ' computer operating system, file name (including host-, path-, and filenames), and the data set size.';
  MetaDataItem.ShortName := 'native';
  MetaDataItem.LongName := 'Native Data Set Environment';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.13';
  MetaDataItem.TextHeight := 160;

  MetaDataItem.CreateMethod := CreateNativeDataSetEnvironment;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateNetworkAddress(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Network Address -- the electronic address from which the data set can be obtained from the distribution computer.';
  MetaDataItem.ShortName := 'networka';
  MetaDataItem.LongName := 'Network Address';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.1';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateNetworkAddress;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateNetworkResourceName(MetaDataItem);
end;

function TframeMetaDataEditor.CreateNetworkResourceName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Network Resource Name -- the name of the file or service from which the data set can be obtained.'
    + sLineBreak
    + '(USGS Best Practice) The distribution linkage for the URL to the web '
    + 'service or direct links to the data.'
    + sLineBreak
    + 'Distribution files are accessed via https (not ftp), and use an address similar to this form:'
    + sLineBreak
    + 'https://water.usgs.gov/GIS/dsdl/OFR2006-1308_Drainage_Basin_shape.zip';

  MetaDataItem.ShortName := 'networkr';
  MetaDataItem.LongName := 'Network Resource Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.1.1';

  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 220;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateNetworkResourceName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateNonDigitalForm(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Non-digital Form -- the description of options for obtaining the data set on non-computer- compatible media.';
  MetaDataItem.ShortName := 'nondig';
  MetaDataItem.LongName := 'Non-digital Form';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.1';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'stdorder';

  MetaDataItem.CreateMethod := CreateNonDigitalForm;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateNumDataBits(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Number DataBits -- number of data bits in each character exchanged in the communication.';
  MetaDataItem.ShortName := 'numdata';
  MetaDataItem.LongName := 'Number DataBits';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.3';

  MetaDataItem.Content := 8;
  MetaDataItem.MinValue := 7;
  MetaDataItem.MaxValue := 8;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateNumDataBits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateNumStopBits(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Number StopBits -- number of stop bits in each character exchanged in the communication.';
  MetaDataItem.ShortName := 'numstop';
  MetaDataItem.LongName := 'Number StopBits';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.4';

  MetaDataItem.Content := 2;
  MetaDataItem.MinValue := 1;
  MetaDataItem.MaxValue := 2;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateNumStopBits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateObliqueLineAzimuth(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Oblique Line Azimuth -- method used to describe the line along which an oblique mercator map projection is centered using the map projection origin and an azimuth.';
  MetaDataItem.ShortName := 'obqlazim';
  MetaDataItem.LongName := 'Oblique Line Azimuth';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.11';
  MetaDataItem.TextHeight := 110;

  MetaDataItem.CreateMethod := CreateObliqueLineAzimuth;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateAzimuthAngle(MetaDataItem);
  CreateAzimuthMeasPtLong(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateObliqueLineLatitude(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Oblique Line Latitude -- latitude of a point defining the oblique line.';
  MetaDataItem.ShortName := 'obqllat';
  MetaDataItem.LongName := 'Oblique Line Latitude';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.12.1';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := -90;
  MetaDataItem.MaxValue := 90;

  MetaDataItem.CreateMethod := CreateObliqueLineLatitude;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateObliqueLineLongitude(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Oblique Line Longitude -- longitude of a point defining the oblique line.';
  MetaDataItem.ShortName := 'obqllong';
  MetaDataItem.LongName := 'Oblique Line Longitude';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.12.2';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := -180;
  MetaDataItem.MaxValue := 180;
  MetaDataItem.MaxLimitType := mltLess;

  MetaDataItem.CreateMethod := CreateObliqueLineLongitude;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateObliqueLinePoint(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Oblique Line Point -- method used to describe the line along which an oblique mercator map projection is centered using two points near the limits of the mapped region that define the center line.';
  MetaDataItem.ShortName := 'obqlpt';
  MetaDataItem.LongName := 'Oblique Line Point';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.12';
  MetaDataItem.TextHeight := 110;

  MetaDataItem.CreateMethod := CreateObliqueLinePoint;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateObliqueLineLatitude(MetaDataItem);
  CreateObliqueLineLatitude(MetaDataItem);
  CreateObliqueLineLongitude(MetaDataItem);
  CreateObliqueLineLongitude(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateObliqueMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Oblique Mercator -- contains parameters for the Oblique Mercator projection.';
  MetaDataItem.ShortName := 'obqmerc';
  MetaDataItem.LongName := 'Oblique Mercator';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.13';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateObliqueMercator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateScaleFactorAtCenterLine(MetaDataItem);
  MChild := CreateObliqueLineAzimuth(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'obqmerc';
  MChild.Required := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

  MChild := CreateObliqueLinePoint(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'obqmerc';
  MChild.Required := False;
  MChild.Used := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOfflineMedia(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Offline Media -- name of the media on which the data set can be received.';
  MetaDataItem.ShortName := 'offmedia';
  MetaDataItem.LongName := 'Offline Media';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.2.1';

  MetaDataItem.Choices.Add('CD-ROM');
  MetaDataItem.Choices.Add('3-1/2 inch floppy disk');
  MetaDataItem.Choices.Add('5-1/4 inch floppy disk');
  MetaDataItem.Choices.Add('9-track tape');
  MetaDataItem.Choices.Add('4 mm cartridge tape');
  MetaDataItem.Choices.Add('"8 mm cartridge tape');
  MetaDataItem.Choices.Add('1/4-inch cartridge tape');

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateOfflineMedia;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOfflineOption(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Offline Option -- information about media-specific options for receiving the data set.';
  MetaDataItem.ShortName := 'offoptn';
  MetaDataItem.LongName := 'Offline Option';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.2';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateOfflineOption;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateOfflineMedia(MetaDataItem);
  CreateRecordingCapacity(MetaDataItem);
  CreateRecordingFormat(MetaDataItem);
  CreateCompatInfo(MetaDataItem);
end;

function TframeMetaDataEditor.CreateOnlineCompAndOpSys(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Online Computer and Operating System -- the brand of distribution computer and its operating system.';
  MetaDataItem.ShortName := 'oncomp';
  MetaDataItem.LongName := 'Online Computer and Operating System';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.2.2.1.3';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateOnlineCompAndOpSys;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOnlineLinkage(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Online Linkage -- the name of an online computer resource that contains the metadata extension information for the data set. Entries should follow the Uniform Resource Locator convention of the Internet.'
    + sLineBreak
    + 'The first online link element in every metadata file, "onlink" must '
    + 'contain a reference to the "getspatial" element to ensure a reference '
    + 'to the node where the metadata and data reside. This allows other '
    + 'metadata catalogs like data.gov to reference the node and access the data.'
    + sLineBreak
    + sLineBreak
    + '<onlink>https://water.usgs.gov/lookup/getspatial?ofr02-338_bedrock</onlink>';

  MetaDataItem.ShortName := 'onlink';
  MetaDataItem.LongName := 'Online Linkage';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '7.11.1';
  MetaDataItem.TextHeight := 340;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateOnlineLinkage;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateOnlineCiteLinkage(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Online Linkage -- the name of an online computer resource that contains the data set. Entries should follow the'
    + ' Uniform Resource Locator convention of the Internet.'
    + sLineBreak
    + '(USGS Best Practice) The URL link to the system home page repeatable '
    + 'field). *For web services or direct links to the data, see '
    + 'Distribution Information section.'
    + sLineBreak
    + 'The first online link element in every metadata file, "onlink" must '
    + 'contain a reference to the "getspatial" element to ensure a reference '
    + 'to the node where the metadata and data reside. This allows other '
    + 'metadata catalogs like data.gov to reference the node and access the data.'
    + sLineBreak
    + sLineBreak
    + '<onlink>https://water.usgs.gov/lookup/getspatial?ofr02-338_bedrock</onlink>';
  MetaDataItem.ShortName := 'onlink';
  MetaDataItem.LongName := 'Online Linkage';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '8.10';
  MetaDataItem.TextHeight := 400;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateOnlineCiteLinkage;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateOnlineOption(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Online Option -- information required to directly obtain the data set electronically.';
  MetaDataItem.ShortName := 'onlinopt';
  MetaDataItem.LongName := 'Online Option';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateOnlineOption;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateComputerContactInfo(MetaDataItem);
  CreateAccessInst(MetaDataItem);
  CreateOnlineCompAndOpSys(MetaDataItem);
end;

function TframeMetaDataEditor.CreateOrderingInstructions(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ordering Instructions -- general instructions and advice about, and special terms and services provided for, the data set by the distributor.';
  MetaDataItem.ShortName := 'ordering';
  MetaDataItem.LongName := 'Ordering Instructions';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.4';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateOrderingInstructions;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOrdinateResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ordinate Resolution -- the (nominal) minimum distance between the "y" or row values of two adjacent points, expressed in Planar Distance Units of measure.';
  MetaDataItem.ShortName := 'ordres';
  MetaDataItem.LongName := 'Ordinate Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.2.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateOrdinateResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOriginator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Originator -- the name of an organization or '
    + 'individual that developed the data set. If the name of editors or '
    + 'compilers are provided, the name must be followed by "(ed.)" or '
    + '"(comp.)" respectively. '
    + sLineBreak
    + '(USGS Best Practice) Enter the Organization (required). Repeat the '
    + 'field and enter the Program/Science Center (required).';
  MetaDataItem.ShortName := 'origin';
  MetaDataItem.LongName := 'Originator';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.1';
  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choice := '';
  MetaDataItem.TextHeight := 180;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateOriginator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateOrthographic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Orthographic -- contains parameters for the Orthographic projection';
  MetaDataItem.ShortName := 'orthogr';
  MetaDataItem.LongName := 'Orthographic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.14';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateOrthographic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongPerspCenter(MetaDataItem);
  CreateLatPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);
end;

function TframeMetaDataEditor.CreateOtherCiteInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Other Citation Details -- other information required to complete the citation.';
  MetaDataItem.ShortName := 'othercit';
  MetaDataItem.LongName := 'Other Citation Details';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.9';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateOtherCiteInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateOtherCoordSys(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Other Grid System''s Definition -- a complete description of a grid system, not defined elsewhere in this'
    + ' standard, that was used for the data set. The information provided shall include the name of the grid'
    + ' system, the names of the parameters and values used for the data set, and the citation of the specification'
    + ' for the algorithms that describe the mathematical relationship between the Earth and the coordinates of the'
    + ' grid system.';
  MetaDataItem.ShortName := 'othergrd';
  MetaDataItem.LongName := 'Other Grid System''s Definition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.6';
  MetaDataItem.TextHeight := 240;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateOtherCoordSys;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOtherProjDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Other Projection''s Definition -- a description of a projection, not defined elsewhere in the standard, that'
    + ' was used for the data set. The information provided shall include the name of the projection, names of'
    + ' parameters and values used for the data set, and the citation of the specification for the algorithms that'
    + ' describe the mathematical relationship between Earth and plane or developable surface for the'
    + ' projection.';
  MetaDataItem.ShortName := 'otherprj';
  MetaDataItem.LongName := 'Other Projection''s Definition';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.18';
  MetaDataItem.TextHeight := 220;

  MetaDataItem.Content := '';

  MetaDataItem.CreateMethod := CreateOtherProjDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateOverviewDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Overview Description -- summary of, and citation to detailed description of, the information content of the data set.';
  MetaDataItem.ShortName := 'overview';
  MetaDataItem.LongName := 'Overview Description';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateOverviewDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateEntityAndAttributeOverviewDescription(MetaDataItem);
  CreateEntityAndAttributeCitation(MetaDataItem);
end;

function TframeMetaDataEditor.CreateParity(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Parity -- parity error checking used in each character exchanged in the communication.';
  MetaDataItem.ShortName := 'parity';
  MetaDataItem.LongName := 'Parity';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.5';

  MetaDataItem.Choices.Add('None');
  MetaDataItem.Choices.Add('Odd');
  MetaDataItem.Choices.Add('Even');
  MetaDataItem.Choices.Add('Mark');
  MetaDataItem.Choices.Add('Space');

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateParity;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePlace(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Place -- geographic locations characterized by the data set.';
  MetaDataItem.ShortName := 'place';
  MetaDataItem.LongName := 'Place';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '1.6.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePlace;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreatePlaceThesaurus(MetaDataItem);
  CreatePlaceKeyword(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePlaceKeyword(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Place Keyword -- the geographic name of a location covered by a data set.';
  MetaDataItem.ShortName := 'placekey';
  MetaDataItem.LongName := 'Place Keyword';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.2.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePlaceKeyword;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePlaceThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Place Keyword Thesaurus -- reference to a formally registered thesaurus or a similar authoritative source of place keywords.'
    + sLineBreak + 'See https://www2.usgs.gov/science/tab-term.html'
    + sLineBreak
    + '*Example authoritative sources for place keywords include Geographic '
    + 'Names Information System. ';
  MetaDataItem.ShortName := 'placekt';
  MetaDataItem.LongName := 'Place Keyword Thesaurus';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.2.1';
  MetaDataItem.Choices.Add('None');
  MetaDataItem.Choices.Add('Geographic Names Information System');
  MetaDataItem.Choice := MetaDataItem.Choices[0];
  MetaDataItem.TextHeight := 180;

  MetaDataItem.CreateMethod := CreatePlaceThesaurus;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePlanar(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Planar -- the quantities of distances, or distances and angles, which define the position of a point on a reference plane to which the surface of the Earth has been projected.';
  MetaDataItem.ShortName := 'planar';
  MetaDataItem.LongName := 'Planar';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2';
  MetaDataItem.TextHeight := 110;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'horizsys';

  MetaDataItem.CreateMethod := CreatePlanar;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateMapProjection(MetaDataItem);
  CreateGridCoordSystem(MetaDataItem);
  CreateLocalPlanar(MetaDataItem);
  CreatePlanarCoordInfo(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePlanarCoordEncodeMethod(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Planar Coordinate Encoding Method -- the means used to represent horizontal positions.';
  MetaDataItem.ShortName := 'plance';
  MetaDataItem.LongName := 'Planar Coordinate Encoding Method';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.1';

  MetaDataItem.Choices.Add('coordinate pair');
  MetaDataItem.Choices.Add('distance and bearing');
  MetaDataItem.Choices.Add('row and column');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreatePlanarCoordEncodeMethod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePlanarCoordInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Planar Coordinate Information -- information about the coordinate system developed on the planar surface.';
  MetaDataItem.ShortName := 'planci';
  MetaDataItem.LongName := 'Planar Coordinate Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4';

  MetaDataItem.CreateMethod := CreatePlanarCoordInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreatePlanarCoordEncodeMethod(MetaDataItem);
  CreateCoordRepresentation(MetaDataItem);
  CreateDistAndBearingRepresentation(MetaDataItem);
  CreatePlanarDistanceUnits(MetaDataItem);
end;

function TframeMetaDataEditor.CreatePlanarDistanceUnits(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Planar Distance Units -- units of measure used for distances.';
  MetaDataItem.ShortName := 'plandu';
  MetaDataItem.LongName := 'Planar Distance Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.4';

  MetaDataItem.Choices.Add('meters');
  MetaDataItem.Choices.Add('international feet');
  MetaDataItem.Choices.Add('survey feet');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreatePlanarDistanceUnits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePointAndVectorObjectCount(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Point and Vector Object Count -- the total number of the point or vector object type occurring in the data set.';
  MetaDataItem.ShortName := 'ptvctcnt';
  MetaDataItem.LongName := 'Point and Vector Object Count';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '3.3.1.2';
//  MetaDataItem.TextHeight := 155;

  MetaDataItem.MinValue := 1;
  MetaDataItem.Content := 1;

  MetaDataItem.CreateMethod := CreatePointAndVectorObjectCount;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePointAndVectorObjectInformation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Point and Vector Object Information -- the types and numbers of vector or nongridded point spatial objects in the data set.'
    + sLineBreak + 'Double-click radio button to deselect';
  MetaDataItem.ShortName := 'ptvctinf';
  MetaDataItem.LongName := 'Point and Vector Object Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '3.3';
  MetaDataItem.TextHeight := 110;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'spdoinfo';

  MetaDataItem.CreateMethod := CreatePointAndVectorObjectInformation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

//  AnItem.OnPaint := CheckSpatialRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;
//  MetaDataItem.RadioButton.OnClick := SpatialRadioButtonClicked;
  MetaDataItem.RadioButton.OnDblClick := OptionalRadioButtonDoubleClicked;

  CreateStdsTermsDescription(MetaDataItem);
  CreateVpfTermsDescription(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePointOfContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Point of Contact -- contact information for an individual or organization that is knowledgeable about the data set.'
    + sLineBreak
    + '(USGS Best Practice) It is preferred to use the programmatic contact '
    + 'information rather than a specific person. If a person must be listed '
    + 'as the contact, include the Contact Organization.'
    + sLineBreak
    + 'Example: “ask@usgs.gov”, “1 800 ASK USGS”';
  MetaDataItem.ShortName := 'ptcontac';
  MetaDataItem.LongName := 'Point of Contact';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.9';
  MetaDataItem.TextHeight := 180;

  MetaDataItem.CreateMethod := CreatePointOfContact;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  ChildItem := CreateContactInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePolarStereoGraphic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Polar Stereographic -- contains parameters for the Polar Stereographic projection.';
  MetaDataItem.ShortName := 'polarst';
  MetaDataItem.LongName := 'Polar Stereographic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.15';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreatePolarStereoGraphic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateStraightVertLineFromPole(MetaDataItem);

  MChild := CreateStandardParallel(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'polarst';
  MChild.Required := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

//  MChild := CreateLatProjOrigin(MetaDataItem);
  MChild := CreateScaleFactOfProjOrigin(MetaDataItem);
  MChild.CheckType := ctRadioButton;
  MChild.RadioGroupName := 'polarst';
  MChild.Required := False;
  MChild.Used := False;
  AssignMetaDataToTreeViewItem(MChild);
  MChild.TreeViewItem.OnPaint := CheckRadioButton;
  MChild.OnUsedChanged := RadioButtonChanged;

  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePolyconic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Polyconic -- contains parameters for the Polyconic projection.';
  MetaDataItem.ShortName := 'polycon';
  MetaDataItem.LongName := 'Polyconic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.16';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreatePolyconic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePositionalAccuracy(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Positional Accuracy -- an assessment of the accuracy of the positions of spatial objects.';
  MetaDataItem.ShortName := 'posacc';
  MetaDataItem.LongName := 'Positional Accuracy';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.4';

  MetaDataItem.CreateMethod := CreatePositionalAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateHorizPositionalAccuracy(MetaDataItem);
  CreateVertPositionalAccuracy(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePostalCode(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Postal Code -- the ZIP or other postal code of the address.'
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "20192".';
  MetaDataItem.ShortName := 'postal';
  MetaDataItem.LongName := 'Postal Code';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.4.5';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreatePostalCode;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProcessContact(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Process Contact -- the party responsible for the processing step information.';
  MetaDataItem.ShortName := 'proccont';
  MetaDataItem.LongName := 'Process Contact';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.5.2.6';

  MetaDataItem.CreateMethod := CreateProcessContact;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  ChildItem := CreateContactInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProcessDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Process Date -- the date when the event was completed.';
  MetaDataItem.ShortName := 'procdate';
  MetaDataItem.LongName := 'Process Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.2.3';

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choices.Add('Not complete');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateProcessDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProcessDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Process Description -- an explanation of the event and related parameters or tolerances.';
  MetaDataItem.ShortName := 'procdesc';
  MetaDataItem.LongName := 'Process Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.2.1';

  MetaDataItem.CreateMethod := CreateProcessDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProcessStep(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Process Step -- information about a single event.';
  MetaDataItem.ShortName := 'procstep';
  MetaDataItem.LongName := 'Process Step';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateProcessStep;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateProcessDescription(MetaDataItem);
  CreateSourceUsedCiteAbbr(MetaDataItem);
  CreateProcessDate(MetaDataItem);
  CreateProcessTime(MetaDataItem);
  CreateSourceProducedCiteAbbr(MetaDataItem);
  CreateProcessContact(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProcessTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTimeMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTimeMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Process Time -- the time when the event was completed.';
  MetaDataItem.ShortName := 'proctime';
  MetaDataItem.LongName := 'Process Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.5.2.4';

  MetaDataItem.CreateMethod := CreateProcessTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateProfileName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Profile Name -- the name given to a document that describes the application of the Standard to a specific user community.';
  MetaDataItem.ShortName := 'metprof';
  MetaDataItem.LongName := 'Profile Name';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '7.11.2';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateProfileName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateProgress(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Progress -- the state of the data set.';
  MetaDataItem.ShortName := 'progress';
  MetaDataItem.LongName := 'Progress';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.4.1';
  MetaDataItem.Choices.Add('Complete');
  MetaDataItem.Choices.Add('In work');
  MetaDataItem.Choices.Add('Planned');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateProgress;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateIssueID(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Issue Identification -- information identifying the issue of the series publication of which the data set is a part.';
  MetaDataItem.ShortName := 'issue';
  MetaDataItem.LongName := 'Issue Identification';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.7.2';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateIssueID;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreatePubInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Publication Information -- publication details for published data sets.';
  MetaDataItem.ShortName := 'pubinfo';
  MetaDataItem.LongName := 'Publication Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.8';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePubInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreatePubPlace(MetaDataItem);
  CreatePublisher(MetaDataItem);
end;

function TframeMetaDataEditor.CreatePublicationDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description :=
    'Publication Date -- the date when the data set is published or otherwise made available for release.'
    + sLineBreak
    + '(USGS Best Practice) The date the system is first released in final '
    + 'publication form.';
  MetaDataItem.ShortName := 'pubdate';
  MetaDataItem.LongName := 'Publication Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.2';
  MetaDataItem.TextHeight := 120;

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choices.Add('Unpublished material');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreatePublicationDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreatePublicationTime(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusTimeDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusTimeDataItem.Create(AnItem);
  MetaDataItem.Description := 'Publication Time -- the time of day when the data set is published or otherwise made available for release.';
  MetaDataItem.ShortName := 'pubtime';
  MetaDataItem.LongName := 'Publication Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '8.3';

  MetaDataItem.Choices.Add('Unknown');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePublicationTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreatePublisher(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Publisher -- the name of the individual or organization that published the data set.'
    + sLineBreak
    + '(USGS Best Practice) Use "U.S. Geological Survey" ';
  MetaDataItem.ShortName := 'publish';
  MetaDataItem.LongName := 'Publisher';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.8.2';
  MetaDataItem.TextHeight := 120;

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePublisher;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreatePubPlace(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Publication Place -- the name of the city (and state or province, and country, if needed to identify the city) where the data set was published or released.';
  MetaDataItem.ShortName := 'pubplace';
  MetaDataItem.LongName := 'Publication Place';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.8.1';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreatePubPlace;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreatePurpose(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Purpose -- a summary of the intentions with which the data set was developed. ';
  MetaDataItem.ShortName := 'purpose';
  MetaDataItem.LongName := 'Purpose';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.2.2';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreatePurpose;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateQuantitativeAttributeAccuracyAssessment(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Quantitative Attribute Accuracy Assessment -- a value assigned to summarize the accuracy'
    + ' of the identification of the entities and assignments of values in the data set and the'
    + ' identification of the test that yielded the value.';
  MetaDataItem.ShortName := 'qattracc';
  MetaDataItem.LongName := 'Quantitative Attribute Accuracy Assessment';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.1.2';
  MetaDataItem.TextHeight := 130;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateQuantitativeAttributeAccuracyAssessment;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateAttributeAccuracyValue(MetaDataItem);
  CreateAttributeAccuracyExplanation(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateQuantitativeHorizPositionalAccuracy(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Quantitative Horizontal Positional Accuracy Assessment -- numeric value assigned to'
    + ' summarize the accuracy of the horizontal coordinate measurements and the'
    + ' identification of the test that yielded the value.';
  MetaDataItem.ShortName := 'qhorizpa';
  MetaDataItem.LongName := 'Quantitative Horizontal Positional Accuracy Assessment';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.4.1.2';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 140;

  MetaDataItem.CreateMethod := CreateQuantitativeHorizPositionalAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateHorizPositionalAccuracyValue(MetaDataItem);
  CreateHorizPositionalAccuracyExplanation(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateQuantitativeVertPositionalAccuracy(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Quantitative Vertical Positional Accuracy Assessment -- numeric value assigned to'
    + ' summarize the accuracy of vertical coordinate measurements and the identification of'
    + ' the test that yielded the value.';
  MetaDataItem.ShortName := 'qvertpa';
  MetaDataItem.LongName := 'Quantitative Vertical Positional Accuracy Assessment';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '2.4.2.2';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 130;

  MetaDataItem.CreateMethod := CreateQuantitativeVertPositionalAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateVertPositionalAccuracyValue(MetaDataItem);
  CreateVertPositionalAccuracyExplanation(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRangeDomain(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Range Domain -- the minimum and maximum values of a continuum of valid values.';
  MetaDataItem.ShortName := 'rdom';
  MetaDataItem.LongName := 'Range Domain';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.2';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateRangeDomain;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateRangeDomainMin(MetaDataItem);
  CreateRangeDomainMax(MetaDataItem);
  CreateAttributeUnitsOfMeasure(MetaDataItem);
  CreateAttributeMeasureRes(MetaDataItem);
  ChildItem := CreateAttribute(MetaDataItem);
  ChildItem.RequiredType := rtManditoryIfApplicable;
end;

function TframeMetaDataEditor.CreateRangeDomainMax(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Range Domain Maximum -- the greatest value that the attribute can be assigned.';
  MetaDataItem.ShortName := 'rdommax';
  MetaDataItem.LongName := 'Range Domain Maximum';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.2.2';

  MetaDataItem.CreateMethod := CreateRangeDomainMax;
  MetaDataItem.MoreThanOneAllowed := False;

  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateRangeDomainMin(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Range Domain Minimum -- the least value that the attribute can be assigned.';
  MetaDataItem.ShortName := 'rdommin';
  MetaDataItem.LongName := 'Range Domain Minimum';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.2.1';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateRangeDomainMin;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateRangeOfTimes(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Range of Dates/Times -- means of encoding a range of dates and times.';
  MetaDataItem.ShortName := 'rngdates';
  MetaDataItem.LongName := 'Range of Dates/Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.3';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'timeinfo';

  MetaDataItem.CreateMethod := CreateRangeOfTimes;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateBeginDate(MetaDataItem);
  CreateBeginningTime(MetaDataItem);
  CreateEndDate(MetaDataItem);
  CreateEndTime(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRasterColumnCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Column Count -- the maximum number of raster objects along the abscissa (x) axis. For use with rectangular raster objects.';
  MetaDataItem.ShortName := 'colcount';
  MetaDataItem.LongName := 'Column Count';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.4.3';

  MetaDataItem.MinValue := 1;
  MetaDataItem.Content := 1;

  MetaDataItem.CreateMethod := CreateRasterColumnCount;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRasterInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Raster Object Information -- the types and numbers of raster spatial objects in the data set.'
    + sLineBreak + 'Double-click radio button to deselect';
  MetaDataItem.ShortName := 'rastinfo';
  MetaDataItem.LongName := 'Raster Object Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '3.4';
  MetaDataItem.TextHeight := 90;
//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'spdoinfo';

  MetaDataItem.CreateMethod := CreateRasterInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);
//  MetaDataItem.RadioButton.OnClick := SpatialRadioButtonClicked;
  MetaDataItem.RadioButton.OnDblClick := OptionalRadioButtonDoubleClicked;

//  AnItem.OnPaint := CheckSpatialRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateRasterObjectType(MetaDataItem);
  CreateRasterRowCount(MetaDataItem);
  CreateRasterColumnCount(MetaDataItem);
  CreateRasterVerticalCount(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRasterObjectType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Raster Object Type -- raster spatial objects used to locate zero-, two-, or three-dimensional locations in the data set.';
  MetaDataItem.ShortName := 'rasttype';
  MetaDataItem.LongName := 'Raster Object Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.4.1';

  MetaDataItem.Choices.Add('Point');
  MetaDataItem.Choices.Add('Pixel');
  MetaDataItem.Choices.Add('Grid Cell');
  MetaDataItem.Choices.Add('Voxel');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateRasterObjectType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRasterRowCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Row Count -- the maximum number of raster objects along the ordinate (y) axis. For use with rectangular raster objects.';
  MetaDataItem.ShortName := 'rowcount';
  MetaDataItem.LongName := 'Row Count';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.4.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MinValue := 1;
  MetaDataItem.Content := 1;

  MetaDataItem.CreateMethod := CreateRasterRowCount;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRasterVerticalCount(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Count -- the maximum number of raster objects along the vertical (z) axis. For use with rectangular volumetric raster objects (voxels).';
  MetaDataItem.ShortName := 'vrtcount';
  MetaDataItem.LongName := 'Vertical Count';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '3.4.4';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MinValue := 1;
  MetaDataItem.Content := 1;

  MetaDataItem.CreateMethod := CreateRasterVerticalCount;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateRecordingCapacity(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Recording Capacity -- the density of information to which data are written. Used in cases where different recording capacities are possible.';
  MetaDataItem.ShortName := 'reccap';
  MetaDataItem.LongName := 'Recording Capacity';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4.2.2.2.2';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateRecordingCapacity;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateRecordingDensity(MetaDataItem);
  CreateRecordingDensityUnits(MetaDataItem);
end;

function TframeMetaDataEditor.CreateRecordingDensity(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Recording Density -- the density in which the data set can be recorded.';
  MetaDataItem.ShortName := 'recden';
  MetaDataItem.LongName := 'Recording Density';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.2.2.1';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateRecordingDensity;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateRecordingDensityUnits(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Recording Density Units -- the units of measure for the recording density.';
  MetaDataItem.ShortName := 'recdenu';
  MetaDataItem.LongName := 'Recording Density Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.2.2.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateRecordingDensityUnits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateRecordingFormat(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Recording Format -- the options available or method used to write the data set to the medium.';
  MetaDataItem.ShortName := 'recfmt';
  MetaDataItem.LongName := 'Recording Format';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.2.3';

  MetaDataItem.Choices.Add('cpio');
  MetaDataItem.Choices.Add('tar');
  MetaDataItem.Choices.Add('High Sierra');
  MetaDataItem.Choices.Add('ISO 9660');
  MetaDataItem.Choices.Add('ISO 9660 with Rock Ridge extensions');
  MetaDataItem.Choices.Add('ISO 9660 with Apple HFS extensions');

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateRecordingFormat;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateReferenceDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Date -- the date that the metadata were created or last updated. '
    + sLineBreak
    + '(USGS Best Practice) Date the metadata record was created. Revise the '
    + 'date to indicate a new version. This is important for catalogs to '
    + 'ascertain if metadata has changed.';
  MetaDataItem.ShortName := 'metd';
  MetaDataItem.LongName := 'Metadata Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.1';
  MetaDataItem.TextHeight := 140;

  MetaDataItem.CreateMethod := CreateReferenceDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  FReviewItem := MetaDataItem;

end;

function TframeMetaDataEditor.CreateReferenceInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Reference Information -- information on the currentness of the metadata information, and the responsible party. ';
  MetaDataItem.ShortName := 'metainfo';
  MetaDataItem.LongName := 'Metadata Reference Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateReferenceInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateReferenceDate(MetaDataItem);
  CreateMetaDataReviewDate(MetaDataItem);
  CreateMetaDataFutureReviewDate(MetaDataItem);
  CreateContact(MetaDataItem);
  CreateStandardName(MetaDataItem);
  CreateStandardVersion(MetaDataItem);
  CreateMetaTimeConvention(MetaDataItem);
  CreateMetaAccessConstraints(MetaDataItem);
  CreateMetaUseConstraints(MetaDataItem);
  CreateMetaSecurityInfo(MetaDataItem);
  CreateMetaExtensions(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateResourceDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Resource Description -- the identifier by which the distributor knows the data set.';
  MetaDataItem.ShortName := 'resdesc';
  MetaDataItem.LongName := 'Resource Description';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateResourceDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateRobinson(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Robinson -- contains parameters for the Robinson projection.';
  MetaDataItem.ShortName := 'robinson';
  MetaDataItem.LongName := 'Robinson';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.17';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateRobinson;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateScaleFactOfProjOrigin(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Scale Factor at Projection Origin -- a multiplier for reducing a distance obtained from a map by computation or scaling to the actual distance at the projection origin.';
  MetaDataItem.ShortName := 'sfprjorg';
  MetaDataItem.LongName := 'Scale Factor at Projection Origin';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.14';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateScaleFactOfProjOrigin;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateScaleFactorAtCenterLine(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Scale Factor at Center Line -- a multiplier for reducing a distance obtained from a map by computation or scaling to the actual distance along the center line.';
  MetaDataItem.ShortName := 'sfctrlin';
  MetaDataItem.LongName := 'Scale Factor at Center Line';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.10';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateScaleFactorAtCenterLine;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateScaleFactorAtCentMerid(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Scale Factor at Central Meridian -- a multiplier for reducing a distance obtained from a map by computation or scaling to the actual distance along the central meridian.';
  MetaDataItem.ShortName := 'sfctrmer';
  MetaDataItem.LongName := 'Scale Factor at Central Meridian';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.17';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateScaleFactorAtCentMerid;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateScaleFactorAtEquator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Scale Factor at Equator -- a multiplier for reducing a distance obtained from a map by computation or scaling to the actual distance along the equator.';
  MetaDataItem.ShortName := 'sfequat';
  MetaDataItem.LongName := 'Scale Factor at Equator';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.6';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateScaleFactorAtEquator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateSDTSPointAndVectorObjectType(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'SDTS Point and Vector Object Type -- name of point and vector spatial objects used to locate zero-, one-, and two-dimensional spatial locations in the data set.';
  MetaDataItem.ShortName := 'sdtstype';
  MetaDataItem.LongName := 'SDTS Point and Vector Object Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.1.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.Choices.Add('Point');
  MetaDataItem.Choices.Add('Entity point');
  MetaDataItem.Choices.Add('Label point');
  MetaDataItem.Choices.Add('Area point');
  MetaDataItem.Choices.Add('Node, planar graph');
  MetaDataItem.Choices.Add('Node, network');
  MetaDataItem.Choices.Add('String');
  MetaDataItem.Choices.Add('Link');
  MetaDataItem.Choices.Add('Complete chain');
  MetaDataItem.Choices.Add('Area chain');
  MetaDataItem.Choices.Add('Network chain, planar graph');
  MetaDataItem.Choices.Add('Network chain, nonplanar graph');
  MetaDataItem.Choices.Add('Circular arc, three point center');
  MetaDataItem.Choices.Add('Elliptical arc');
  MetaDataItem.Choices.Add('Uniform B-spline');
  MetaDataItem.Choices.Add('Piecewise Bezier');
  MetaDataItem.Choices.Add('Ring with mixed composition');
  MetaDataItem.Choices.Add('Ring composed of strings');
  MetaDataItem.Choices.Add('Ring composed of chains');
  MetaDataItem.Choices.Add('Ring composed of arcs');
  MetaDataItem.Choices.Add('G-polygon');
  MetaDataItem.Choices.Add('GT-polygon composed of rings');
  MetaDataItem.Choices.Add('GT-polygon composed of chains');
  MetaDataItem.Choices.Add('Universe polygon composed of rings');
  MetaDataItem.Choices.Add('Universe polygon composed of chains');
  MetaDataItem.Choices.Add('Void polygon composed of rings');
  MetaDataItem.Choices.Add('Void polygon composed of chains');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateSDTSPointAndVectorObjectType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSecurityClassification(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Security Classification -- name of the handling restrictions on the data set.';
  MetaDataItem.ShortName := 'secclass';
  MetaDataItem.LongName := 'Security Classification';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.12.2';
  MetaDataItem.Choices.Add('Top secret');
  MetaDataItem.Choices.Add('Secret');
  MetaDataItem.Choices.Add('Confidential');
  MetaDataItem.Choices.Add('Restricted');
  MetaDataItem.Choices.Add('Unclassified');
  MetaDataItem.Choices.Add('Sensitive');
  MetaDataItem.Choice := MetaDataItem.Choices[4];

  MetaDataItem.CreateMethod := CreateSecurityClassification;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSecurityHandling(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Security Handling Description -- additional information about the restrictions on handling the data set.';
  MetaDataItem.ShortName := 'sechandl';
  MetaDataItem.LongName := 'Security Handling Description';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.12.3';

  MetaDataItem.CreateMethod := CreateSecurityHandling;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSecurityInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Security Information -- handling restrictions imposed on the data set because of national security, privacy, or other concerns.';
  MetaDataItem.ShortName := 'secinfo';
  MetaDataItem.LongName := 'Security Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.12';

  MetaDataItem.CreateMethod := CreateSecurityInformation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateSecuritySystem(MetaDataItem);
  CreateSecurityClassification(MetaDataItem);
  CreateSecurityHandling(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSecuritySystem(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Security Classification System -- name of the classification system.';
  MetaDataItem.ShortName := 'secsys';
  MetaDataItem.LongName := 'Security Classification System';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.12.1';

  MetaDataItem.CreateMethod := CreateSecuritySystem;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSemiMajorAxis(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Semi-major Axis -- radius of the equatorial axis of the ellipsoid.';
  MetaDataItem.ShortName := 'semiaxis';
  MetaDataItem.LongName := 'Semi-major Axis';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.4.3';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateSemiMajorAxis;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateSeriesInformation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Series Information -- the identification of the series publication of which the data set is a part.';
  MetaDataItem.ShortName := 'serinfo';
  MetaDataItem.LongName := 'Series Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.7';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateSeriesInformation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateSeriesName(MetaDataItem);
  CreateIssueID(MetaDataItem);
end;

function TframeMetaDataEditor.CreateSeriesName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Series Name -- the name of the series publication of which the data set is a part.';
  MetaDataItem.ShortName := 'sername';
  MetaDataItem.LongName := 'Series Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.7.1';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateSeriesName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateSingleDateTime(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Single Date/Time -- means of encoding a single date and time. ';
  MetaDataItem.ShortName := 'sngdate';
  MetaDataItem.LongName := 'Single Date/Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.1';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'timeinfo';

  MetaDataItem.CreateMethod := CreateSingleDateTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  Result := MetaDataItem;

  CreateCalendarDateTime(MetaDataItem);
  CreateTimeOfDay(MetaDataItem);
end;

function TframeMetaDataEditor.CreateSinusoidal(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Sinusoidal -- contains parameters for the Sinusoidal projection.';
  MetaDataItem.ShortName := 'sinusoid';
  MetaDataItem.LongName := 'Sinusoidal';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.18';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateSinusoidal;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceCiteAbbr(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Citation Abbreviation -- short-form alias for the source citation.';
  MetaDataItem.ShortName := 'srccitea';
  MetaDataItem.LongName := 'Source Citation Abbreviation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.5';

  MetaDataItem.CreateMethod := CreateSourceCiteAbbr;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceCiteInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Citation -- reference for a source data set.';
  MetaDataItem.ShortName := 'srccite';
  MetaDataItem.LongName := 'Source Citation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.1';

  MetaDataItem.CreateMethod := CreateSourceCiteInformation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  ChildItem := CreateCiteInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceContribution(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Contribution -- brief statement identifying the information contributed by the source to the data set.';
  MetaDataItem.ShortName := 'srccontr';
  MetaDataItem.LongName := 'Source Contribution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.6';

  MetaDataItem.CreateMethod := CreateSourceContribution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceCurrentness(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Currentness Reference -- the basis on which the source time period of content information of the source data set is determined.';
  MetaDataItem.ShortName := 'srccurr';
  MetaDataItem.LongName := 'Source Currentness Reference';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.4.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Choices.Add('ground condition');
  MetaDataItem.Choices.Add('publication date');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateSourceCurrentness;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceInformation(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Information -- list of sources and a short discussion of the information contributed by each.';
  MetaDataItem.ShortName := 'srcinfo';
  MetaDataItem.LongName := 'Source Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.5.1';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateSourceInformation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateSourceCiteInformation(MetaDataItem);
  CreateSourceScaleDenominator(MetaDataItem);
  CreateTypeOfSourceMedia(MetaDataItem);
  CreateSourceTimePeriod(MetaDataItem);
  CreateSourceCiteAbbr(MetaDataItem);
  CreateSourceContribution(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceProducedCiteAbbr(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Produced Citation Abbreviation -- the Source Citation Abbreviation of an intermediate data set that (1)'
    + ' is significant in the opinion of the data producer, (2) is generated in the processing step, and (3) is used in'
    + ' later processing steps.';
  MetaDataItem.ShortName := 'srcprod';
  MetaDataItem.LongName := 'Source Produced Citation Abbreviation';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.5.2.5';
  MetaDataItem.TextHeight := 135;
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.OnUpdateChoices := UpdateCiteAbbreviations;;

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateSourceProducedCiteAbbr;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceScaleDenominator(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Scale Denominator -- the denominator of the representative fraction on a map (for example, on a 1:24,000-scale map, the Source Scale Denominator is 24000).';
  MetaDataItem.ShortName := 'srcscale';
  MetaDataItem.LongName := 'Source Scale Denominator';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.5.1.2';
  MetaDataItem.TextHeight := 100;
  MetaDataItem.MinValue := 2;
  MetaDataItem.Content := 24000;

  MetaDataItem.CreateMethod := CreateSourceScaleDenominator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceTimePeriod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Time Period of Content -- time period(s) for which the source data set corresponds to the ground.';
  MetaDataItem.ShortName := 'srctime';
  MetaDataItem.LongName := 'Source Time Period of Content';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.4';

  MetaDataItem.CreateMethod := CreateSourceTimePeriod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  ChildItem := CreateTimePeriodInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
  CreateSourceCurrentness(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSourceUsedCiteAbbr(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Source Used Citation Abbreviation -- the Source Citation Abbreviation of a data set used in the processing step.';
  MetaDataItem.ShortName := 'srcused';
  MetaDataItem.LongName := 'Source Used Citation Abbreviation';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.5.2.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.OnUpdateChoices := UpdateCiteAbbreviations;;

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateSourceUsedCiteAbbr;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSpaceObliqueMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Space Oblique Mercator (Landsat) -- contains parameters for the Space Oblique Mercator (Landsat) projection.';
  MetaDataItem.ShortName := 'spaceobq';
  MetaDataItem.LongName := 'Space Oblique Mercator (Landsat)';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.19';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateSpaceObliqueMercator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLandsatNumber(MetaDataItem);
  CreateLandsatPathNumber(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSpatialDataOrgInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Spatial Data Organization Information -- the mechanism used to represent spatial information in the data set.';
  MetaDataItem.ShortName := 'spdoinfo';
  MetaDataItem.LongName := 'Spatial Data Organization Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '3';

  MetaDataItem.CreateMethod := CreateSpatialDataOrgInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateIndirectSpatialReference(MetaDataItem);
  CreateDirectSpatialReferenceMethod(MetaDataItem);
  CreatePointAndVectorObjectInformation(MetaDataItem);
  CreateRasterInfo(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSpatialDomain(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Spatial Domain - the geographic areal domain of the data set.'
    + sLineBreak
    + '(USGS Best Practice) The limits of coverage of the system’s data '
    + 'expressed by latitude and longitude in the form of a bounding box '
    + '(West, East, North, South).';
  MetaDataItem.ShortName := 'spdom';
  MetaDataItem.LongName := 'Spatial Domain';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.5';
  MetaDataItem.TextHeight := 130;

  MetaDataItem.CreateMethod := CreateSpatialDomain;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateBoundingCoordinates(MetaDataItem);
  CreateGPolygon(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSpatialReferenceInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Spatial Reference Information -- the description of the reference frame for, and the means to encode, coordinates in the data set.';
  MetaDataItem.ShortName := 'spref';
  MetaDataItem.LongName := 'Spatial Reference Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4';

  MetaDataItem.CreateMethod := CreateSpatialReferenceInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateHorizontalCoordSysDef(MetaDataItem);
  CreateVertCoordSysDef(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStandardName(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Metadata Standard Name -- the name of the metadata standard used to document the data set. ';
  MetaDataItem.ShortName := 'metstdn';
  MetaDataItem.LongName := 'Metadata Standard Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.5';
  MetaDataItem.Choices.Add('FGDC Content Standard for Digital Geospatial Metadata');
  MetaDataItem.Choices.Add('Content Standard for Digital Geospatial Metadata - Extensions for Remote Sensing Metadata');
  MetaDataItem.Choices.Add('FGDC Biological Data Profile of the Content Standard for Digital Geospatial Metadata');
  MetaDataItem.Choices.Add('Metadata Profile for Shoreline Data');

  MetaDataItem.Choice := MetaDataItem.Choices[0];

  MetaDataItem.CreateMethod := CreateStandardName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStandardOrderProcess(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Standard Order Process -- the common ways in which the data set may be obtained or received, and related instructions and fee information.'
    + sLineBreak
    + '(USGS Best Practice) This section should be used for web services that '
    + 'provide multiple access to the data of systems.';
  MetaDataItem.ShortName := 'stdorder';
  MetaDataItem.LongName := 'Standard Order Process';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4';
  MetaDataItem.TextHeight := 160;

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateStandardOrderProcess;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateNonDigitalForm(MetaDataItem);
  CreateDigitalForm(MetaDataItem);
  CreateFees(MetaDataItem);
  CreateOrderingInstructions(MetaDataItem);
  CreateTurnAround(MetaDataItem);
end;

function TframeMetaDataEditor.CreateStandardParallel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Standard Parallel -- line of constant latitude at which the surface of the Earth and the plane or developable surface intersect.';
  MetaDataItem.ShortName := 'stdparll';
  MetaDataItem.LongName := 'Standard Parallel';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.1';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 90;
  MetaDataItem.MinValue := -90;

  MetaDataItem.CreateMethod := CreateStandardParallel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateStandardVersion(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Metadata Standard Version -- identification of the version of the metadata standard used to document the data set. ';
  MetaDataItem.ShortName := 'metstdv';
  MetaDataItem.LongName := 'Metadata Standard Version';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '7.6';

  MetaDataItem.Choices.Add('FGDC-STD-001-1998');
  MetaDataItem.Choices.Add('FGDC-STD-001.1-1999');
  MetaDataItem.Choices.Add('FGDC-STD-001.2-2001');
  MetaDataItem.Choices.Add('FGDC-STD-012-2002');

  MetaDataItem.Choice := MetaDataItem.Choices[0];

  MetaDataItem.CreateMethod := CreateStandardVersion;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTreeViewItem(Parent: TCustomMetaDataItem): TTreeViewItem;
begin
  result := TTreeViewItem.Create(self);
  result.Parent := Parent.TreeViewItem;
  result.Visible := True;
  result.OnMouseDown := DuplicateItem;
end;

function TframeMetaDataEditor.CreateTurnAround(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Turnaround -- typical turnaround time for the filling of an order.';
  MetaDataItem.ShortName := 'turnarnd';
  MetaDataItem.LongName := 'Turnaround';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.5';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateTurnAround;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateTypeOfSourceMedia(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Type of Source Media -- the medium of the source data set.';
  MetaDataItem.ShortName := 'typesrc';
  MetaDataItem.LongName := 'Type of Source Media';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.5.1.3';

  MetaDataItem.Choices.Add('paper');
  MetaDataItem.Choices.Add('stable-base material');
  MetaDataItem.Choices.Add('microfiche');
  MetaDataItem.Choices.Add('microfilm');
  MetaDataItem.Choices.Add('audiocassette');
  MetaDataItem.Choices.Add('chart');
  MetaDataItem.Choices.Add('filmstrip');
  MetaDataItem.Choices.Add('transparency');
  MetaDataItem.Choices.Add('videocassette');
  MetaDataItem.Choices.Add('videodisc');
  MetaDataItem.Choices.Add('videotape');
  MetaDataItem.Choices.Add('physical model');
  MetaDataItem.Choices.Add('computer program');
  MetaDataItem.Choices.Add('disc');
  MetaDataItem.Choices.Add('cartridge tape');
  MetaDataItem.Choices.Add('magnetic tape');
  MetaDataItem.Choices.Add('online');
  MetaDataItem.Choices.Add('CD-ROM');
  MetaDataItem.Choices.Add('electronic bulletin board');
  MetaDataItem.Choices.Add('electronic mail system');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateTypeOfSourceMedia;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

procedure TframeMetaDataEditor.AssignMetaDataToTreeViewItem(
  MetaDataItem: TCustomMetaDataItem);
var
  AnItem: TTreeViewItem;
begin
  Assert(Assigned(MetaDataItem.CreateMethod));
  AnItem := MetaDataItem.TreeViewItem;
  AnItem.TagObject := MetaDataItem;
  AnItem.Text := '      ' + MetaDataItem.ID + ': ' + MetaDataItem.LongName;
  AnItem.IsChecked := MetaDataItem.Used;
  MetaDataItem.ApplyCheckType;
  if MetaDataItem.MoreThanOneAllowed then
  begin
    AnItem.ImageIndex := 0;
  end
  else
  begin
    AnItem.ImageIndex := -1;
  end;
  if MetaDataItem.Required then
  begin
    if MetaDataItem.CheckBox <> nil then
    begin
      MetaDataItem.CheckBox.Enabled := False;
    end;
  end
  else
  begin
    if MetaDataItem.CheckBox <> nil then
    begin
      MetaDataItem.CheckBox.Enabled := True;
    end;
  end;

  if MetaDataItem is TNumericMetaDataItem then
  begin
    AnItem.OnPaint := NumberItemPaint;
  end
  else if MetaDataItem is TExtentDataItem then
  begin
    AnItem.OnPaint := ExtentItemPaint;
  end
  else if MetaDataItem is TPolygonMetaDataItem then
  begin
    AnItem.OnPaint := PolygonItemPaint;
  end
  else if not (MetaDataItem is TCompoundItemMetaDataItem) then
  begin
    AnItem.OnPaint := TextItemPaint;
  end
  else
  begin
    AnItem.OnPaint := FixVisible;
  end;

  MetaDataItem.OnChange := ContentChanged;
end;

procedure TframeMetaDataEditor.AddAttributeChildren(MetaDataItem: TCustomMetaDataItem);
begin
  CreateAttributeLabel(MetaDataItem);
  CreateAttributeDefinition(MetaDataItem);
  CreateAttributeDefinitionSource(MetaDataItem);
  CreateAttributeDomainValue(MetaDataItem);
  CreateAttBeginDate(MetaDataItem);
  CreateAttEndDate(MetaDataItem);
  CreateAttValueAccuracyInfo(MetaDataItem);
  CreateAttMeasurementFrequency(MetaDataItem);
end;

function TframeMetaDataEditor.CreateStateOrProvince(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'State or Province -- the state or province of the address.'
    + sLineBreak
    + 'For Metadata Reference Information about USGS data sets, this must be "VA".';
  MetaDataItem.ShortName := 'state';
  MetaDataItem.LongName := 'State or Province';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '10.4.4';
  MetaDataItem.TextHeight := 105;

  MetaDataItem.CreateMethod := CreateStateOrProvince;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStatePlaneCoordSys(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'State Plane Coordinate System (SPCS) -- a plane-rectangular coordinate system established for each state in the United States by the National Geodetic Survey.';
  MetaDataItem.ShortName := 'spcs';
  MetaDataItem.LongName := 'State Plane Coordinate System (SPCS)';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.4';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateStatePlaneCoordSys;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateStatePlaneCoordSysZoneID(MetaDataItem);

  MChild := CreateLambertConformalConic(MetaDataItem);
  MChild.RadioGroupName := 'spcs';
  AssignMetaDataToTreeViewItem(MChild);

  MChild := CreateTransverseMercator(MetaDataItem);
  MChild.RadioGroupName := 'spcs';
  AssignMetaDataToTreeViewItem(MChild);

  MChild := CreateObliqueMercator(MetaDataItem);
  MChild.RadioGroupName := 'spcs';
  AssignMetaDataToTreeViewItem(MChild);

  MChild := CreatePolyconic(MetaDataItem);
  MChild.RadioGroupName := 'spcs';
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateStatePlaneCoordSysZoneID(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'SPCS Zone Identifier -- identifier for the SPCS zone. '
    + 'Four-digit numeric codes for the State Plane Coordinate Systems based on the North'
    + ' American Datum of 1927 are found in Department of Commerce, 1986, Representation of'
    + ' geographic point locations for information interchange (Federal Information Processing Standard'
    + ' 70-1): Washington: Department of Commerce, National Institute of Standards and Technology.'
    + ' Codes for the State Plane Coordinate Systems based on the North American Datum of 1983 are'
    + ' found in Department of Commerce, 1989 (January), State Plane Coordinate System of 1983'
    + ' (National Oceanic and Atmospheric Administration Manual NOS NGS 5): Silver Spring, Maryland,'
    + ' National Oceanic and Atmospheric Administration, National Ocean Service, Coast and Geodetic'
    + ' Survey.';
  MetaDataItem.ShortName := 'spcszone';
  MetaDataItem.LongName := 'SPCS Zone Identifier';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.4.1';
  MetaDataItem.TextHeight := 430;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateStatePlaneCoordSysZoneID;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateStatus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Status -- the state of and maintenance information for the data set.';
  MetaDataItem.ShortName := 'status';
  MetaDataItem.LongName := 'Status';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.4';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateStatus;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateProgress(MetaDataItem);
  CreateUpdateFrequency(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStraightVertLineFromPole(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Straight Vertical Longitude from Pole -- longitude to be oriented straight up from the North or South Pole.';
  MetaDataItem.ShortName := 'svlong';
  MetaDataItem.LongName := 'Straight Vertical Longitude from Pole';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.13';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := -180;
  MetaDataItem.MaxValue := 180;
  MetaDataItem.MaxLimitType := mltLess;

  MetaDataItem.CreateMethod := CreateStraightVertLineFromPole;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateTransferSize(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Transfer Size -- the size, or estimated size, of the transferred data set in megabytes.';
  MetaDataItem.ShortName := 'transize';
  MetaDataItem.LongName := 'Transfer Size';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.2.1.7';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateTransferSize;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateTransverseMercator(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Transverse Mercator -- contains parameters for theTransverse mercator projection.';
  MetaDataItem.ShortName := 'transmer';
  MetaDataItem.LongName := 'Transverse Mercator';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.21';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateTransverseMercator;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateScaleFactorAtCentMerid(MetaDataItem);
  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateStratum(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Stratum -- layered, vertical locations characterized by the data set.';
  MetaDataItem.ShortName := 'stratum';
  MetaDataItem.LongName := 'Stratum';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '1.6.3';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateStratum;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateStratumThesaurus(MetaDataItem);
  CreateStratumKeywords(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStratumKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Stratum Keyword -- the name of a vertical location used to describe the locations covered by a data set.';
  MetaDataItem.ShortName := 'stratkey';
  MetaDataItem.LongName := 'Stratum Keyword';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.3.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateStratumKeywords;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStratumThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Stratum Keyword Thesaurus -- reference to a formally registered thesaurus or a similar authoritative source of stratum keywords.'
    + sLineBreak + 'See https://www2.usgs.gov/science/tab-term.html';
  MetaDataItem.ShortName := 'stratkt';
  MetaDataItem.LongName := 'Stratum Keyword Thesaurus';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.3.1';
  MetaDataItem.Choices.Add('None');
  MetaDataItem.Choice := MetaDataItem.Choices[0];
  MetaDataItem.TextHeight := 130;

  MetaDataItem.CreateMethod := CreateStratumThesaurus;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateSupplimentalInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Supplemental Information -- other descriptive information about the data set.';
  MetaDataItem.ShortName := 'supplinf';
  MetaDataItem.LongName := 'Supplemental Information';
  MetaDataItem.Required := False;
  MetaDataItem.ID := '1.2.3';
  MetaDataItem.RequiredType := rtOptional;

  MetaDataItem.CreateMethod := CreateSupplimentalInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTechPrerequisites(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Technical Prerequisites -- description of any technical capabilities that the consumer must have to use the data set in the form(s) provided by the distributor.';
  MetaDataItem.ShortName := 'techpreq';
  MetaDataItem.LongName := 'Technical Prerequisites';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.6';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateTechPrerequisites;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateTemporal(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Temporal -- time period(s) characterized by the data set.';
  MetaDataItem.ShortName := 'temporal';
  MetaDataItem.LongName := 'Temporal';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '1.6.4';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateTemporal;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateTemporalThesaurus(MetaDataItem);
  CreateTemporalKeywords(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTemporalKeywords(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Temporal Keyword -- the name of a time period covered by a data set.';
  MetaDataItem.ShortName := 'tempkey';
  MetaDataItem.LongName := 'Temporal Keyword';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.4.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateTemporalKeywords;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTemporalThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Temporal Keyword Thesaurus -- reference to a formally registered thesaurus or a similar authoritative source of temporal keywords.'
    + sLineBreak + 'See https://www2.usgs.gov/science/tab-term.html';
  MetaDataItem.ShortName := 'tempkt';
  MetaDataItem.LongName := 'Temporal Keyword Thesaurus';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.4.1';
  MetaDataItem.Choices.Add('None');
  MetaDataItem.Choice := MetaDataItem.Choices[0];
  MetaDataItem.TextHeight := 130;

  MetaDataItem.CreateMethod := CreateTemporalThesaurus;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStdsTermsDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'SDTS Terms Description -- point and vector object information using the terminology and concepts from'
    + ' "Spatial Data Concepts," which is Chapter 2 of Part 1 in Department of Commerce, 1992, Spatial Data Transfer'
    + ' Standard (SDTS) (Federal Information Processing Standard 173): Washington, Department of Commerce,'
    + ' National Institute of Standards and Technology. (Note that this reference to the SDTS is used ONLY to'
    + ' provide a set of terminology for the point and vector objects.)';
  MetaDataItem.ShortName := 'sdtsterm';
  MetaDataItem.LongName := 'SDTS Terms Description';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.1';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 265;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'ptvctinf';

  MetaDataItem.CreateMethod := CreateStdsTermsDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateSDTSPointAndVectorObjectType(MetaDataItem);
  CreatePointAndVectorObjectCount(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateStereographic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Stereographic -- contains parameters for the Stereographic projection.';
  MetaDataItem.ShortName := 'stereo';
  MetaDataItem.LongName := 'Stereographic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.20';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateStereographic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongPerspCenter(MetaDataItem);
  CreateLatPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTheme(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Theme -- subjects covered by the data set (for '
    + 'a list of some commonly-used thesauri, see Part IV: Subject/index term '
    + 'sources in Network Development and MARC Standards Office, 1988, USMARC '
    + 'code list for relators, sources, and description conventions: '
    + 'Washington, Library of Congress). ';
  MetaDataItem.ShortName := 'theme';
  MetaDataItem.LongName := 'Theme';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.1';
  MetaDataItem.TextHeight := 160;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateTheme;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateThemeThesaurus(MetaDataItem);
  CreateThemeKeyword(MetaDataItem);


  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateThemeKeyword(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description :=
    'Theme Keyword -- common-use word or phrase used to describe the subject of the data set.'
    + sLineBreak
    + 'USGS groundwater models must include "usgsgroundwatermodel" as a theme keyword.';
  MetaDataItem.ShortName := 'themekey';
  MetaDataItem.LongName := 'Theme Keyword';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.1.2';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 140;

  MetaDataItem.CreateMethod := CreateThemeKeyword;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateThemeThesaurus(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Theme Keyword Thesaurus -- reference to a formally registered thesaurus or a similar authoritative source of theme keywords.'
    + sLineBreak + 'See https://www2.usgs.gov/science/services.html'
    + sLineBreak
    + '*Example authoritative sources for theme keywords include the USGS '
    + 'Thesaurus (https://apps.usgs.gov/thesaurus/), USGS Biocomplexity '
    + 'Thesaurus, and the ISO 19115 Topic '
    + 'Category.';
  MetaDataItem.ShortName := 'themekt';
  MetaDataItem.LongName := 'Theme Keyword Thesaurus';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.6.1.1';
  MetaDataItem.Choices.Add('None');
  MetaDataItem.Choice := MetaDataItem.Choices[0];
  MetaDataItem.TextHeight := 220;

  MetaDataItem.CreateMethod := CreateThemeThesaurus;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTimeOfDay(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusTimeDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusTimeDataItem.Create(AnItem);
  MetaDataItem.Description := 'Time of Day -- the hour (and optionally minute, or minute and second) of the day.';
  MetaDataItem.ShortName := 'time';
  MetaDataItem.LongName := 'Time of Day';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '9.1.2';

  MetaDataItem.Choices.Add('Unknown');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateTimeOfDay;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateTimePeriod(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Time Period of Content -- time period(s) for which the data set corresponds to the currentness reference.';
  MetaDataItem.ShortName := 'timeperd';
  MetaDataItem.LongName := 'Time Period of Content';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.3';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateTimePeriod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  ChildItem := CreateTimePeriodInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
  CreateCurrentRef(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTimePeriodInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Time Period Information -- information about the date and time of an event.';
  MetaDataItem.ShortName := 'timeinfo';
  MetaDataItem.LongName := 'Time Period Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9';

  MetaDataItem.CreateMethod := CreateTimePeriodInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateSingleDateTime(MetaDataItem);
  CreateMultipleDateTimes(MetaDataItem);
  CreateRangeOfTimes(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateTitle(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Title -- the name by which the data set is known.'
    + sLineBreak
    + '(USGS Best Practice) Title should include the full name of the system '
    + 'and abbreviation if applicable.';
  MetaDataItem.ShortName := 'title';
  MetaDataItem.LongName := 'Title';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '8.4';
  MetaDataItem.TextHeight := 120;

  MetaDataItem.CreateMethod := CreateTitle;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateUnivPolarStereo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Universal Polar Stereographic (UPS) -- a grid system based on the polar stereographic projection, applied to the Earth''s polar regions north of 84 degrees north and south of 80 degrees south.';
  MetaDataItem.ShortName := 'ups';
  MetaDataItem.LongName := 'Universal Polar Stereographic (UPS)';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.3';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateUnivPolarStereo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateUnivPolarStereoZoneID(MetaDataItem);

  MChild := CreatePolarStereoGraphic(MetaDataItem);
  MChild.CheckType := ctCheckBox;
  MChild.Required := True;
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateUnivPolarStereoZoneID(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'UPS Zone Identifier -- identifier for the UPS zone.';
  MetaDataItem.ShortName := 'upszone';
  MetaDataItem.LongName := 'UPS Zone Identifier';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.3.1';

  MetaDataItem.Choices.Add('A');
  MetaDataItem.Choices.Add('B');
  MetaDataItem.Choices.Add('Y');
  MetaDataItem.Choices.Add('Z');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateUnivPolarStereoZoneID;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateUnknownCompound(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Unknown';
  MetaDataItem.ShortName := '';
  MetaDataItem.LongName := '';
  MetaDataItem.Required := False;
  MetaDataItem.ID := 'Unknown';

  MetaDataItem.CreateMethod := CreateUnknownCompound;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateUnknownText(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Unknown';
  MetaDataItem.ShortName := '';
  MetaDataItem.LongName := '';
  MetaDataItem.Required := False;
  MetaDataItem.ID := 'Unknown';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateUnknownText;

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateUnrepresentableDomain(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Unrepresentable Domain -- description of the values and reasons why they cannot be represented.';
  MetaDataItem.ShortName := 'udom';
  MetaDataItem.LongName := 'Unrepresentable Domain';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.4';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateUnrepresentableDomain;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateUpdateFrequency(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Maintenance and Update Frequency -- the frequency with which changes and additions are made to the data set after the initial data set is completed. ';
  MetaDataItem.ShortName := 'update';
  MetaDataItem.LongName := 'Maintenance and Update Frequency';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.4.2';

  MetaDataItem.Choices.Add('Continually');
  MetaDataItem.Choices.Add('Daily');
  MetaDataItem.Choices.Add('Weekly');
  MetaDataItem.Choices.Add('Monthly');
  MetaDataItem.Choices.Add('Annually');
  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choices.Add('As needed');
  MetaDataItem.Choices.Add('Irregular');
  MetaDataItem.Choices.Add('None planned');
  MetaDataItem.TextHeight := 95;

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateUpdateFrequency;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateUseConstraints(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Use Constraints -- restrictions and legal '
    + 'prerequisites for using the data set after access is granted. These '
    + 'include any use constraints applied to assure the protection of privacy '
    + 'or intellectual property, and any special restrictions or limitations '
    + 'on using the data set. '
    + sLineBreak
    + '(USGS Best Practice) Example: “The USGS shall not be held liable for '
    + 'improper or incorrect use of the data retrieved from the system”';
  MetaDataItem.ShortName := 'useconst';
  MetaDataItem.LongName := 'Use Constraints';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.8';
  MetaDataItem.TextHeight := 220;

  MetaDataItem.CreateMethod := CreateUseConstraints;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateUTM(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Universal Transverse Mercator (UTM) -- a grid system based on the transverse mercator projection, applied between latitudes 84 degrees north and 80 degrees south on the Earth''s surface.';
  MetaDataItem.ShortName := 'utm';
  MetaDataItem.LongName := 'Universal Transverse Mercator (UTM)';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.2';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'gridsys';

  MetaDataItem.CreateMethod := CreateUTM;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateUTMZoneNumber(MetaDataItem);
  MChild := CreateTransverseMercator(MetaDataItem);
  MChild.CheckType := ctCheckBox;
  MChild.Required := True;
  AssignMetaDataToTreeViewItem(MChild);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateUTMZoneNumber(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'UTM Zone Number -- identifier for the UTM zone.'
    + sLineBreak
    + '1 <= UTM Zone Number <= 60 for the northern hemisphere'
    + sLineBreak
    + '-60 <= UTM Zone Number <= -1 for the southern hemisphere';
  MetaDataItem.ShortName := 'utmzone';
  MetaDataItem.LongName := 'UTM Zone Number';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.2.1';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.Content := 0;
  MetaDataItem.MaxValue := 60;
  MetaDataItem.MinValue := -60;

  MetaDataItem.AddIllegalValue(0);

  MetaDataItem.CreateMethod := CreateUTMZoneNumber;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateVanDerGrinten(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'van der Grinten -- contains parameters for the van der Grinten projection.';
  MetaDataItem.ShortName := 'vdgrin';
  MetaDataItem.LongName := 'van der Grinten';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.22';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateVanDerGrinten;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVertCoordSysDef(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Coordinate System Definition -- the reference frame or system from which vertical distances (altitudes or depths) are measured.';
  MetaDataItem.ShortName := 'vertdef';
  MetaDataItem.LongName := 'Vertical Coordinate System Definition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateVertCoordSysDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAltitudeSysDef(MetaDataItem);
  CreateDepthSysDef(MetaDataItem);
end;

function TframeMetaDataEditor.CreateVertPositionalAccuracy(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Positional Accuracy -- an estimate of accuracy of the vertical positions in the data set.';
  MetaDataItem.ShortName := 'vertacc';
  MetaDataItem.LongName := 'Vertical Positional Accuracy';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.4.2';

  MetaDataItem.CreateMethod := CreateVertPositionalAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateVertPositionalAccuracyReport(MetaDataItem);
  CreateQuantitativeVertPositionalAccuracy(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVertPositionalAccuracyExplanation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Positional Accuracy Explanation -- the identification of the test that yielded the Vertical Positional Accuracy Value.';
  MetaDataItem.ShortName := 'vertacce';
  MetaDataItem.LongName := 'Vertical Positional Accuracy Explanation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.2.2.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateVertPositionalAccuracyExplanation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVertPositionalAccuracyReport(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Positional Accuracy Report -- an explanation of the accuracy of the vertical coordinate measurements and a description of the tests used.';
  MetaDataItem.ShortName := 'vertaccr';
  MetaDataItem.LongName := 'Vertical Positional Accuracy Report';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.2.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateVertPositionalAccuracyReport;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVertPositionalAccuracyValue(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Vertical Positional Accuracy Value -- an estimate of the accuracy of the vertical coordinate measurements in the data set expressed in (ground) meters.';
  MetaDataItem.ShortName := 'vertaccv';
  MetaDataItem.LongName := 'Vertical Positional Accuracy Value';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.2.2.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateVertPositionalAccuracyValue;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVpfInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'VPF Point and Vector Object Information -- information about VPF point and vector objects';
  MetaDataItem.ShortName := 'vpfinfo';
  MetaDataItem.LongName := 'VPF Point and Vector Object Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.2.2';
  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateVpfInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateVpfInfoObjectType(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVpfInfoObjectType(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'VPF Point and Vector Object Type -- name of point and vector spatial objects used to locate zero-, one-, and two-dimensional spatial locations in the data set.';
  MetaDataItem.ShortName := 'vpftype';
  MetaDataItem.LongName := 'VPF Point and Vector Object Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.2.2.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Choices.Add('Node');
  MetaDataItem.Choices.Add('Edge');
  MetaDataItem.Choices.Add('Face');
  MetaDataItem.Choices.Add('Text');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateVpfInfoObjectType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVpfLevel(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'VPF Topology Level -- the completeness of the topology carried by the data set. The levels of completeness'
    + ' are defined in Department of Defense, 1992, Vector Product Format (MIL-STD-600006): Philadelphia,'
    + ' Department of Defense, Defense Printing Service Detachment Office.';
  MetaDataItem.ShortName := 'vpflevel';
  MetaDataItem.LongName := 'VPF Topology Level';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.2.1';
  MetaDataItem.TextHeight := 155;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MaxValue := 3;
  MetaDataItem.Content := 0;

  MetaDataItem.CreateMethod := CreateVpfLevel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateVpfTermsDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'VPF Terms Description -- point and vector object information using the terminology and concepts from'
    + ' Department of Defense, 1992, Vector Product Format (MIL-STD-600006): Philadelphia, Department of Defense,'
    + ' Defense Printing Service Detachment Office. (Note that this reference to the VPF is used ONLY to provide a'
    + ' set of terminology for the point and vector objects.)';
  MetaDataItem.ShortName := 'vpfterm';
  MetaDataItem.LongName := 'VPF Terms Description';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '3.3.2';
  MetaDataItem.TextHeight := 200;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'ptvctinf';

  MetaDataItem.CreateMethod := CreateVpfTermsDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateVpfLevel(MetaDataItem);
  CreateVpfInfo(MetaDataItem);
  CreatePointAndVectorObjectCount(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateDataQuality(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Data Quality Information -- a general assessment of the quality of the data set. (Recommendations '
    + 'on information to be reported and tests to be performed are found in "Spatial Data Quality," which '
    + 'is chapter 3 of part 1 in Department of Commerce, 1992, Spatial Data Transfer Standard (SDTS) '
    + '(Federal Information Processing Standard 173): Washington, Department of Commerce, National '
    + 'Institute of Standards and Technology.)';
  MetaDataItem.ShortName := 'dataqual';
  MetaDataItem.LongName := 'Data Quality Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2';
  MetaDataItem.TextHeight := 240;

  MetaDataItem.CreateMethod := CreateDataQuality;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateAttributeAccuracy(MetaDataItem);
  CreateLogicalConsistency(MetaDataItem);
  CreateCompleteness(MetaDataItem);
  CreatePositionalAccuracy(MetaDataItem);
  CreateLineage(MetaDataItem);
  CreateCloudCover(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateDataSetCredit(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Data Set Credit -- recognition of those who contributed to the data set.';
  MetaDataItem.ShortName := 'datacred';
  MetaDataItem.LongName := 'Data Set Credit';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.11';

  MetaDataItem.CreateMethod := CreateDataSetCredit;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateDenFlatRatio(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Denominator of Flattening Ratio -- the denominator of the ratio of the difference between the equatorial and polar radii of the ellipsoid when the numerator is set to 1.';
  MetaDataItem.ShortName := 'denflat';
  MetaDataItem.LongName := 'Denominator of Flattening Ratio';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.4.4';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateDenFlatRatio;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDepthDatumName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Depth Datum Name -- the identification given to surface of reference from which depths are measured.';
  MetaDataItem.ShortName := 'depthdn';
  MetaDataItem.LongName := 'Depth Datum Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.2.1';

  MetaDataItem.Choices.Add('Local surface');
  MetaDataItem.Choices.Add('Chart datum; datum for sounding reduction');
  MetaDataItem.Choices.Add('Lowest astronomical tide');
  MetaDataItem.Choices.Add('Highest astronomical tide');
  MetaDataItem.Choices.Add('Mean low water');
  MetaDataItem.Choices.Add('Mean high water');
  MetaDataItem.Choices.Add('Mean sea level');
  MetaDataItem.Choices.Add('Land survey datum');
  MetaDataItem.Choices.Add('Mean low water springs');
  MetaDataItem.Choices.Add('Mean high water springs');
  MetaDataItem.Choices.Add('Mean low water neap');
  MetaDataItem.Choices.Add('Mean high water neap');
  MetaDataItem.Choices.Add('Mean lower low water');
  MetaDataItem.Choices.Add('Mean lower low water springs');
  MetaDataItem.Choices.Add('Mean higher high water');
  MetaDataItem.Choices.Add('Mean higher low water');
  MetaDataItem.Choices.Add('Mean lower high water');
  MetaDataItem.Choices.Add('Spring tide');
  MetaDataItem.Choices.Add('Tropic lower low water');
  MetaDataItem.Choices.Add('Neap tide');
  MetaDataItem.Choices.Add('High water');
  MetaDataItem.Choices.Add('Higher high water');
  MetaDataItem.Choices.Add('Low water');
  MetaDataItem.Choices.Add('Low-water datum');
  MetaDataItem.Choices.Add('Lowest low water');
  MetaDataItem.Choices.Add('Lower low water');
  MetaDataItem.Choices.Add('Lowest normal low water');
  MetaDataItem.Choices.Add('Mean tide level');
  MetaDataItem.Choices.Add('Indian spring low water');
  MetaDataItem.Choices.Add('High-water full and charge');
  MetaDataItem.Choices.Add('Low-water full and charge');
  MetaDataItem.Choices.Add('Columbia River datum');
  MetaDataItem.Choices.Add('Gulf Coast low water datum');
  MetaDataItem.Choices.Add('Equatorial springs low water');
  MetaDataItem.Choices.Add('Approximate lowest astronomical tide');
  MetaDataItem.Choices.Add('No correction');

  MetaDataItem.CreateMethod := CreateDepthDatumName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDepthDistanceUnit(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Depth Distance Units -- units in which depths are recorded.';
  MetaDataItem.ShortName := 'depthdu';
  MetaDataItem.LongName := 'Depth Distance Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.2.3';

  MetaDataItem.Choices.Add('meters');
  MetaDataItem.Choices.Add('feet');

  MetaDataItem.CreateMethod := CreateDepthDistanceUnit;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDepthEncodingMethod(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Depth Encoding Method -- the means used to encode depths.';
  MetaDataItem.ShortName := 'depthem';
  MetaDataItem.LongName := 'Depth Encoding Method';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.2.4';

  MetaDataItem.Choices.Add('Explicit depth coordinate included with horizontal coordinates');
  MetaDataItem.Choices.Add('Implicit coordinate');
  MetaDataItem.Choices.Add('Attribute value');

  MetaDataItem.CreateMethod := CreateDepthEncodingMethod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDepthResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Depth Resolution -- the minimum distance possible between two adjacent depth values, expressed in Depth Distance Units of measure.';
  MetaDataItem.ShortName := 'depthres';
  MetaDataItem.LongName := 'Depth Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.2.2.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.MoreThanOneAllowed := true;

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateDepthResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDepthSysDef(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Depth System Definition -- the reference frame or system from which depths are measured.';
  MetaDataItem.ShortName := 'depthsys';
  MetaDataItem.LongName := 'Depth System Definition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.2.2';

  MetaDataItem.CreateMethod := CreateDepthSysDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateDepthDatumName(MetaDataItem);
  CreateDepthResolution(MetaDataItem);
  CreateDepthDistanceUnit(MetaDataItem);
  CreateDepthEncodingMethod(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Description -- a characterization of the data set, including its intended use and limitations.';
  MetaDataItem.ShortName := 'descript';
  MetaDataItem.LongName := 'Description';
  MetaDataItem.Required := True;
  MetaDataItem.ID := '1.2';
  MetaDataItem.RequiredType := rtManditory;

  MetaDataItem.CreateMethod := CreateDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateAbstract(MetaDataItem);
  CreatePurpose(MetaDataItem);
  CreateSupplimentalInfo(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDetailedDescription(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Detailed Description -- description of the entities, attributes, attribute values, and related characteristics encoded in the data set.';
  MetaDataItem.ShortName := 'detailed';
  MetaDataItem.LongName := 'Detailed Description';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateDetailedDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateEntityType(MetaDataItem);
  CreateAttribute(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDialupFileName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Dialup File Name -- the name of a file containing the data set on the distribution computer.';
  MetaDataItem.ShortName := 'dialfile';
  MetaDataItem.LongName := 'Dialup File Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.7';

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateDialupFileName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDialupInstructions(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Dialup Instructions -- information required to access the distribution computer remotely through telephone lines.';
  MetaDataItem.ShortName := 'dialinst';
  MetaDataItem.LongName := 'Dialup Instructions';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateDialupInstructions;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateLowestBPS(MetaDataItem);
  CreateHighestBPS(MetaDataItem);
  CreateNumDataBits(MetaDataItem);
  CreateNumStopBits(MetaDataItem);
  CreateParity(MetaDataItem);
  CreateCompressionSupport(MetaDataItem);
  CreateDialupTel(MetaDataItem);
  CreateDialupFileName(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDialupTel(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Dialup Telephone -- the telephone number of the distribution computer.';
  MetaDataItem.ShortName := 'dialtel';
  MetaDataItem.LongName := 'Dialup Telephone';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2.1.1.2.7';

  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateDialupTel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDigitalForm(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Digital Form -- the description of options for obtaining the data set on computer-compatible media.';
  MetaDataItem.ShortName := 'digform';
  MetaDataItem.LongName := 'Digital Form';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'stdorder';

  MetaDataItem.CreateMethod := CreateDigitalForm;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateDigitalTransferInfo(MetaDataItem);
  CreateDigitalTransferOption(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDigitalTransferInfo(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Digital Transfer Information - description of the form of the data to be distributed.';
  MetaDataItem.ShortName := 'digtinfo';
  MetaDataItem.LongName := 'Digital Transfer Information';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.1';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateDigitalTransferInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateFormatName(MetaDataItem);
  CreateFormatVersionNumber(MetaDataItem);
  CreateFormatVersionDate(MetaDataItem);
  CreateFormatSpecification(MetaDataItem);
  CreateFormatInformationContent(MetaDataItem);
  CreateFileDecompTech(MetaDataItem);
  CreateTransferSize(MetaDataItem)
end;

function TframeMetaDataEditor.CreateDigitalTransferOption(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Digital Transfer Option -- the means and media by which a data set is obtained from the distributor.';
  MetaDataItem.ShortName := 'digtopt';
  MetaDataItem.LongName := 'Digital Transfer Option';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.2';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateDigitalTransferOption;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateOnlineOption(MetaDataItem);
  CreateOfflineOption(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDirectSpatialReferenceMethod(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Direct Spatial Reference Method -- the system of objects used to represent space in the data set.';
  MetaDataItem.ShortName := 'direct';
  MetaDataItem.LongName := 'Direct Spatial Reference Method';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '3.2';

  MetaDataItem.Choices.Add('Point');
  MetaDataItem.Choices.Add('Vector');
  MetaDataItem.Choices.Add('Raster');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateDirectSpatialReferenceMethod;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateDistAndBearingRepresentation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Distance and Bearing Representation -- a method of encoding the position of a point by measuring its distance and direction (azimuth angle) from another point.';
  MetaDataItem.ShortName := 'distbrep';
  MetaDataItem.LongName := 'Distance and Bearing Representation';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'planci';

  MetaDataItem.CreateMethod := CreateDistAndBearingRepresentation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateDistResolution(MetaDataItem);
  CreateBearingResolution(MetaDataItem);
  CreateBearingUnits(MetaDataItem);
  CreateBearingRefDir(MetaDataItem);
  CreateBearingRefMeridian(MetaDataItem);
end;

function TframeMetaDataEditor.CreateDistResolution(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Distance Resolution -- the minimum distance measurable between two points, expressed Planar Distance Units of measure.';
  MetaDataItem.ShortName := 'distres';
  MetaDataItem.LongName := 'Distance Resolution';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.4.3.1';

  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateDistResolution;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDistributionInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Distribution Information -- information about the distributor of and options for obtaining the data set.';
  MetaDataItem.ShortName := 'distinfo';
  MetaDataItem.LongName := 'Distribution Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateDistributionInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateDistributor(MetaDataItem);
  CreateResourceDescription(MetaDataItem);
  CreateDistributionLiability(MetaDataItem);
  CreateStandardOrderProcess(MetaDataItem);
  CreateCustomOrderProcess(MetaDataItem);
  CreateTechPrerequisites(MetaDataItem);
  CreateAvailableTimePeriod(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateDistributionLiability(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Distribution Liability -- statement of the liability assumed by the distributor.'
    + sLineBreak
    + '(USGS Best Practice)'
    + sLineBreak
    + 'Some suggestions for USGS Information Products '
    + sLineBreak
    + 'http://www.usgs.gov/fsp/fsp_disclaimers.asp';
  MetaDataItem.ShortName := 'distliab';
  MetaDataItem.LongName := 'Distribution Liability';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.3';
  MetaDataItem.TextHeight := 160;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateDistributionLiability;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateDistributor(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Distributor -- the party from whom the data set may be obtained.'
    + sLineBreak
    + '(USGS Best Practice) It is preferred to use the position or role '
    + 'contact information rather than a specific person. If a person must be '
    + 'listed as the contact, include the Contact Organization.'
    + sLineBreak
    + 'Example: “request@usgs.gov”';
  MetaDataItem.ShortName := 'distrib';
  MetaDataItem.LongName := 'Distributor';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.1';
  MetaDataItem.TextHeight := 180;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateDistributor;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  ChildItem := CreateContactInfo(MetaDataItem);
  ChildItem.RequiredType := rtManditory;
end;

function TframeMetaDataEditor.CreateEdition(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Edition -- the version of the title.';
  MetaDataItem.ShortName := 'edition';
  MetaDataItem.LongName := 'Edition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.5';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEdition;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateEllipsoidName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ellipsoid Name -- identification given to established representations of the Earth''s shape.';
  MetaDataItem.ShortName := 'ellips';
  MetaDataItem.LongName := 'Ellipsoid Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.4.2';

  MetaDataItem.Choices.Add('Clarke 1866');
  MetaDataItem.Choices.Add('Geodetic Reference System 80');

  MetaDataItem.CreateMethod := CreateEllipsoidName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEMailAddress(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Contact Electronic Mail Address -- the address of the electronic mailbox of the organization or individual.'
    + sLineBreak
    + 'For 6.1: "Distributor" about USGS data sets, "https://answers.usgs.gov/" might be a good choice.'
    + sLineBreak
    + 'For 7: "Metadata Reference Information" about USGS data sets, this must be "https://answers.usgs.gov/cgi-bin/gsanswers?pemail=h2oteam&amp;subject=GIS+Dataset+the name of your metadata file, Example: ofr02-338_bedrock".';
  MetaDataItem.ShortName := 'cntemail';
  MetaDataItem.LongName := 'Contact Electronic Mail Address';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.8';
  MetaDataItem.TextHeight := 290;

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEMailAddress;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateEndDate(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ending Date -- the last year (and optionally month, or month and day) for the event.'
    + sLineBreak
    + '(USGS Best Practice) The last observation or event in the system. If '
    + 'data collection is ongoing, enter “Present”.';
  MetaDataItem.ShortName := 'enddate';
  MetaDataItem.LongName := 'Ending Date';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '9.3.3';
  MetaDataItem.TextHeight := 130;

  MetaDataItem.Choices.Add('Unknown');
  MetaDataItem.Choices.Add('Present');
  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateEndDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateEndTime(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusTimeDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusTimeDataItem.Create(AnItem);
  MetaDataItem.Description := 'Ending Time -- the last hour (and optionally minute, or minute and second) of the day for the event.';
  MetaDataItem.ShortName := 'endtime';
  MetaDataItem.LongName := 'Ending Time';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '9.3.4';

  MetaDataItem.Choices.Add('Unknown');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEndTime;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateEntityAndAttributeCitation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity and Attribute Detail Citation -- reference to the complete description of the entity types, attributes, and attribute values for the data set.';
  MetaDataItem.ShortName := 'eadetcit';
  MetaDataItem.LongName := 'Entity and Attribute Detail Citation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.2.2';
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateEntityAndAttributeCitation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEntityAndAttributeOverviewDescription(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity and Attribute Overview -- detailed summary of the information contained in a data set.';
  MetaDataItem.ShortName := 'eaover';
  MetaDataItem.LongName := 'Entity and Attribute Overview';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.2.1';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateEntityAndAttributeOverviewDescription;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEntityAttributesInfo(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity and Attribute Information -- details about the information content of the data set, including '
    + 'the entity types, their attributes, and the domains from which attribute values may be assigned.';
  MetaDataItem.ShortName := 'eainfo';
  MetaDataItem.LongName := 'Entity and Attribute Information';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '5';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateEntityAttributesInfo;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateDetailedDescription(MetaDataItem);
  CreateOverviewDescription(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateEntityType(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity Type -- the definition and description of a set into which similar entity instances are classified.';
  MetaDataItem.ShortName := 'enttyp';
  MetaDataItem.LongName := 'Entity Type';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.1';

  MetaDataItem.CreateMethod := CreateEntityType;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateEntityTypeLabel(MetaDataItem);
  CreateEntityTypeDefinition(MetaDataItem);
  CreateEntityTypeDefinitionSource(MetaDataItem);
end;

function TframeMetaDataEditor.CreateEntityTypeDefinition(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity Type Definition -- the description of the entity type.';
  MetaDataItem.ShortName := 'enttypd';
  MetaDataItem.LongName := 'Entity Type Definition';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.1.2';

  MetaDataItem.CreateMethod := CreateEntityTypeDefinition;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEntityTypeDefinitionSource(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity Type Definition Source -- the authority of the definition.';
  MetaDataItem.ShortName := 'enttypds';
  MetaDataItem.LongName := 'Entity Type Definition Source';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.1.3';

  MetaDataItem.CreateMethod := CreateEntityTypeDefinitionSource;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEntityTypeLabel(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Entity Type Label -- the name of the entity type.';
  MetaDataItem.ShortName := 'enttypl';
  MetaDataItem.LongName := 'Entity Type Label';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.1.1';

  MetaDataItem.CreateMethod := CreateEntityTypeLabel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEnumeratedDomain(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  ChildItem: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Enumerated Domain -- the members of an established set of valid values.';
  MetaDataItem.ShortName := 'edom';
  MetaDataItem.LongName := 'Enumerated Domain';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.1';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'attrdomv';

  MetaDataItem.CreateMethod := CreateEnumeratedDomain;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;

  CreateEnumeratedDomainValue(MetaDataItem);
  CreateEnumeratedDomainValueDef(MetaDataItem);
  CreateEnumeratedDomainValueDefSource(MetaDataItem);
  ChildItem := CreateAttribute(MetaDataItem);
  ChildItem.RequiredType := rtManditoryIfApplicable;
end;

function TframeMetaDataEditor.CreateEnumeratedDomainValue(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Enumerated Domain Value -- the name or label of a member of the set.';
  MetaDataItem.ShortName := 'edomv';
  MetaDataItem.LongName := 'Enumerated Domain Value';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.1';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEnumeratedDomainValue;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEnumeratedDomainValueDef(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Enumerated Domain Value Definition -- the description of the value.';
  MetaDataItem.ShortName := 'edomvd';
  MetaDataItem.LongName := 'Enumerated Domain Value Definition';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.1.2';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEnumeratedDomainValueDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEnumeratedDomainValueDefSource(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Enumerated Domain Value Definition Source -- the authority of the definition.';
  MetaDataItem.ShortName := 'edomvds';
  MetaDataItem.LongName := 'Enumerated Domain Value Definition Source';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '5.1.2.4.1.3';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateEnumeratedDomainValueDefSource;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateEquidistantConic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
  MChild: TCustomMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Equidistant Conic -- contains parameters for the Equidistant Conic projection.';
  MetaDataItem.ShortName := 'equicon';
  MetaDataItem.LongName := 'Equidistant Conic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.4';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateEquidistantConic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateStandardParallel(MetaDataItem);

  // stdparll
  MChild := CreateStandardParallel(MetaDataItem);
  MChild.Required := False;
  MChild.Used := False;
  AssignMetaDataToTreeViewItem(MChild);


  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateLatProjOrigin(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateEquirectangular(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := ' Equirectangular -- contains parameters for the Equirectangular projection.';
  MetaDataItem.ShortName := 'equirect';
  MetaDataItem.LongName := 'Equirectangular';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.5';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateEquirectangular;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateStandardParallel(MetaDataItem);
  CreateLongitudeStrdMeridian(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  Result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFalseEasting(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'False Easting -- the value added to all "x" values in the rectangular coordinates for a map projection. This value frequently is assigned to eliminate negative numbers. Exp ressed in the unit of measure identified in Planar Coordinate Units.';
  MetaDataItem.ShortName := 'feast';
  MetaDataItem.LongName := 'False Easting';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.4';
  MetaDataItem.TextHeight := 140;

  MetaDataItem.Content := 0;

  MetaDataItem.CreateMethod := CreateFalseEasting;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFalseNorthing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'False Northing -- the value added to all "y" values in the rectangular coordinates for a map projection. This value frequently is assigned to eliminate negative numbers. Expressed in the unit of measure identified in Planar Coordinate Units.';
  MetaDataItem.ShortName := 'fnorth';
  MetaDataItem.LongName := 'False Northing';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.5';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.Content := 0;

  MetaDataItem.CreateMethod := CreateFalseNorthing;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFees(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Fees -- the fees and terms for retrieving the data set.';
  MetaDataItem.ShortName := 'fees';
  MetaDataItem.LongName := 'Fees';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.3';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtopt';

  MetaDataItem.CreateMethod := CreateFees;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFileDecompTech(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'File Decompression Technique -- recommendations of algorithms or processes (including means of'
    + ' obtaining these algorithms or processes) that can be applied to read or expand data sets to which data'
    + ' compression techniques have been applied.';
  MetaDataItem.ShortName := 'filedec';
  MetaDataItem.LongName := 'File Decompression Technique';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4.2.1.6';
  MetaDataItem.TextHeight := 140;

  MetaDataItem.Choices.Add('No compression applied');

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateFileDecompTech;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFormatInformationContent(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Format Information Content -- description of the content of the data encoded in a format.'
    + sLineBreak
    + '(USGS Best Practice) Description of the web service or direct links to '
    + 'the data.'
    + sLineBreak
    + 'Example: “NOAA''s Environmental Research Division Data Access Program '
    + 'enables users to query scientific data by flexible parameters and '
    + 'obtain output in many formats.”';
  MetaDataItem.ShortName := 'formcont';
  MetaDataItem.LongName := 'Format Information Content';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.2.1.5';
  MetaDataItem.TextHeight := 200;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateFormatInformationContent;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFormatName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Format Name -- the name of the data transfer format.'
    + sLineBreak
    + '(USGS Best Practice) Name of the web service. Example: “ERDDAP”';
  MetaDataItem.ShortName := 'formname';
  MetaDataItem.LongName := 'Format Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.1.1';

  MetaDataItem.Choices.Add('ARCE');
  MetaDataItem.AlternativeChoices.Add('"ARCE" ARC/INFO Export format');
  MetaDataItem.Choices.Add('ARCG');
  MetaDataItem.AlternativeChoices.Add('"ARCG" ARC/INFO Generate format');
  MetaDataItem.Choices.Add('ASCII');
  MetaDataItem.AlternativeChoices.Add('"ASCII" ASCII file, formatted for text attributes, declared format');
  MetaDataItem.Choices.Add('BIL');
  MetaDataItem.AlternativeChoices.Add('"BIL" Imagery, band interleaved by line');
  MetaDataItem.Choices.Add('BIP');
  MetaDataItem.AlternativeChoices.Add('"BIP" Imagery, band interleaved by pixel');
  MetaDataItem.Choices.Add('BSQ');
  MetaDataItem.AlternativeChoices.Add('"BSQ" Imagery, band interleaved sequential');
  MetaDataItem.Choices.Add('CDF');
  MetaDataItem.AlternativeChoices.Add('"CDF" Common Data Format');
  MetaDataItem.Choices.Add('CFF');
  MetaDataItem.AlternativeChoices.Add('"CFF" Cartographic Feature File (U.S. Forest Service)');
  MetaDataItem.Choices.Add('COORD');
  MetaDataItem.AlternativeChoices.Add('"COORD" User-created coordinate file, declared format');
  MetaDataItem.Choices.Add('DEM');
  MetaDataItem.AlternativeChoices.Add('"DEM" Digital Elevation Model format (U.S. Geological Survey)');
  MetaDataItem.Choices.Add('DFAD');
  MetaDataItem.AlternativeChoices.Add('"DFAD" Digital Feature Analysis Data (National Imagery and Mapping Agency)');
  MetaDataItem.Choices.Add('DGN');
  MetaDataItem.AlternativeChoices.Add('"DGN" Microstation format (Intergraph Corporation)');
  MetaDataItem.Choices.Add('DIGEST');
  MetaDataItem.AlternativeChoices.Add('"DIGEST" Digital Geographic Information Exchange Standard');
  MetaDataItem.Choices.Add('DLG');
  MetaDataItem.AlternativeChoices.Add('"DLG" Digital Line Graph (U.S. Geological Survey)');
  MetaDataItem.Choices.Add('DTED');
  MetaDataItem.AlternativeChoices.Add('"DTED" Digital Terrain Elevation Data (MIL-D-89020)');
  MetaDataItem.Choices.Add('DWG');
  MetaDataItem.AlternativeChoices.Add('"DWG" AutoCAD Drawing format');
  MetaDataItem.Choices.Add('DX90');
  MetaDataItem.AlternativeChoices.Add('"DX90" Data Exchange ''90');
  MetaDataItem.Choices.Add('DXF');
  MetaDataItem.AlternativeChoices.Add('"DXF" AutoCAD Drawing Exchange Format');
  MetaDataItem.Choices.Add('ERDAS');
  MetaDataItem.AlternativeChoices.Add('"ERDAS" ERDAS image files (ERDAS Corporation)');
  MetaDataItem.Choices.Add('GRASS');
  MetaDataItem.AlternativeChoices.Add('"GRASS" Geographic Resources Analysis Support System');
  MetaDataItem.Choices.Add('HDF');
  MetaDataItem.AlternativeChoices.Add('"HDF" Hierarchical Data Format');
  MetaDataItem.Choices.Add('IGDS');
  MetaDataItem.AlternativeChoices.Add('"IGDS" Interactive Graphic Design System format (Intergraph Corporation)');
  MetaDataItem.Choices.Add('IGES');
  MetaDataItem.AlternativeChoices.Add('"IGES" Initial Graphics Exchange Standard');
  MetaDataItem.Choices.Add('MOSS');
  MetaDataItem.AlternativeChoices.Add('"MOSS" Multiple Overlay Statistical System export file');
  MetaDataItem.Choices.Add('netCDF');
  MetaDataItem.AlternativeChoices.Add('"netCDF" network Common Data Format');
  MetaDataItem.Choices.Add('NITF');
  MetaDataItem.AlternativeChoices.Add('"NITF" National Imagery Transfer Format');
  MetaDataItem.Choices.Add('RPF');
  MetaDataItem.AlternativeChoices.Add('"RPF" Raster Product Format (National Imagery and Mapping Agency)');
  MetaDataItem.Choices.Add('RVC');
  MetaDataItem.AlternativeChoices.Add('"RVC" Raster Vector Converted format (MicroImages)');
  MetaDataItem.Choices.Add('RVF');
  MetaDataItem.AlternativeChoices.Add('"RVF" Raster Vector Format (MicroImages)');
  MetaDataItem.Choices.Add('SDTS');
  MetaDataItem.AlternativeChoices.Add('"SDTS" Spatial Data Transfer Standard (Federal Information Processing Standard 173)');
  MetaDataItem.Choices.Add('SIF');
  MetaDataItem.AlternativeChoices.Add('"SIF" Standard Interchange Format (DOD Project 2851)');
  MetaDataItem.Choices.Add('SLF');
  MetaDataItem.AlternativeChoices.Add('"SLF" Standard Linear Format (National Imagery and Mapping Agency)');
  MetaDataItem.Choices.Add('TIFF');
  MetaDataItem.AlternativeChoices.Add('"TIFF" Tagged Image File Format');
  MetaDataItem.Choices.Add('TGRLN');
  MetaDataItem.AlternativeChoices.Add('"TGRLN" Topologically Integrated Geographic Encoding and Referencing (TIGER) Line format (Bureau of the Census)');
  MetaDataItem.Choices.Add('VPF');
  MetaDataItem.AlternativeChoices.Add('"VPF" Vector Product Format (National Imagery and Mapping Agency)');
  MetaDataItem.TextHeight := 90;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'stdorder';

  MetaDataItem.CreateMethod := CreateFormatName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFormatSpecification(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Format Specification -- name of a subset, profile, or product specification of the format.';
  MetaDataItem.ShortName := 'formspec';
  MetaDataItem.LongName := 'Format Specification';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '6.4.2.1.4';

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateFormatSpecification;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFormatVersionDate(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TDateMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TDateMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Format Version Date -- date of the version of the format.'
    + sLineBreak + 'Double-click radio button to deselect';
  MetaDataItem.ShortName := 'formverd';
  MetaDataItem.LongName := 'Format Version Date';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.1.3';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateFormatVersionDate;
  AssignMetaDataToTreeViewItem(MetaDataItem);
  MetaDataItem.RadioButton.OnDblClick := OptionalRadioButtonDoubleClicked;

//  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateFormatVersionNumber(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Format Version Number -- version number of the format.'
    + sLineBreak + 'Double-click radio button to deselect'
    + sLineBreak
    + '(USGS Best Practice) Version of the web service if applicable';
  MetaDataItem.ShortName := 'formvern';
  MetaDataItem.LongName := 'Format Version Number';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '6.4.2.1.2';
  MetaDataItem.TextHeight := 110;

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'digtinfo';

  MetaDataItem.CreateMethod := CreateFormatVersionNumber;
  AssignMetaDataToTreeViewItem(MetaDataItem);
  MetaDataItem.RadioButton.OnDblClick := OptionalRadioButtonDoubleClicked;

//  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateGenVertNearPersp(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'General Vertical Near-sided Perspective -- contains parameters for the General Vertical Near-sided Perspective projection.';
  MetaDataItem.ShortName := 'gvnsp';
  MetaDataItem.LongName := 'General Vertical Near-sided Perspective';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.6';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateGenVertNearPersp;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateHeightOfPerspectivePoint(MetaDataItem);
  CreateLongPerspCenter(MetaDataItem);
  CreateLatPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGeoCoordUnits(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Geographic Coordinate Units -- units of measure used for the latitude and longitude values.';
  MetaDataItem.ShortName := 'geogunit';
  MetaDataItem.LongName := 'Geographic Coordinate Units';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.1.3';

  MetaDataItem.Choices.Add('Decimal degrees');
  MetaDataItem.Choices.Add('Decimal minutes');
  MetaDataItem.Choices.Add('Decimal seconds');
  MetaDataItem.Choices.Add('Degrees and decimal minutes');
  MetaDataItem.Choices.Add('Degrees, minutes, and decimal seconds');
  MetaDataItem.Choices.Add('Radians');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateGeoCoordUnits;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGeodeticModel(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Geodetic Model -- parameters for the shape of the earth.';
  MetaDataItem.ShortName := 'geodetic';
  MetaDataItem.LongName := 'Geodetic Model';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.1.4';

  MetaDataItem.CreateMethod := CreateGeodeticModel;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

  CreateHorizontalDatumName(MetaDataItem);
  CreateEllipsoidName(MetaDataItem);
  CreateSemiMajorAxis(MetaDataItem);
  CreateDenFlatRatio(MetaDataItem);
end;

function TframeMetaDataEditor.CreateGeospatialForm(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Geospatial Data Presentation Form -- the mode in which the geospatial data are represented.';
  MetaDataItem.ShortName := 'geoform';
  MetaDataItem.LongName := 'Geospatial Data Presentation Form';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '8.6';

  MetaDataItem.Choices.Add('atlas');
  MetaDataItem.Choices.Add('audio');
  MetaDataItem.Choices.Add('diagram');
  MetaDataItem.Choices.Add('document');
  MetaDataItem.Choices.Add('globe');
  MetaDataItem.Choices.Add('map');
  MetaDataItem.Choices.Add('model');
  MetaDataItem.Choices.Add('multimedia presentation');
  MetaDataItem.Choices.Add('profile');
  MetaDataItem.Choices.Add('raster digital data');
  MetaDataItem.Choices.Add('remote-sensing image');
  MetaDataItem.Choices.Add('section');
  MetaDataItem.Choices.Add('spreadsheet');
  MetaDataItem.Choices.Add('tabular digital data');
  MetaDataItem.Choices.Add('vector digital data');
  MetaDataItem.Choices.Add('video');
  MetaDataItem.Choices.Add('view');

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateGeospatialForm;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

function TframeMetaDataEditor.CreateGnomonic(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Gnomonic -- contains parameters for the Gnomonic projection.';
  MetaDataItem.ShortName := 'gnomonic';
  MetaDataItem.LongName := 'Gnomonic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.7';

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'Projection Parameters';

  MetaDataItem.CreateMethod := CreateGnomonic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLongPerspCenter(MetaDataItem);
  CreateLatPerspCenter(MetaDataItem);
  CreateFalseEasting(MetaDataItem);
  CreateFalseNorthing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGPolygon(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Data Set G-Polygon -- coordinates defining the outline of an area covered by a data set.';
  MetaDataItem.ShortName := 'dsgpoly';
  MetaDataItem.LongName := 'Data Set G-Polygon';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '1.5.2';

  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateGPolygon;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateGPolygonOuterRing(MetaDataItem);
  CreateGPolygonExclusionRing(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGPolygonExclusionRing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TPolygonMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TPolygonMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Data Set G-Polygon Exclusion G-Ring -- the closed nonintersecting boundary of a'
    + ' void area (or “hole” in an interior area) (in decimal degrees). -180 <= Longitude < +180.';
  MetaDataItem.ShortName := 'dsgpolyx';
  MetaDataItem.LongName := 'Data Set G-Polygon Exclusion G-Ring';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '1.5.2.2';
  MetaDataItem.MoreThanOneAllowed := True;
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateGPolygonExclusionRing;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGPolygonOuterRing(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TPolygonMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TPolygonMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Data Set G-Polygon Outer G-Ring -- the closed nonintersecting boundary of an interior area (in decimal degrees). -180 <= Longitude < +180.';
  MetaDataItem.ShortName := 'dsgpolyo';
  MetaDataItem.LongName := 'Data Set G-Polygon Outer G-Ring';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '1.5.2.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreateGPolygonOuterRing;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateGridCoordSystem(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Grid Coordinate System -- a plane-rectangular coordinate system usually based on, and mathematically'
    + ' adjusted to, a map projection so that geographic positions can be readily transformed to and from plane'
    + ' coordinates.';
  MetaDataItem.ShortName := 'gridsys';
  MetaDataItem.LongName := 'Grid Coordinate System';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2';
  MetaDataItem.TextHeight := 135;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'planar';

  MetaDataItem.CreateMethod := CreateGridCoordSystem;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateGridCoordSystemName(MetaDataItem);
  CreateUTM(MetaDataItem);
  CreateUnivPolarStereo(MetaDataItem);
  CreateStatePlaneCoordSys(MetaDataItem);
  CreateArcCoordSys(MetaDataItem);
  CreateOtherCoordSys(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateGridCoordSystemName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoiceMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoiceMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Grid Coordinate System Name -- name of the grid coordinate system.';
  MetaDataItem.ShortName := 'gridsysn';
  MetaDataItem.LongName := 'Grid Coordinate System Name';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.2.1';

  MetaDataItem.Choices.Add('Universal Transverse Mercator');
  MetaDataItem.Choices.Add('Universal Polar Stereographic');
  MetaDataItem.Choices.Add('State Plane Coordinate System 1927');
  MetaDataItem.Choices.Add('State Plane Coordinate System 1983');
  MetaDataItem.Choices.Add('ARC Coordinate System');
  MetaDataItem.Choices.Add('other grid system');

  MetaDataItem.Choice := '';

  MetaDataItem.CreateMethod := CreateGridCoordSystemName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateHeightOfPerspectivePoint(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Height of Perspective Point Above Surface -- height of viewpoint above the Earth, expressed in meters.';
  MetaDataItem.ShortName := 'heightpt';
  MetaDataItem.LongName := 'Height of Perspective Point Above Surface';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.2.1.23.7';

  MetaDataItem.Content := 0;
  MetaDataItem.MinValue := 0;
  MetaDataItem.MinLimitType := mltGreater;

  MetaDataItem.CreateMethod := CreateHeightOfPerspectivePoint;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateHighestBPS(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TIntegerMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TIntegerMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Highest BPS -- highest speed for the connection''s communication, expressed in bits per second. Used in cases when a range of rates are provided.';
  MetaDataItem.ShortName := 'highbps';
  MetaDataItem.LongName := 'Highest BPS';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '6.4.2.2.1.1.2.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.Content := 111;
  MetaDataItem.MinValue := 110;

//  MetaDataItem.MoreThanOneAllowed := True;

//  MetaDataItem.CheckType := ctRadioButton;
//  MetaDataItem.RadioGroupName := 'computer';

  MetaDataItem.CreateMethod := CreateHighestBPS;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateHorizontalCoordSysDef(Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Coordinate System Definition -- the reference frame or system from which linear or angular quantities are measured and assigned to the position that a point occupies.';
  MetaDataItem.ShortName := 'horizsys';
  MetaDataItem.LongName := 'Horizontal Coordinate System Definition';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.1';
  MetaDataItem.TextHeight := 115;

  MetaDataItem.CreateMethod := CreateHorizontalCoordSysDef;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateHorizontalCoordSysGeographic(MetaDataItem);
  CreatePlanar(MetaDataItem);
  CreateLocal(MetaDataItem);
  CreateGeodeticModel(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHorizontalCoordSysGeographic(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Geographic -- the quantities of latitude and longitude which define the position of a point on the Earth''s surface with respect to a reference spheroid.';
  MetaDataItem.ShortName := 'geograph';
  MetaDataItem.LongName := 'Geographic';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '4.1.1';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CheckType := ctRadioButton;
  MetaDataItem.RadioGroupName := 'horizsys';

  MetaDataItem.CreateMethod := CreateHorizontalCoordSysGeographic;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  AnItem.OnPaint := CheckRadioButton;
  MetaDataItem.OnUsedChanged := RadioButtonChanged;

  CreateLatitudeResolution(MetaDataItem);
  CreateLongitudeResolution(MetaDataItem);
  CreateGeoCoordUnits(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHorizontalDatumName(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TChoicePlusMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TChoicePlusMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Datum Name -- the identification given to the reference system used for defining the coordinates of points.';
  MetaDataItem.ShortName := 'horizdn';
  MetaDataItem.LongName := 'Horizontal Datum Name';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '4.1.4.1';

  MetaDataItem.Choices.Add('North American Datum of 1927');
  MetaDataItem.Choices.Add('North American Datum of 1983');

  MetaDataItem.CreateMethod := CreateHorizontalDatumName;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;
end;

function TframeMetaDataEditor.CreateHorizPositionalAccuracy(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TCompoundItemMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TCompoundItemMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Positional Accuracy -- an estimate of accuracy of the horizontal positions of the spatial objects.';
  MetaDataItem.ShortName := 'horizpa';
  MetaDataItem.LongName := 'Horizontal Positional Accuracy';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtManditoryIfApplicable;
  MetaDataItem.ID := '2.4.1';

  MetaDataItem.CreateMethod := CreateHorizPositionalAccuracy;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  CreateHorizPositionalAccuracyReport(MetaDataItem);
  CreateQuantitativeHorizPositionalAccuracy(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHorizPositionalAccuracyExplanation(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Positional Accuracy Explanation -- the identification of the test that yielded the Horizontal Positional Accuracy Value.';
  MetaDataItem.ShortName := 'horizpae';
  MetaDataItem.LongName := 'Horizontal Positional Accuracy Explanation';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.1.2.2';
  MetaDataItem.TextHeight := 90;

  MetaDataItem.CreateMethod := CreateHorizPositionalAccuracyExplanation;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHorizPositionalAccuracyReport(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Positional Accuracy Report -- an explanation of the accuracy of the horizontal coordinate measurements and a description of the tests used.';
  MetaDataItem.ShortName := 'horizpar';
  MetaDataItem.LongName := 'Horizontal Positional Accuracy Report';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.1.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreateHorizPositionalAccuracyReport;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHorizPositionalAccuracyValue(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TNumericMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TNumericMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Horizontal Positional Accuracy Value -- an estimate of the accuracy of the'
    + ' horizontal coordinate measurements in the data set expressed in (ground)'
    + ' meters.';
  MetaDataItem.ShortName := 'horizpav';
  MetaDataItem.LongName := 'Horizontal Positional Accuracy Value';
  MetaDataItem.Required := True;
  MetaDataItem.RequiredType := rtManditory;
  MetaDataItem.ID := '2.4.1.2.1';
  MetaDataItem.TextHeight := 95;

  MetaDataItem.CreateMethod := CreateHorizPositionalAccuracyValue;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem;

end;

function TframeMetaDataEditor.CreateHoursOfService(
  Parent: TCustomMetaDataItem): TCustomMetaDataItem;
var
  AnItem: TTreeViewItem;
  MetaDataItem: TTextMetaDataItem;
begin
  AnItem := CreateTreeViewItem(Parent);

  MetaDataItem := TTextMetaDataItem.Create(AnItem);
  MetaDataItem.Description := 'Hours of Service -- time period when individuals can speak to the organization or individual.';
  MetaDataItem.ShortName := 'hours';
  MetaDataItem.LongName := 'Hours of Service';
  MetaDataItem.Required := False;
  MetaDataItem.RequiredType := rtOptional;
  MetaDataItem.ID := '10.9';

//  MetaDataItem.MoreThanOneAllowed := True;

  MetaDataItem.CreateMethod := CreateHoursOfService;
  AssignMetaDataToTreeViewItem(MetaDataItem);

  result := MetaDataItem
end;

procedure TframeMetaDataEditor.DuplicateItem(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  ATreeViewItem: TTreeViewItem;
  MetaData: TCustomMetaDataItem;
begin
  ATreeViewItem := Sender as TTreeViewItem;
  MetaData := ATreeViewItem.TagObject as TCustomMetaDataItem;
  btnDuplicate.Enabled := MetaData.MoreThanOneAllowed;
  btnDuplicate.ImageIndex := 1 - Ord(MetaData.MoreThanOneAllowed);
  if MetaData.MoreThanOneAllowed and (X > 20) and (X < 36) then
  begin
    MetaData.NewItem(ATreeViewItem.ParentItem.TagObject as TCustomMetaDataItem)
  end;
end;

procedure TframeMetaDataEditor.tmr1Timer(Sender: TObject);
begin
  if Assigned(FRadioButton) and FRadioButton.IsChecked then
  begin
    FRadioButton.IsChecked := False;
    tmr1.Enabled := False;
  end;
end;

procedure TframeMetaDataEditor.tvMetaDataChange(Sender: TObject);
begin
  if tvMetaData.Selected <> nil then
  begin
    SelectedItem := tvMetaData.Selected.TagObject as TCustomMetaDataItem;
  end;
end;

procedure TframeMetaDataEditor.tvMetaDataChangeCheck(Sender: TObject);
var
  Item: TTreeViewItem;
  MetaDataItem: TCustomMetaDataItem;
begin
  Item := Sender as TTreeViewItem;
  MetaDataItem := Item.TagObject as TCustomMetaDataItem;
  MetaDataItem.Used := Item.IsChecked;
  Item.IsChecked := MetaDataItem.Used;
end;

end.
