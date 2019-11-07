
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeReg;

interface

{$I OFFICEVER.INC}

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
     {$IFDEF OVCL_D6} DesignEditors, DesignIntf {$ELSE} DsgnIntf {$ENDIF};

procedure Register;

implementation {===============================================================}

uses AutoCombo, ColorCombo, ColorCombo2000, ComboBox, ComboEdit, OfficeDateTime,
  OfficeEdit, FontCombo, MruCombo, SpectrumCombo, TrackCombo, TreeCombo,
  GlyphCombo, OfficeBar, OfficeControls, OfficeHint, OfficeHintDsgn,
  OfficeBalloon, OfficeButtons, OfficePanel, OfficeToolBar;

{$R officereg.dcr}

procedure Register;
begin
  RegisterComponents('OfficeVCL', [
    TksoOfficeDock,
    TksoOfficeToolBar,

    TksoOfficeButton,
    TksoOfficeSpeedButton,
    TksoOfficePanel,
    TksoOfficeLabel,
    TksoOfficeMemo,
    TksoOfficeListBox,
    TksoOfficeCheckBox,
    TksoOfficeRadioButton,
    TksoOfficeGroupBox,
    TksoOfficeCheckListBox,

    TksoComboBox,
    TksoAutoFillComboBox,
    TksoColorComboBox,
    TksoColorComboBox2000,
    TksoDateTimePicker,
    TksoEdit,
    TksoFontComboBox,
    TksoGlyphComboBox,
    TksoMRUComboBox,
    TksoSpectrumComboBox,
    TksoTrackComboBox,
    TksoTreeComboBox,

    TksoOfficeHint,

    TksoBalloonForm,
    TksoBalloonButton,
    TksoOfficeBalloon
  ]);

  RegisterClass(TksoComboBox);

  RegisterPropertyEditor(TypeInfo(TksoHintShape), TksoOfficeHint, '', TksoHintShapeProperty);
end;

end.
