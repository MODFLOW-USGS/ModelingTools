object FormNamingConvention: TFormNamingConvention
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Naming Convention'
  ClientHeight = 281
  ClientWidth = 560
  Color = clInactiveCaptionText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 18
  object JvRichEdit1: TJvRichEdit
    Left = 0
    Top = 0
    Width = 560
    Height = 249
    TabStop = False
    AdvancedTypography = True
    Align = alTop
    AutoAdvancedTypography = False
    AutoSize = False
    BevelOuter = bvSpace
    BorderStyle = bsNone
    Color = clInactiveCaptionText
    Flat = True
    Lines.Strings = (
      ''
      
        'User-specified names for parameters, observations, predictions, ' +
        'prior-information '
      'items, and groups need to conform to the following two rules:'
      ''
      
        'Rule 1. The first character needs to be a letter of the English ' +
        'alphabet.'
      ''
      
        'Rule 2. All characters after the first letter need to be a lette' +
        'r, digit, or member of '
      
        'the set: "_", ".", ":", "&", "#", "@" (underscore, dot, colon, a' +
        'mpersand, number '
      'sign, at symbol).'
      ''
      
        'Names of parameters and all groups are limited to 12 characters.' +
        ' Names of '
      
        'observations, predictions, and prior-information items are limit' +
        'ed to 20 characters.')
    ParentFlat = False
    ReadOnly = True
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 242
    Top = 249
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
end
