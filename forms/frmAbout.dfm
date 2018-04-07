object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077'...'
  ClientHeight = 144
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object lProjectName: TLabel
    Left = 52
    Top = 8
    Width = 85
    Height = 16
    Caption = 'lProjectName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lVersion: TLabel
    Left = 52
    Top = 27
    Width = 37
    Height = 13
    Caption = 'lVersion'
  end
  object lAuthorComment: TLabel
    Left = 8
    Top = 50
    Width = 38
    Height = 13
    Caption = #1040#1074#1090#1086#1088': '
  end
  object lAuthor: TLabel
    Left = 52
    Top = 50
    Width = 102
    Height = 13
    Cursor = crHandPoint
    Caption = #1042#1080#1090#1072#1083#1080#1081' '#1055#1077#1083#1077#1093#1072#1090#1099#1081
    OnClick = lAuthorClick
    OnMouseEnter = lAuthorMouseEnter
    OnMouseLeave = lAuthorMouseLeave
  end
  object lLicense: TLabel
    Left = 8
    Top = 79
    Width = 412
    Height = 30
    AutoSize = False
    Caption = 
      #1055#1088#1086#1075#1088#1072#1084#1084#1085#1099#1081' '#1087#1088#1086#1076#1091#1082#1090' '#1088#1072#1089#1087#1088#1086#1089#1090#1088#1072#1085#1103#1077#1090#1089#1103' "'#1050#1040#1050' '#1045#1057#1058#1068'", '#1041#1045#1047' '#1050#1040#1050#1048#1061'-'#1051#1048#1041#1054' ' +
      #1043#1040#1056#1040#1053#1058#1048#1049', '#1082#1072#1082' '#1103#1074#1085#1099#1093', '#1090#1072#1082' '#1080' '#1087#1088#1077#1076#1087#1086#1083#1072#1075#1072#1077#1084#1099#1093
    WordWrap = True
  end
  object lFreeware: TLabel
    Left = 8
    Top = 120
    Width = 283
    Height = 13
    Caption = #1045#1089#1083#1080' '#1042#1099' '#1079#1072#1087#1083#1072#1090#1080#1083#1080' '#1079#1072' '#1087#1088#1086#1075#1088#1072#1084#1084#1091', '#1079#1085#1072#1095#1080#1090' '#1042#1072#1089' '#1086#1073#1084#1072#1085#1091#1083#1080'!'
  end
end
