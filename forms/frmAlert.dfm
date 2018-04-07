object AlertForm: TAlertForm
  Left = 0
  Top = 0
  AlphaBlend = True
  BorderStyle = bsNone
  Caption = 'AlertForm'
  ClientHeight = 75
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object lTitle: TLabel
    Left = 52
    Top = 8
    Width = 28
    Height = 13
    Caption = 'lTitle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lMsg: TLabel
    Left = 52
    Top = 27
    Width = 275
    Height = 40
    AutoSize = False
    Caption = 'lMsg'
    WordWrap = True
  end
  object LiveTimer: TTimer
    Enabled = False
    OnTimer = LiveTimerTimer
    Left = 154
    Top = 10
  end
end
