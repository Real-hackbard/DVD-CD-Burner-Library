object Form3: TForm3
  Left = 698
  Top = 113
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1079#1072#1087#1080#1089#1080
  ClientHeight = 200
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 65
    Height = 13
    Caption = #1052#1077#1090#1082#1072' '#1076#1080#1089#1082#1072
  end
  object SpeedButton1: TSpeedButton
    Left = 136
    Top = 168
    Width = 121
    Height = 22
    Caption = #1055#1088#1080#1085#1103#1090#1100
    Flat = True
    OnClick = SpeedButton1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 48
    Width = 249
    Height = 21
    BevelKind = bkSoft
    BorderStyle = bsNone
    TabOrder = 0
    Text = 'DataCD'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 80
    Width = 233
    Height = 17
    Caption = #1060#1080#1085#1072#1083#1080#1079#1080#1088#1086#1074#1072#1090#1100' '#1076#1080#1089#1082
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 104
    Width = 233
    Height = 17
    Caption = #1048#1079#1074#1083#1077#1095#1100' '#1087#1086#1089#1083#1077' '#1054#1095#1080#1089#1090#1082#1080'/'#1047#1072#1087#1080#1089#1080
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
