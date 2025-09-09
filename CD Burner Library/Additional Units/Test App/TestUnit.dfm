object Form1: TForm1
  Left = 378
  Top = 130
  Width = 500
  Height = 346
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 5
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Add Track'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Save Track'
    TabOrder = 1
    OnClick = Button2Click
  end
  object TrackListBox: TListBox
    Left = 16
    Top = 62
    Width = 467
    Height = 237
    ItemHeight = 13
    TabOrder = 2
  end
  object Button3: TButton
    Left = 194
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Refresh'
    TabOrder = 3
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Left = 404
    Top = 7
  end
  object SaveDialog1: TSaveDialog
    Left = 348
    Top = 9
  end
end
