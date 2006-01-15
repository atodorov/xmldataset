object MainForm: TMainForm
  Left = 276
  Top = 417
  Width = 331
  Height = 265
  Caption = 'Simple TFTP-Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    323
    238)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 184
    Width = 25
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Path:'
  end
  object Log: TMemo
    Left = 8
    Top = 8
    Width = 306
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BExit: TButton
    Left = 244
    Top = 208
    Width = 70
    Height = 25
    Anchors = [akBottom]
    Caption = 'E&xit'
    TabOrder = 1
    OnClick = BExitClick
  end
  object BAbout: TButton
    Left = 7
    Top = 208
    Width = 70
    Height = 25
    Anchors = [akBottom]
    Caption = '&About'
    TabOrder = 2
    OnClick = BAboutClick
  end
  object PathEdit: TEdit
    Left = 40
    Top = 181
    Width = 273
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Text = 'C:\'
  end
end
