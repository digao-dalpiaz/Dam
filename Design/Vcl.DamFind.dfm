object FrmDamFind: TFrmDamFind
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Find Dam Message'
  ClientHeight = 382
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 107
    Height = 13
    Caption = 'Message name or text'
  end
  object EdText: TEdit
    Left = 8
    Top = 24
    Width = 409
    Height = 21
    TabOrder = 0
    OnChange = EdTextChange
  end
  object L: TListBox
    Left = 8
    Top = 56
    Width = 409
    Height = 193
    ItemHeight = 13
    TabOrder = 1
    OnClick = LClick
    OnDblClick = LDblClick
  end
  object BoxDIO: TPanel
    Left = 128
    Top = 352
    Width = 169
    Height = 33
    BevelOuter = bvNone
    TabOrder = 3
    object BtnCancel: TButton
      Left = 88
      Top = 0
      Width = 73
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object BtnOK: TButton
      Left = 8
      Top = 0
      Width = 73
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = BtnOKClick
    end
  end
  object EdMessage: TMemo
    Left = 8
    Top = 256
    Width = 409
    Height = 89
    TabStop = False
    Color = clInfoBk
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
