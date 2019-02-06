object FrmDamDialog: TFrmDamDialog
  Left = 285
  Top = 124
  BorderStyle = bsDialog
  ClientHeight = 128
  ClientWidth = 409
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Ico: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object LbMsg: THTLabel
    Left = 48
    Top = 8
    Width = 353
    Height = 73
    Text = ''
  end
  object BoxButtons: TPanel
    Left = 0
    Top = 89
    Width = 409
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object BoxFloatBtns: TPanel
      Left = 0
      Top = 0
      Width = 249
      Height = 39
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object Btn1: TBitBtn
        Left = 8
        Top = 8
        Width = 73
        Height = 25
        TabOrder = 0
      end
      object Btn2: TBitBtn
        Left = 88
        Top = 8
        Width = 73
        Height = 25
        TabOrder = 1
      end
      object Btn3: TBitBtn
        Left = 168
        Top = 8
        Width = 73
        Height = 25
        TabOrder = 2
      end
    end
  end
  object AL: TActionList
    Left = 16
    Top = 64
    object Action_Copy: TAction
      Caption = 'Action_Copy'
      ShortCut = 16451
      OnExecute = Action_CopyExecute
    end
  end
end
