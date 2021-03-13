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
  object LbMsg: TDzHTMLText
    Left = 48
    Top = 8
    Width = 353
    Height = 73
    OnLinkClick = LbMsgLinkClick
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
    object BtnHelp: TSpeedButton
      Left = 8
      Top = 8
      Width = 25
      Height = 25
      Caption = '?'
      OnClick = BtnHelpClick
    end
    object BoxFloatBtns: TPanel
      Left = 52
      Top = 8
      Width = 285
      Height = 25
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
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
    object Action_Help: TAction
      Caption = 'Action_Help'
      ShortCut = 112
      OnExecute = Action_HelpExecute
    end
  end
end
