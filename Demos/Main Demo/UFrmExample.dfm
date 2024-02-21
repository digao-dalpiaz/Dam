object FrmExample: TFrmExample
  Left = 0
  Top = 0
  Caption = 'Dam Example'
  ClientHeight = 265
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 232
    Top = 128
    Width = 51
    Height = 13
    Caption = 'Your name'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 33
    Caption = 'Quick Message Information'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 48
    Width = 201
    Height = 33
    Caption = 'Quick Message Question'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 88
    Width = 201
    Height = 33
    Caption = 'Object Message Information'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 128
    Width = 201
    Height = 33
    Caption = 'Object Message Question'
    TabOrder = 3
    OnClick = Button4Click
  end
  object EdName: TEdit
    Left = 232
    Top = 144
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'Jacob'
  end
  object Button5: TButton
    Left = 8
    Top = 168
    Width = 201
    Height = 33
    Caption = 'Quick Raise'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 208
    Width = 201
    Height = 33
    Caption = 'Object Raise Message'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Dam1: TDam
    Language = dgEnglish
    HandleExceptions = True
    DamDefault = True
    DamUnitName = 'MsgDialogs'
    Left = 272
    Top = 40
    object _InfoGoodJob: TDamMsg
      Message = 
        'You are doing a <b>good job</b>!'#13#10#13#10'<i>Please, continue your wor' +
        'k.</i>'
      Dam = Dam1
    end
    object _QuestionSaveFile: TDamMsg
      Icon = diQuest
      Message = 
        'Hi <fc:clBlue>%p</fc>, do you want to <u>save the file now</u>?'#13 +
        #10#13#10'<i>Please select Yes to save the file, or select No to cancel' +
        ' this operation.</i>'#13#10#13#10'This operation will be registered with t' +
        'he time: <fc:clRed><b>%p</b></fc>'
      Buttons = dbYesNo
      Dam = Dam1
    end
    object _RaiseLoadingFile: TDamMsg
      Icon = diError
      Message = 
        'Fatal error on loading file "<fc:clRed>%p</fc>".'#13#10#13#10'<b>Please ch' +
        'eck if file exists on disk.</b>'
      RaiseExcept = True
      Dam = Dam1
    end
  end
end
