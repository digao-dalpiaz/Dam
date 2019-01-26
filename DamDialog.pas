unit DamDialog;

interface

uses Vcl.Forms, System.Classes, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, HTLabel,
  //
  DamUnit;

{$R Dam_Resource.res}

type
  TFrmDamDialog = class(TForm)
    Ico: TImage;
    BoxButtons: TPanel;
    Btn2: TBitBtn;
    Btn3: TBitBtn;
    Btn1: TBitBtn;
    AL: TActionList;
    Action_Copy: TAction;
    LbMsg: THTLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Action_CopyExecute(Sender: TObject);
  private
    DamMsg: TDamMsg;
    DamResult: Byte;
    NumButtons: Byte;

    LangStrs: record
      OK, Yes, No, Info, Quest, Warn, Error, Msg: String;
    end;

    procedure CalcHeight;
    procedure RealignButtons;
    procedure LoadLanguage;

    function GetIconTitle(I: TDamMsgIcon): String;
    procedure DoSound;
  end;

function RunDamDialog(aDamMsg: TDamMsg; const aText: String): TDamMsgRes;

implementation

{$R *.dfm}

uses Winapi.Windows, System.SysUtils, Vcl.Clipbrd, System.IniFiles,
  Winapi.MMSystem;

function RunDamDialog(aDamMsg: TDamMsg; const aText: String): TDamMsgRes;
var F: TFrmDamDialog;
begin
    F := TFrmDamDialog.Create(Application);
    try
      F.DamMsg := aDamMsg;
      F.LbMsg.Text := aText;

      F.ShowModal;
      Result := F.DamResult;
    finally
      F.Free;
    end;
end;

//

procedure TFrmDamDialog.FormCreate(Sender: TObject);
begin
    LbMsg.Anchors := [akLeft, akRight, akTop, akBottom];

    Btn1.ModalResult := 101;
    Btn2.ModalResult := 102;
    Btn3.ModalResult := 103;
end;

procedure TFrmDamDialog.CalcHeight;
begin
    if LbMsg.TextHeight>LbMsg.Height then
      ClientHeight := LbMsg.TextHeight+(ClientHeight-LbMsg.Height);
end;

//

function TFrmDamDialog.GetIconTitle(I: TDamMsgIcon): String;
begin
    Result := '';
    case I of
      diApp   : Result := Application.Title;
      diInfo  : Result := LangStrs.Info;
      diQuest : Result := LangStrs.Quest;
      diWarn  : Result := LangStrs.Warn;
      diError : Result := LangStrs.Error;
      diCustom: Result := LangStrs.Msg;
    end;
end;

function GetNumButtons(B: TDamMsgButtons): Byte;
begin
    Result := 0;
    case B of
      dbOne, dbOK: Result := 1;
      dbTwo, dbYesNo: Result := 2;
      dbThree: Result := 3;
    end;
end;

procedure TFrmDamDialog.DoSound;
begin
    case DamMsg.Icon of
      diQuest: PlaySound('SYSTEMQUESTION', 0, SND_ASYNC);
      diWarn: PlaySound('SYSTEMEXCLAMATION', 0, SND_ASYNC);
      diError: PlaySound('SYSTEMHAND', 0, SND_ASYNC);
    end;
end;

//

procedure TFrmDamDialog.FormShow(Sender: TObject);
begin
    LoadLanguage;
    //

    if DamMsg.Dam.PlaySounds then DoSound;

    case DamMsg.Title of
      dtApp       : Caption := Application.Title;
      dtParentForm: Caption := TForm(DamMsg.Dam.Owner).Caption;
      dtMainForm  : Caption := Application.MainForm.Caption;
      dtByIcon    : Caption := GetIconTitle(DamMsg.Icon);
      dtCustom    : Caption := DamMsg.CustomTitle;
    end;

    case DamMsg.Icon of
      diApp   : Ico.Picture.Icon := Application.Icon;
      diInfo  : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_INFORMATION);
      diQuest : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
      diWarn  : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_WARNING);
      diError : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
      diCustom: Ico.Picture.Icon := DamMsg.CustomIcon;
    end;

    LbMsg.Font.Assign(DamMsg.Dam.MessageFont);
    CalcHeight;

    Btn1.Caption := DamMsg.Button1;
    Btn2.Caption := DamMsg.Button2;
    Btn3.Caption := DamMsg.Button3;

    case DamMsg.Buttons of
      dbOK: Btn1.Caption := LangStrs.OK;
      dbYesNo:
        begin
            Btn1.Caption := '&'+LangStrs.Yes;
            Btn2.Caption := '&'+LangStrs.No;
        end;
    end;

    NumButtons := GetNumButtons(DamMsg.Buttons);

    Btn2.Visible := NumButtons>1;
    Btn3.Visible := NumButtons>2;

    RealignButtons;

    case NumButtons of //is the last button
      1: Btn1.Cancel := True;
      2: Btn2.Cancel := True;
      3: Btn3.Cancel := True;
    end;

    if DamMsg.SwapFocus then
      case NumButtons of
        2: ActiveControl := Btn2;
        3: ActiveControl := Btn3;
      end;

end;

procedure TFrmDamDialog.RealignButtons;
begin
    case NumButtons of
      1: Btn1.Left := Btn3.Left;
      2: begin
           Btn1.Left := Btn2.Left;
           Btn2.Left := Btn3.Left;
         end;
    end;
end;

procedure TFrmDamDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    case ModalResult of
      101: DamResult := 1;
      102: DamResult := 2;
      103: DamResult := 3;
      else DamResult := NumButtons; //** last button
    end;
end;

procedure TFrmDamDialog.Action_CopyExecute(Sender: TObject);
begin
    Clipboard.AsText := LbMsg.Text;
end;

procedure TFrmDamDialog.LoadLanguage;
var aLang: String;
    R: TResourceStream;
    S: TStringList;
    Ini: TMemIniFile;
begin
    case DamMsg.Dam.Language of
      dgEnglish: aLang := 'English';
      dgPortuguese: aLang := 'Portuguese';
      dgSpanish: aLang := 'Spanish';
      dgGerman: aLang := 'German';
      dgItalian: aLang := 'Italian';
      dgChinese: aLang := 'Chinese';
      dgJapanese: aLang := 'Japanese';
      dgGreek: aLang := 'Greek';
      dgRussian: aLang := 'Russian';
      dgFrench: aLang := 'French';
      else raise Exception.Create('Unknown language');
    end;

    S := TStringList.Create;
    try
      R := TResourceStream.Create(FindClassHInstance(TDam), 'DAM_LANG', RT_RCDATA);
      try
        S.LoadFromStream(R);
      finally
        R.Free;
      end;

      Ini := TMemIniFile.Create('');
      try
        Ini.SetStrings(S);
        S.Clear;
        Ini.ReadSectionValues(aLang, S);
      finally
        Ini.Free;
      end;

      if S.Count=0 then
        raise Exception.CreateFmt('Language "%s" not found in resource', [aLang]);

      with LangStrs do
      begin
        OK := S.Values['OK'];
        Yes := S.Values['Yes'];
        No := S.Values['No'];
        Info := S.Values['Info'];
        Quest := S.Values['Quest'];
        Warn := S.Values['Warn'];
        Error := S.Values['Error'];
        Msg := S.Values['Msg'];
      end;

    finally
      S.Free;
    end;
end;

end.
