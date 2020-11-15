unit DamDialog;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF FPC}
  Forms, Classes, ActnList, Buttons, Controls, ExtCtrls,
{$ELSE}
  Vcl.Forms, System.Classes, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls,
{$ENDIF}
  //
  DamUnit, DzHTMLText;

{$R Dam_Resource.res}

type
  TFrmDamDialog = class(TForm)
    Ico: TImage;
    BoxButtons: TPanel;
    AL: TActionList;
    Action_Copy: TAction;
    LbMsg: TDzHTMLText;
    BoxFloatBtns: TPanel;
    Btn1: TBitBtn;
    Btn2: TBitBtn;
    Btn3: TBitBtn;
    BtnHelp: TSpeedButton;
    Action_Help: TAction;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Action_CopyExecute(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);
  private
    DamMsg: TDamMsg;
    DamResult: Byte;
    NumButtons: Byte;

    LangStrs: record
      OK, Yes, No, Info, Quest, Warn, Error, Msg: string;
    end;

    procedure LoadText(const aText: string);
    procedure CalcWidth(const aText: string);
    procedure CalcHeight;
    procedure RealignButtons;
    procedure LoadLanguage;

    function GetIconTitle(I: TDamMsgIcon): string;
    procedure DoSound;

    procedure SetFormCustomization;
    procedure LoadHelp;
  end;

function RunDamDialog(aDamMsg: TDamMsg; const aText: string): TDamMsgRes;

implementation

{$R *.dfm}

uses
{$IFDEF FPC}
  Windows, SysUtils, Clipbrd, IniFiles, MMSystem;
{$ELSE}
  Winapi.Windows, System.SysUtils, Vcl.Clipbrd, System.IniFiles,
  Winapi.MMSystem, System.Math, Vcl.Themes;
{$ENDIF}

{$IFDEF FPC}
const
  {$EXTERNALSYM IDI_HAND}
  IDI_HAND = MakeIntResource(32513);
  {$EXTERNALSYM IDI_QUESTION}
  IDI_QUESTION = MakeIntResource(32514);
  {$EXTERNALSYM IDI_EXCLAMATION}
  IDI_EXCLAMATION = MakeIntResource(32515);
  {$EXTERNALSYM IDI_ASTERISK}
  IDI_ASTERISK = MakeIntResource(32516);
  {$EXTERNALSYM IDI_WINLOGO}
  IDI_WINLOGO = MakeIntResource(32517);
  {$EXTERNALSYM IDI_WARNING}
  IDI_WARNING = IDI_EXCLAMATION;
  {$EXTERNALSYM IDI_ERROR}
  IDI_ERROR = IDI_HAND;
  {$EXTERNALSYM IDI_INFORMATION}
  IDI_INFORMATION = IDI_ASTERISK;
{$ENDIF}

function RunDamDialog(aDamMsg: TDamMsg; const aText: string): TDamMsgRes;
var F: TFrmDamDialog;
begin
  F := TFrmDamDialog.Create(Application);
  try
    F.DamMsg := aDamMsg;

    F.LoadText(aText);
    F.SetFormCustomization;

    F.ShowModal;
    Result := F.DamResult;
  finally
    F.Free;
  end;
end;

//

procedure TFrmDamDialog.FormCreate(Sender: TObject);
begin
  Btn1.ModalResult := 101;
  Btn2.ModalResult := 102;
  Btn3.ModalResult := 103;

  {$IFNDEF FPC}
  //when using app custom style theme, the DzHTMLText doesn't get theme backgound color
  if TStyleManager.IsCustomStyleActive then
    LbMsg.Color := TStyleManager.ActiveStyle.GetStyleColor(TStyleColor.scWindow);
  {$ENDIF}
end;

procedure TFrmDamDialog.LoadText(const aText: string);
begin
  LbMsg.Images := DamMsg.Dam.Images;
  LbMsg.Font.Assign(DamMsg.Dam.MessageFont);
  CalcWidth(aText);
  CalcHeight;
end;

procedure TFrmDamDialog.CalcWidth(const aText: string);
const MinSize=300;
begin
  if DamMsg.FixedWidth=0 then
    LbMsg.Width := Trunc(Monitor.Width * 0.75) //max width
  else
    LbMsg.Width := DamMsg.FixedWidth;

  LbMsg.Text := aText; //set MESSAGE TEXT

  if (DamMsg.FixedWidth=0) and (LbMsg.TextWidth < LbMsg.Width) then
    LbMsg.Width := Max(LbMsg.TextWidth, MinSize);

  ClientWidth := LbMsg.Left+LbMsg.Width+8;
end;

procedure TFrmDamDialog.CalcHeight;
var Dif: Integer;
begin
  Dif := ClientHeight-LbMsg.Height;

  LbMsg.Height := LbMsg.TextHeight;
  ClientHeight := Max(LbMsg.Height, Ico.Height)+Dif;

  if LbMsg.Height<Ico.Height then //text smaller than icon
    LbMsg.Top := LbMsg.Top + ((Ico.Height-LbMsg.Height) div 2);
end;

procedure TFrmDamDialog.SetFormCustomization;
var F: TForm;
  R: TRect;
begin
  if not DamMsg.Dam.DialogBorder then
    BorderStyle := bsNone;

  case DamMsg.Dam.DialogPosition of
    dpScreenCenter: {Position := poScreenCenter}; //default
    dpActiveFormCenter:
      begin
        F := Screen.ActiveForm;
        if Assigned(F) then
        begin
          Position := poDesigned;

          if not GetWindowRect(F.Handle, R) then
            raise Exception.Create('Error getting window rect');

          Left := R.Left + ((R.Width - Width) div 2);
          Top := R.Top + ((R.Height - Height) div 2);
        end;
      end;
    dpMainFormCenter: Position := poMainFormCenter;
    else raise Exception.Create('Invalid dialog position property');
  end;
end;

//

function TFrmDamDialog.GetIconTitle(I: TDamMsgIcon): string;
begin
  case I of
    diApp   : Result := Application.Title;
    diInfo  : Result := LangStrs.Info;
    diQuest : Result := LangStrs.Quest;
    diWarn  : Result := LangStrs.Warn;
    diError : Result := LangStrs.Error;
    diCustom: Result := LangStrs.Msg;
    else raise Exception.Create('Unknown icon kind property');
  end;
end;

function GetNumButtons(B: TDamMsgButtons): Byte;
begin
  case B of
    dbOne, dbOK: Result := 1;
    dbTwo, dbYesNo: Result := 2;
    dbThree: Result := 3;
    else raise Exception.Create('Unknown buttons kind property');
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

  Color := DamMsg.Dam.MessageColor;
  BoxButtons.Color := DamMsg.Dam.ButtonsColor;

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

  LoadHelp;
  RealignButtons;
end;

procedure TFrmDamDialog.RealignButtons;
var LastBtn: TBitBtn;
begin
  Btn2.Visible := NumButtons>1;
  Btn3.Visible := NumButtons>2;

  LastBtn := nil;
  case NumButtons of
    1: LastBtn := Btn1;
    2: LastBtn := Btn2;
    3: LastBtn := Btn3;
  end;

  BoxFloatBtns.Width := LastBtn.Left+LastBtn.Width+8;
  if DamMsg.Dam.CenterButtons then
    BoxFloatBtns.Left := (BoxButtons.Width-BoxFloatBtns.Width) div 2 //center
  else
    BoxFloatBtns.Left := BoxButtons.Width-BoxFloatBtns.Width; //right

  LastBtn.Cancel := True; //set last button as cancel (esc) button
  if DamMsg.SwapFocus then ActiveControl := LastBtn; //set last button as start focus button
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
var aLang: string;
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
    dgPolish: aLang := 'Polish';
    dgDutch: aLang := 'Dutch';
    else raise Exception.Create('Unknown language');
  end;

  S := TStringList.Create;
  try
    R := TResourceStream.Create({$IFDEF FPC}HInstance{$ELSE}FindClassHInstance(TDam){$ENDIF}, 'DAM_LANG', RT_RCDATA);
    try
      S.LoadFromStream(R, TEncoding.UTF8);
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

procedure TFrmDamDialog.LoadHelp;
begin
  BtnHelp.Visible := (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr);
end;

procedure TFrmDamDialog.BtnHelpClick(Sender: TObject);
begin
  if DamMsg.HelpContext<>0 then
    Application.HelpContext(DamMsg.HelpContext)
  else
  if DamMsg.HelpKeyword<>EmptyStr then
    Application.HelpKeyword(DamMsg.HelpKeyword)
  else
    raise Exception.Create('Unknown help property');
end;

procedure TFrmDamDialog.Action_HelpExecute(Sender: TObject);
begin
  if BtnHelp.Visible then
    BtnHelp.Click;
end;

end.
