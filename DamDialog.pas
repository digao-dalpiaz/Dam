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
  DamUnit, DamLanguage, Vcl.DzHTMLText;

type
  TFrmDamDialog = class(TForm)
    Ico: TImage;
    BoxButtons: TPanel;
    AL: TActionList;
    Action_Copy: TAction;
    LbMsg: TDzHTMLText;
    BoxFloatBtns: TPanel;
    BtnHelp: TSpeedButton;
    Action_Help: TAction;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);
    procedure LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink;
      var Handled: Boolean);
  private
    DamMsg: TDamMsg;
    DamResult: TDamMsgRes;
    LangStrs: TDamLanguageDefinition;

    procedure OnBtnClick(Sender: TObject);

    procedure BuildButtons;
    procedure LoadText(const aText: string);
    procedure CalcWidth(const aText: string);
    procedure CalcHeight;
    procedure SetFormCustomization;
    procedure SetTitleAndIcon;
    procedure LoadHelp;

    procedure AlignButtonsPanel;
    procedure DoSound;
  end;

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;

implementation

{$R *.dfm}

uses
{$IFDEF FPC}
  Windows, SysUtils, Clipbrd, MMSystem, Graphics
{$ELSE}
  Winapi.Windows, System.SysUtils, Vcl.Clipbrd,
  Winapi.MMSystem, Vcl.Graphics, System.Math, Vcl.Themes
{$ENDIF};

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

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;
var
  F: TFrmDamDialog;
begin
  F := TFrmDamDialog.Create(Application);
  try
    F.DamMsg := DamMsg;

    F.LangStrs := LoadLanguage(DamMsg.Dam.Language);

    F.BuildButtons;
    F.LoadText(aText);
    F.SetFormCustomization;
    F.SetTitleAndIcon;
    F.LoadHelp;

    F.ShowModal;
    Result := F.DamResult;
  finally
    F.Free;
  end;
end;

//

procedure TFrmDamDialog.FormCreate(Sender: TObject);
begin
  {$IFNDEF FPC}
  //when using app custom style theme, the DzHTMLText doesn't get theme backgound color
  if TStyleManager.IsCustomStyleActive then
    LbMsg.Color := TStyleManager.ActiveStyle.GetStyleColor(TStyleColor.scWindow);
  {$ENDIF}
end;

function GetButtonWidth(Btn: TBitBtn): Integer;
var
  B: {$IFNDEF FPC}Vcl.{$ENDIF}Graphics.TBitmap;
begin
  B := {$IFNDEF FPC}Vcl.{$ENDIF}Graphics.TBitmap.Create;
  try
    B.Canvas.Font.Assign(Btn.Font);
    Result := Max(B.Canvas.TextWidth(Btn.Caption)+20, 75);
  finally
    B.Free;
  end;
end;

procedure TFrmDamDialog.BuildButtons;
var
  NumButtons: Byte;
  I, X: Integer;
  Btn: TBitBtn;
  Names: array[1..3] of string;
begin
  case DamMsg.Buttons of
    dbOne, dbOK: NumButtons := 1;
    dbTwo, dbYesNo: NumButtons := 2;
    dbThree: NumButtons := 3;
    else raise Exception.Create('Unknown buttons kind property');
  end;

  DamResult := NumButtons; //default result - last button

  Names[1] := DamMsg.Button1;
  Names[2] := DamMsg.Button2;
  Names[3] := DamMsg.Button3;

  case DamMsg.Buttons of
    dbOK: Names[1] := LangStrs.OK;
    dbYesNo:
      begin
        Names[1] := '&'+LangStrs.Yes;
        Names[2] := '&'+LangStrs.No;
      end;
  end;

  Btn := nil;

  X := 0;
  for I := 1 to NumButtons do
  begin
    Btn := TBitBtn.Create(Self);
    Btn.Parent := BoxFloatBtns;
    Btn.Caption := Names[I];
    Btn.SetBounds(X, 0, GetButtonWidth(Btn), BoxFloatBtns.Height);
    Btn.OnClick := OnBtnClick;
    Btn.Tag := I;

    Inc(X, Btn.Width+8);
  end;

  //here btn contains last button reference

  Btn.Cancel := True; //set last button as cancel (esc) button
  if DamMsg.SwapFocus then ActiveControl := Btn; //set last button as start focus button

  BoxFloatBtns.Width := Btn.BoundsRect.Right;
end;

procedure TFrmDamDialog.LoadText(const aText: string);
begin
  LbMsg.Images := DamMsg.Dam.Images;
  LbMsg.Font.Assign(DamMsg.Dam.MessageFont);
  CalcWidth(aText);
  CalcHeight;
end;

procedure TFrmDamDialog.CalcWidth(const aText: string);
var
  MinSize: Integer;
begin
  if DamMsg.FixedWidth=0 then
    LbMsg.Width := Trunc(Monitor.Width * 0.75) //max width
  else
    LbMsg.Width := DamMsg.FixedWidth;

  LbMsg.Text := aText; //set MESSAGE TEXT

  if (DamMsg.FixedWidth=0) and (LbMsg.TextWidth < LbMsg.Width) then
  begin
    MinSize := Max(300, BoxFloatBtns.Width);
    LbMsg.Width := Max(LbMsg.TextWidth, MinSize);
  end;

  ClientWidth := LbMsg.BoundsRect.Right+8;
end;

procedure TFrmDamDialog.CalcHeight;
var
  Dif: Integer;
begin
  Dif := ClientHeight-LbMsg.Height;

  LbMsg.Height := LbMsg.TextHeight;
  ClientHeight := Max(LbMsg.Height, Ico.Height)+Dif;

  if LbMsg.Height<Ico.Height then //text smaller than icon
    LbMsg.Top := LbMsg.Top + ((Ico.Height-LbMsg.Height) div 2);
end;

procedure TFrmDamDialog.SetFormCustomization;
var
  F: TForm;
  R: TRect;
begin
  //form border
  if not DamMsg.Dam.DialogBorder then
    BorderStyle := bsNone;

  //form screen position
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

  //form theme colors
  Color := DamMsg.Dam.MessageColor;
  BoxButtons.Color := DamMsg.Dam.ButtonsColor;
end;

procedure TFrmDamDialog.SetTitleAndIcon;

  function GetIconTitle: string;
  begin
    case DamMsg.Icon of
      diApp   : Result := Application.Title;
      diInfo  : Result := LangStrs.Info;
      diQuest : Result := LangStrs.Quest;
      diWarn  : Result := LangStrs.Warn;
      diError : Result := LangStrs.Error;
      diCustom: Result := LangStrs.Msg;
      else raise Exception.Create('Unknown icon kind property');
    end;
  end;

begin
  case DamMsg.Title of
    dtApp       : Caption := Application.Title;
    dtParentForm: Caption := TForm(DamMsg.Dam.Owner).Caption;
    dtMainForm  : Caption := Application.MainForm.Caption;
    dtByIcon    : Caption := GetIconTitle;
    dtCustom    : Caption := DamMsg.CustomTitle;
    else raise Exception.Create('Unknown title kind property');
  end;

  case DamMsg.Icon of
    diApp   : Ico.Picture.Icon := Application.Icon;
    diInfo  : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_INFORMATION);
    diQuest : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
    diWarn  : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_WARNING);
    diError : Ico.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
    diCustom: Ico.Picture.Icon := DamMsg.CustomIcon;
    else raise Exception.Create('Unknown icon kind property');
  end;
end;

procedure TFrmDamDialog.LoadHelp;
begin
  BtnHelp.Visible := (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr);
end;

procedure TFrmDamDialog.AlignButtonsPanel;
begin
  if DamMsg.Dam.CenterButtons then
    BoxFloatBtns.Left := (BoxButtons.Width-BoxFloatBtns.Width) div 2 //center
  else
    BoxFloatBtns.Left := BoxButtons.Width-BoxFloatBtns.Width-8; //right
end;

procedure TFrmDamDialog.DoSound;

  procedure Play(const aSound: string);
  begin
    PlaySound(PChar(aSound), 0, SND_ASYNC);
  end;

begin
  case DamMsg.Icon of
    diQuest: Play('SYSTEMQUESTION');
    diWarn: Play('SYSTEMEXCLAMATION');
    diError: Play('SYSTEMHAND');
  end;
end;

procedure TFrmDamDialog.FormShow(Sender: TObject);
begin
  AlignButtonsPanel;

  if DamMsg.Dam.PlaySounds then
    DoSound;
end;

procedure TFrmDamDialog.Action_CopyExecute(Sender: TObject);
begin
  Clipboard.AsText := LbMsg.Text;
end;

procedure TFrmDamDialog.Action_HelpExecute(Sender: TObject);
begin
  if BtnHelp.Visible then
    BtnHelp.Click;
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

procedure TFrmDamDialog.LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink;
  var Handled: Boolean);
var
  CloseMsg: Boolean;
  ImmediateRes: TDamMsgRes;
begin
  if (Link.Kind = lkLinkRef) and Assigned(DamMsg.Dam.OnLinkClick) then
  begin
    CloseMsg := False;
    ImmediateRes := DamResult;

    DamMsg.Dam.OnLinkClick(DamMsg.Dam, DamMsg,
      Link.LinkRef.Target, Handled, CloseMsg, ImmediateRes);

    if CloseMsg then
    begin
      DamResult := ImmediateRes;
      Close;
    end;
  end;
end;

procedure TFrmDamDialog.OnBtnClick(Sender: TObject);
begin
  DamResult := TBitBtn(Sender).Tag;
  Close;
end;

end.
