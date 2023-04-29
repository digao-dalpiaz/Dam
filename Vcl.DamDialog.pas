{$IFNDEF FMX}unit Vcl.DamDialog;{$ENDIF}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  {$IFDEF FMX}
  FMX.DamUnit
  {$ELSE}
  Vcl.DamUnit
  {$ENDIF};

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;

implementation

//uses
//{$IFDEF FPC}
//  Windows, SysUtils, Clipbrd, MMSystem, Graphics
//{$ELSE}
//  Winapi.Windows, System.SysUtils, Vcl.Clipbrd,
//  Winapi.MMSystem, Vcl.Graphics, System.Math
//{$ENDIF};

uses
{$IFDEF FPC}
  Forms, Classes, ActnList, Buttons, Controls, ExtCtrls,
{$ELSE}
  System.Math, System.SysUtils, System.Types,
  {$IFDEF FMX}
  FMX.DzHTMLText,
  FMX.Forms, FMX.Objects, FMX.StdCtrls, FMX.ActnList, FMX.Types,
  FMX.Graphics, FMX.Controls,
  {$ELSE}
  Vcl.DzHTMLText,
  Vcl.Forms, System.Classes, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,
  {$ENDIF}
{$ENDIF}
  //
  DamLanguage;

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

type
  TPixels = {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

  TFrmDamDialogDyn = class(TForm)
  private
    Icon: TImage;
    LbMsg: TDzHTMLText;
    BoxButtons, BoxFloatBtns: TPanel;
    BtnHelp: TSpeedButton;
    ActionList: TActionList;

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

    procedure FormShow(Sender: TObject);
    procedure LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink; var Handled: Boolean);
    procedure BtnHelpClick(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);
  public
    constructor Create; reintroduce;
  end;

constructor TFrmDamDialogDyn.Create;
var
  Action: TAction;
begin
  inherited CreateNew(Application);

  OnShow := FormShow;
  {$IFDEF FMX}
  BorderIcons := [];
  {$ENDIF}

  ActionList := TActionList.Create(Self);

  Action := TAction.Create(ActionList);
  Action.ShortCut := 16451; //CTRL+C
  Action.OnExecute := Action_CopyExecute;

  Action := TAction.Create(ActionList);
  Action.ShortCut := 112; //F1
  Action.OnExecute := Action_HelpExecute;

  Icon := TImage.Create(Self);
  Icon.Parent := Self;
  Icon.SetBounds(8, 8, 32, 32);

  LbMsg := TDzHTMLText.Create(Self);
  LbMsg.Parent := Self;
  LbMsg.SetBounds(48, 8, 0, 0);
  LbMsg.OnLinkClick := LbMsgLinkClick;
  {$IFNDEF FMX}
  LbMsg.ParentColor := True;
  {$ENDIF}

  BoxButtons := TPanel.Create(Self);
  BoxButtons.Parent := Self;
  BoxButtons.Height := 39;
  {$IFDEF FMX}
  BoxButtons.Align := TAlignLayout.Bottom;
  {$ELSE}
  BoxButtons.Align := alBottom;
  BoxButtons.BevelOuter := bvNone;
  BoxButtons.ParentBackground := False;
  {$ENDIF}

  BoxFloatBtns := TPanel.Create(Self);
  BoxFloatBtns.Parent := BoxButtons;
  BoxFloatBtns.SetBounds(0, 8, 0, 25);
  {$IFDEF FMX}

  {$ELSE}
  BoxFloatBtns.BevelOuter := bvNone;
  {$ENDIF}

  BtnHelp := TSpeedButton.Create(Self);
  BtnHelp.Parent := BoxButtons;
  BtnHelp.SetBounds(8, 8, 25, 25);
  BtnHelp.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := '?';
  BtnHelp.OnClick := BtnHelpClick;
end;

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;
var
  F: TFrmDamDialogDyn;
begin
  F := TFrmDamDialogDyn.Create;
  try
    {$IFNDEF FMX}
    if (csDesigning in DamMsg.ComponentState) then F.LbMsg.StyleElements := []; //do not use themes in Delphi IDE
    {$ENDIF}

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

function GetDiv2(Value: TPixels): TPixels;
begin
  Result := Value {$IFDEF FMX}/{$ELSE}div{$ENDIF} 2;
end;

procedure SetLeft(Control: TControl; Left: TPixels);
begin
  {$IFDEF FMX}Control.Position.X{$ELSE}Control.Left{$ENDIF} := Left;
end;

procedure SetTop(Control: TControl; Top: TPixels);
begin
  {$IFDEF FMX}Control.Position.Y{$ELSE}Control.Top{$ENDIF} := Top;
end;

function GetButtonWidth(Btn: TButton): TPixels;
var
  B: {$IFDEF DCC}
    {$IFDEF FMX}FMX{$ELSE}Vcl{$ENDIF}.
  {$ENDIF}Graphics.TBitmap;
begin
  B := {$IFDEF DCC}
    {$IFDEF FMX}FMX{$ELSE}Vcl{$ENDIF}.
  {$ENDIF}Graphics.TBitmap.Create;
  try
    B.Canvas.Font.Assign(Btn.Font);
    Result := Max(B.Canvas.TextWidth(Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF})+20, 75);
  finally
    B.Free;
  end;
end;

procedure TFrmDamDialogDyn.BuildButtons;
var
  NumButtons: Byte;
  I: Integer;
  Btn: TButton;
  X: TPixels;
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
    Btn := TButton.Create(Self);
    Btn.Parent := BoxFloatBtns;
    Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Names[I];
    Btn.SetBounds(X, 0, GetButtonWidth(Btn), BoxFloatBtns.Height);
    Btn.OnClick := OnBtnClick;
    Btn.Tag := I;

    X := X + (Btn.Width+8);
  end;

  //here btn contains last button reference

  Btn.Cancel := True; //set last button as cancel (esc) button
  if DamMsg.SwapFocus then ActiveControl := Btn; //set last button as start focus button

  BoxFloatBtns.Width := Btn.BoundsRect.Right;
end;

procedure TFrmDamDialogDyn.LoadText(const aText: string);
begin
  LbMsg.Images := DamMsg.Dam.Images;
  LbMsg.Font.Assign(DamMsg.Dam.MessageFont);
  CalcWidth(aText);
  CalcHeight;
end;

procedure TFrmDamDialogDyn.CalcWidth(const aText: string);
var
  MinSize: {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};
begin
  if DamMsg.FixedWidth=0 then
    LbMsg.Width := Trunc({$IFDEF FMX}Screen.DisplayFromForm(Self).BoundsRect.Width{$ELSE}Monitor.Width{$ENDIF} * 0.75) //max width
  else
    LbMsg.Width := DamMsg.FixedWidth;

  LbMsg.Text := aText; //set MESSAGE TEXT

  if (DamMsg.FixedWidth=0) and (LbMsg.TextWidth < LbMsg.Width) then
  begin
    MinSize := Max(300, BoxFloatBtns.Width);
    LbMsg.Width := Max(LbMsg.TextWidth, MinSize);
  end;

  ClientWidth := Trunc(LbMsg.BoundsRect.Right+8);
end;

procedure TFrmDamDialogDyn.CalcHeight;
var
  Dif: TPixels;
begin
  LbMsg.Height := LbMsg.TextHeight;
  ClientHeight := Trunc(Max(LbMsg.Height, Icon.Height)+14+BoxButtons.Height);

  if LbMsg.Height<Icon.Height then //text smaller than icon
  begin
    Dif := GetDiv2(Icon.Height-LbMsg.Height);
    {$IFDEF FMX}
    LbMsg.Position.Y := LbMsg.Position.Y + Dif;
    {$ELSE}
    LbMsg.Top := LbMsg.Top + Dif;
    {$ENDIF}
  end;
end;

procedure TFrmDamDialogDyn.SetFormCustomization;
var
  F: {$IFDEF FMX}TCommonCustomForm{$ELSE}TForm{$ENDIF};
  R: TRect;
begin
  //form border
  if DamMsg.Dam.DialogBorder then
    BorderStyle := {$IFDEF FMX}TFmxFormBorderStyle.Single{$ELSE}bsDialog{$ENDIF}
  else
    BorderStyle := {$IFDEF FMX}TFmxFormBorderStyle.None{$ELSE}bsNone{$ENDIF};

  //form screen position
  case DamMsg.Dam.DialogPosition of
    dpScreenCenter: Position := {$IFDEF FMX}TFormPosition.ScreenCenter{$ELSE}poScreenCenter{$ENDIF};
    dpActiveFormCenter:
      begin
        F := Screen.ActiveForm;
        if Assigned(F) then
        begin
          Position := {$IFDEF FMX}TFormPosition.Designed{$ELSE}poDesigned{$ENDIF};

          {$IFDEF Defined(MSWINDOWS) and !Defined(FMX)}
          if not GetWindowRect(F.Handle, R) then
            raise Exception.Create('Error getting window rect');
          {$ELSE}
          R := F.ClientRect;
          {$ENDIF}

          Left := Trunc(R.Left + GetDiv2(R.Width - Width));
          Top := Trunc(R.Top + GetDiv2(R.Height - Height));
        end;
      end;
    dpMainFormCenter: Position := {$IFDEF FMX}TFormPosition.MainFormCenter{$ELSE}poMainFormCenter{$ENDIF};
    else raise Exception.Create('Invalid dialog position property');
  end;

  //form theme colors
  {$IFDEF FMX}Fill.Color{$ELSE}Color{$ENDIF} := DamMsg.Dam.MessageColor;
  {$IFDEF FMX}

  {$ELSE}
  BoxButtons.Color := DamMsg.Dam.ButtonsColor;
  {$ENDIF}
end;

procedure TFrmDamDialogDyn.SetTitleAndIcon;

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

  {case DamMsg.Icon of
    diApp   : Icon.Picture.Icon := Application.Icon;
    diInfo  : Icon.Picture.Icon.Handle := LoadIcon(0, IDI_INFORMATION);
    diQuest : Icon.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
    diWarn  : Icon.Picture.Icon.Handle := LoadIcon(0, IDI_WARNING);
    diError : Icon.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
    diCustom: Icon.Picture.Icon := DamMsg.CustomIcon;
    else raise Exception.Create('Unknown icon kind property');
  end;}
end;

procedure TFrmDamDialogDyn.LoadHelp;
begin
//  BtnHelp.Visible := (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr);
end;

procedure TFrmDamDialogDyn.AlignButtonsPanel;
var
  X: TPixels;
begin

  if DamMsg.Dam.CenterButtons then
    X := GetDiv2(BoxButtons.Width-BoxFloatBtns.Width) //center
  else
    X := BoxButtons.Width-BoxFloatBtns.Width-8; //right

  SetLeft(BoxFloatBtns, X);
end;

procedure TFrmDamDialogDyn.DoSound;

  procedure Play(const aSound: string);
  begin
    //PlaySound(PChar(aSound), 0, SND_ASYNC);
  end;

begin
  case DamMsg.Icon of
    diQuest: Play('SYSTEMQUESTION');
    diWarn: Play('SYSTEMEXCLAMATION');
    diError: Play('SYSTEMHAND');
  end;
end;

procedure TFrmDamDialogDyn.FormShow(Sender: TObject);
begin
  AlignButtonsPanel;

  if DamMsg.Dam.PlaySounds then
    DoSound;
end;

procedure TFrmDamDialogDyn.Action_CopyExecute(Sender: TObject);
begin
  //Clipboard.AsText := TDzHTMLText.HTMLToPlainText(LbMsg.Text);
end;

procedure TFrmDamDialogDyn.Action_HelpExecute(Sender: TObject);
begin
  {if BtnHelp.Visible then
    BtnHelp.Click; }
end;

procedure TFrmDamDialogDyn.BtnHelpClick(Sender: TObject);
begin
 { if DamMsg.HelpContext<>0 then
    Application.HelpContext(DamMsg.HelpContext)
  else
  if DamMsg.HelpKeyword<>EmptyStr then
    Application.HelpKeyword(DamMsg.HelpKeyword)
  else
    raise Exception.Create('Unknown help property');}
end;

procedure TFrmDamDialogDyn.LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink;
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

procedure TFrmDamDialogDyn.OnBtnClick(Sender: TObject);
begin
  DamResult := TButton(Sender).Tag;
  Close;
end;

end.
