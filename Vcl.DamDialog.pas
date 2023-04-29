{$IFNDEF FMX}unit Vcl.DamDialog;{$ENDIF}

{$IFDEF FMX}
  //
{$ELSE}
  {$DEFINE VCL}
{$ENDIF}

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

uses
{$IFDEF VCL}ScalingUtils, {$ENDIF}
{$IFDEF FPC}
  Forms, Classes, FGL, ActnList, Buttons, Controls, ExtCtrls, Clipbrd,
  SysUtils, Graphics,
  {$IFDEF MSWINDOWS}
  Windows, MMSystem,
  {$ENDIF}
{$ELSE}
  System.Math, System.SysUtils, System.Types, System.UITypes,
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.MMSystem,
  {$ENDIF}
  {$IFDEF FMX}
  FMX.DzHTMLText,
  FMX.Forms, FMX.Objects, FMX.StdCtrls, FMX.ActnList, FMX.Types,
  FMX.Graphics, FMX.Controls, FMX.Platform,
  {$ELSE}
  Vcl.DzHTMLText,
  Vcl.Forms, System.Classes, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Clipbrd,
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
  TFrmDamDialogDyn = class(TForm)
  private
    Icon: TImage;
    LbMsg: TDzHTMLText;
    BoxButtons, BoxFloatBtns: TPanel;
    ButtonsList: TList<TButton>;
    BtnHelp: TSpeedButton;
    ActionList: TActionList;

    DamMsg: TDamMsg;
    DamResult: TDamMsgRes;
    LangStrs: TDamLanguageDefinition;

    {$IFDEF VCL}
    Scaling: TDzFormScaling;
    {$ENDIF}

    function ToScale(Value: TPixels): TPixels;
    function GetCurrentMonitorRect: TRect;

    procedure SetFormCustomization;
    procedure SetTitleAndIcon;
    procedure BuildButtons;
    procedure LoadHelp;
    procedure LoadTextProps;

    procedure AlignButtons;
    procedure CalcWidth;
    procedure CalcHeight;

    procedure CenterForm;

    procedure DoSound;

    procedure FormShow(Sender: TObject);
    procedure LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink; var Handled: Boolean);
    procedure BtnHelpClick(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);

    procedure OnBtnClick(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

constructor TFrmDamDialogDyn.Create;
var
  Action: TAction;
begin
  inherited CreateNew(Application);

  ButtonsList := TList<TButton>.Create;

  OnShow := FormShow;
  {$IFDEF FMX}
  BorderIcons := [];
  {$ELSE}
  Position := poDesigned;
  PixelsPerInch := 96;
  {$ENDIF}

  ActionList := TActionList.Create(Self);

  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := 16451; //CTRL+C
  Action.OnExecute := Action_CopyExecute;

  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := 112; //F1
  Action.OnExecute := Action_HelpExecute;

  Icon := TImage.Create(Self);
  Icon.Parent := Self;
  Icon.SetBounds(8, 8, 32, 32);

  LbMsg := TDzHTMLText.Create(Self);
  LbMsg.Parent := Self;
  LbMsg.SetBounds(48, 8, 0, 0);
  LbMsg.OnLinkClick := LbMsgLinkClick;
  {$IFDEF VCL}
  LbMsg.ParentColor := True;
  {$ENDIF}

  BoxButtons := TPanel.Create(Self);
  BoxButtons.Parent := Self;
  BoxButtons.Height := 39;
  {$IFDEF FMX}
  BoxButtons.Align := TAlignLayout.Bottom;
  BoxButtons.StyleLookup := 'pushpanel';
  {$ELSE}
  BoxButtons.Align := alBottom;
  BoxButtons.BevelOuter := bvNone;
  BoxButtons.ParentBackground := False;
  {$ENDIF}

  BoxFloatBtns := TPanel.Create(Self);
  BoxFloatBtns.Parent := BoxButtons;
  BoxFloatBtns.SetBounds(0, 8, 0, 25);
  {$IFDEF FMX}
  BoxFloatBtns.StyleLookup := 'pushpanel';
  {$ELSE}
  BoxFloatBtns.BevelOuter := bvNone;
  {$ENDIF}

  BtnHelp := TSpeedButton.Create(Self);
  BtnHelp.Parent := BoxButtons;
  BtnHelp.SetBounds(8, 8, 25, 25);
  BtnHelp.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := '?';
  BtnHelp.OnClick := BtnHelpClick;
end;

destructor TFrmDamDialogDyn.Destroy;
begin
  ButtonsList.Free;
  inherited;
end;

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;
var
  F: TFrmDamDialogDyn;
begin
  F := TFrmDamDialogDyn.Create;
  try
    {$IFDEF VCL}
    if (csDesigning in DamMsg.ComponentState) then F.LbMsg.StyleElements := []; //do not use themes in Delphi IDE
    {$ENDIF}

    F.DamMsg := DamMsg;

    F.LangStrs := LoadLanguage(DamMsg.Dam.Language);

    F.SetFormCustomization;
    F.SetTitleAndIcon;
    F.BuildButtons;
    F.LoadHelp;
    F.LoadTextProps; //required before auto form scaling

    {$IFDEF VCL}
    F.ScaleForCurrentDpi; //auto form scaling
    F.Scaling := TDzFormScaling.Create;
    try
      F.Scaling.Update(F);
    {$ENDIF}
      F.AlignButtons;
      F.CalcWidth;
      F.CalcHeight;
    {$IFDEF VCL}
    finally
      F.Scaling.Free;
    end;
    {$ENDIF}

    F.CenterForm;

    F.ShowModal;
    Result := F.DamResult;
  finally
    F.Free;
  end;
end;

//

procedure TFrmDamDialogDyn.SetFormCustomization;
begin
  //form border
  //if DamMsg.Dam.DialogBorder then
  //  BorderStyle := {$IFDEF FMX}TFmxFormBorderStyle.Single{$ELSE}bsDialog{$ENDIF}
  //else
  //  BorderStyle := {$IFDEF FMX}TFmxFormBorderStyle.None{$ELSE}bsNone{$ENDIF};

  //form theme colors
  {$IFDEF FMX}
  Fill.Color := DamMsg.Dam.MessageColor;
  if Fill.Color<>TAlphaColors.Null then Fill.Kind := TBrushKind.Solid;

  if DamMsg.Dam.ButtonsColor<>TAlphaColors.Null then
    TShape(BoxButtons.Controls[0]).Fill.Color := DamMsg.Dam.ButtonsColor;
  {$ELSE}
  Color := DamMsg.Dam.MessageColor;
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

procedure TFrmDamDialogDyn.BuildButtons;
var
  NumButtons: Byte;
  I: Integer;
  Btn: TButton;
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

  for I := 1 to NumButtons do
  begin
    Btn := TButton.Create(Self);
    Btn.Parent := BoxFloatBtns;
    Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Names[I];
    Btn.OnClick := OnBtnClick;
    Btn.Tag := I;

    ButtonsList.Add(Btn);
  end;

  //here btn contains last button reference

  Btn.Cancel := True; //set last button as cancel (esc) button
  if DamMsg.SwapFocus then ActiveControl := Btn; //set last button as start focus button
end;

procedure TFrmDamDialogDyn.LoadHelp;
begin
  BtnHelp.Visible :=
    {$IFDEF VCL}
    (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr)
    {$ELSE}
    False
    {$ENDIF};
end;

procedure TFrmDamDialogDyn.LoadTextProps;
begin
  LbMsg.Images := DamMsg.Dam.Images;
  LbMsg.Font.Assign(DamMsg.Dam.MessageFont);
  {$IFDEF FMX}
  LbMsg.FontColor := DamMsg.Dam.MessageFontColor;
  {$ENDIF}
end;

function TFrmDamDialogDyn.ToScale(Value: TPixels): TPixels;
begin
  {$IFDEF VCL}
  Result := Scaling.Calc(Value);
  {$ELSE}
  Result := Value;
  {$ENDIF}
end;

function TFrmDamDialogDyn.GetCurrentMonitorRect: TRect;
begin
  Result := {$IFDEF FMX}Screen.DisplayFromForm(Self){$ELSE}Monitor{$ENDIF}.BoundsRect;
end;

function GetDiv2(Value: TPixels): TPixels;
begin
  Result := Value {$IFDEF FMX}/{$ELSE}div{$ENDIF} 2;
end;

procedure TFrmDamDialogDyn.AlignButtons;
type TBmp = {$IFDEF DCC}
    {$IFDEF FMX}FMX{$ELSE}Vcl{$ENDIF}.
  {$ENDIF}Graphics.TBitmap;
var
  B: TBmp;
  Btn: TButton;
  X, W: TPixels;
begin
  B := TBmp.Create;
  try
    B.Canvas.Font.Assign(ButtonsList.First.Font);

    X := 0;
    for Btn in ButtonsList do
    begin
      W := Max(B.Canvas.TextWidth(Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF})+ToScale(20), ToScale(75));
      Btn.SetBounds(X, 0, W, BoxFloatBtns.Height);
      X := X + Btn.Width + ToScale(8);
    end;
  finally
    B.Free;
  end;

  BoxFloatBtns.Width := ButtonsList.Last.BoundsRect.Right;
end;

procedure TFrmDamDialogDyn.CalcWidth;
var
  MinSize, X: TPixels;
begin
  if DamMsg.FixedWidth=0 then
    LbMsg.Width := Trunc(GetCurrentMonitorRect.Width * 0.75) //max width
  else
    LbMsg.Width := ToScale(DamMsg.FixedWidth);

  LbMsg.Text := DamMsg.Message; //set TEXT

  if (DamMsg.FixedWidth=0) and (LbMsg.TextWidth < LbMsg.Width) then
  begin
    MinSize := Max(ToScale(300), BoxFloatBtns.Width);
    LbMsg.Width := Max(LbMsg.TextWidth, MinSize);
  end;

  ClientWidth := Trunc(LbMsg.BoundsRect.Right+ToScale(8));

  //align FloatBtns
  if DamMsg.Dam.CenterButtons then
    X := GetDiv2(ClientWidth-BoxFloatBtns.Width) //center
  else
    X := ClientWidth-BoxFloatBtns.Width-ToScale(8); //right

  BoxFloatBtns.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF} := X;
end;

procedure TFrmDamDialogDyn.CalcHeight;
begin
  LbMsg.Height := LbMsg.TextHeight;
  ClientHeight := Trunc(
    Max(LbMsg.Height, Icon.Height)+
    (LbMsg.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF}*2)+
    BoxButtons.Height);

  if LbMsg.Height<Icon.Height then //text smaller than icon
  begin
    LbMsg.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF} :=
      LbMsg.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF} + GetDiv2(Icon.Height-LbMsg.Height);
  end;
end;

procedure TFrmDamDialogDyn.CenterForm;
var
  R: TRect;
  F: {$IFDEF FMX}TCommonCustomForm{$ELSE}TForm{$ENDIF};
begin
  //form screen position
  R := GetCurrentMonitorRect;
  F := nil;
  case DamMsg.Dam.DialogPosition of
    dpScreenCenter: {};
    dpMainFormCenter: F := Application.MainForm;
    dpActiveFormCenter: F := Screen.ActiveForm;
    else raise Exception.Create('Invalid dialog position property');
  end;
  if F<>nil then
    R := F.{$IFDEF FMX}Bounds{$ELSE}BoundsRect{$ENDIF};

  Left := Round(R.Left + GetDiv2(R.Width - Width));
  Top := Round(R.Top + GetDiv2(R.Height - Height));
end;

procedure TFrmDamDialogDyn.FormShow(Sender: TObject);
begin
  if DamMsg.Dam.PlaySounds then
    DoSound;
end;

procedure TFrmDamDialogDyn.DoSound;

  procedure Play(const aSound: string);
  begin
    {$IFDEF MSWINDOWS}
    PlaySound(PChar(aSound), 0, SND_ASYNC);
    {$ENDIF}
  end;

begin
  case DamMsg.Icon of
    diQuest: Play('SYSTEMQUESTION');
    diWarn: Play('SYSTEMEXCLAMATION');
    diError: Play('SYSTEMHAND');
  end;
end;

procedure TFrmDamDialogDyn.Action_CopyExecute(Sender: TObject);
var
  aMsg: string;
  {$IFDEF FMX}
  uClipBoard: IFMXClipboardService;
  {$ENDIF}
begin
  aMsg := TDzHTMLText.HTMLToPlainText(LbMsg.Text);

  {$IFDEF FMX}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, uClipBoard) then
    uClipBoard.SetClipboard(aMsg);
  {$ELSE}
  Clipboard.AsText := aMsg;
  {$ENDIF}
end;

procedure TFrmDamDialogDyn.Action_HelpExecute(Sender: TObject);
begin
  if BtnHelp.Visible then
    BtnHelpClick(nil);
end;

procedure TFrmDamDialogDyn.BtnHelpClick(Sender: TObject);
begin
  {$IFDEF VCL}
  if DamMsg.HelpContext<>0 then
    Application.HelpContext(DamMsg.HelpContext)
  else
  if DamMsg.HelpKeyword<>EmptyStr then
    Application.HelpKeyword(DamMsg.HelpKeyword)
  else
    raise Exception.Create('Unknown help property');
  {$ENDIF}
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
