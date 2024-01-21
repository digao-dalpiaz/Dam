{$IFNDEF FMX}unit Vcl.DamDialog;{$ENDIF}

{$INCLUDE 'Defines.inc'}

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
{$IFDEF FPC}
  Vcl.DzHTMLText,
  Forms, Classes, FGL, ActnList, Buttons, Controls, StdCtrls, ExtCtrls, Clipbrd,
  SysUtils, Math, Graphics,
  {$IFDEF MSWINDOWS}
  Windows, MMSystem,
  {$ENDIF}
{$ELSE}
  System.Math, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.MMSystem,
  {$ENDIF}
  {$IFDEF FMX}
  FMX.DzHTMLText,
  FMX.Forms, FMX.Objects, FMX.ActnList, FMX.Types, FMX.Controls, FMX.Platform,
    {$IFDEF USE_NEW_UNITS}
    FMX.StdCtrls, FMX.Graphics,
    {$ENDIF}
  {$ELSE}
  Vcl.DzHTMLText,
  Vcl.Forms, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Clipbrd,
  Vcl.Imaging.pngimage,
  {$ENDIF}
{$ENDIF}
  //
  DamLanguage, DamInternalExcept;

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
  TBoxComps = {$IFDEF FMX}TRectangle{$ELSE}TPanel{$ENDIF};

type
  TFrmDamDialogDyn = class(TForm)
  private
    Icon: TImage;
    LbMsg: TDzHTMLText;
    BoxMsg, BoxButtons, BoxFloatBtns: TBoxComps;

    BtnHelp: TSpeedButton;
    ActionList: TActionList;

    DamMsg: TDamMsg;
    DamResult: TDamMsgRes;
    LangStrs: TDamLanguageDefinition;

    procedure BuildControls;

    function GetCurrentMonitorWidth: Integer;

    procedure SetFormCustomization;
    procedure SetFormTitle;
    procedure SetIcon;
    procedure SetHelpButton;
    procedure BuildButtons;
    procedure LoadTextProps(const MsgText: string);

    procedure CalcFormBounds;

    procedure DoSound;

    procedure FormShow(Sender: TObject);
    procedure LbMsgLinkClick(Sender: TObject; Link: TDHBaseLink; var Handled: Boolean);
    procedure BtnHelpClick(Sender: TObject);
    procedure Action_CopyExecute(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);

    {$IFDEF USE_DPICHANGE}
    procedure OnAfterDpiChanged(Sender: TObject; Old, New: Integer);
    {$ENDIF}

    procedure OnBtnClick(Sender: TObject);
  public
    constructor CreateNew; reintroduce;
  end;

const
  BRD_SPACE = 8;
  BTN_HEIGHT = 25;
  {$IFDEF FMX}
  BRUSH_KIND_NONE = TBrushKind.{$IFDEF USE_NEW_ENUMS}None{$ELSE}bkNone{$ENDIF};
  {$ENDIF}

function GetDiv2(Value: TPixels): TPixels;
begin
  Result := Value {$IFDEF FMX}/{$ELSE}div{$ENDIF} 2;
end;

function GetControlLeft(C: TControl): TPixels;
begin
  Result := C.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF};
end;

function GetControlRight(C: TControl): TPixels;
begin
  Result := GetControlLeft(C) + C.Width;
end;

function CalcButtonWidth(Btn: TButton): TPixels;
type TBmp =
  {$IFDEF FPC}
    Graphics
  {$ELSE}
    {$IFDEF FMX}
    FMX.{$IFDEF USE_NEW_UNITS}Graphics{$ELSE}Types{$ENDIF}
    {$ELSE}
    Vcl.Graphics
    {$ENDIF}
  {$ENDIF}.TBitmap;
var
  B: TBmp;
begin
  B := TBmp.Create{$IFDEF USE_FMX_OLD_ENV}(1, 1){$ENDIF};
  try
    B.Canvas.Font.Assign(Btn.Font);

    Result := Max(B.Canvas.TextWidth(Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF})+20, 75);
  finally
    B.Free;
  end;
end;

//

constructor TFrmDamDialogDyn.CreateNew;
begin
  //using CreateNew for resources to work correctly
  inherited CreateNew(Screen.ActiveForm); //active form is used when centering by active form

  OnShow := FormShow;

  {$IFDEF USE_DPICHANGE}
  OnAfterMonitorDpiChanged := OnAfterDpiChanged;
  {$ENDIF}
end;

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;
var
  F: TFrmDamDialogDyn;
begin
  F := TFrmDamDialogDyn.CreateNew;
  try
    F.DamMsg := DamMsg;
    F.LangStrs := LoadLanguage(DamMsg.Dam.Language);

    F.BuildControls;
    F.LoadTextProps(aText);
    F.SetFormCustomization;
    F.SetFormTitle;
    F.SetHelpButton;
    F.BuildButtons;

    F.SetIcon;
    F.CalcFormBounds;

    F.ShowModal;
    Result := F.DamResult;
  finally
    F.Free;
  end;
end;

//

procedure TFrmDamDialogDyn.BuildControls;
var
  Action: TAction;
begin
  ActionList := TActionList.Create(Self);

  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := 16451; //CTRL+C
  Action.OnExecute := Action_CopyExecute;

  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := 112; //F1
  Action.OnExecute := Action_HelpExecute;

  BoxMsg := TBoxComps.Create(Self);
  BoxMsg.Parent := Self;
  {$IFDEF FMX}
  BoxMsg.Align := TAlignLayout.{$IFDEF USE_NEW_ENUMS}Client{$ELSE}alClient{$ENDIF};
  BoxMsg.Stroke.Kind := BRUSH_KIND_NONE; //remove border
  {$ELSE}
  BoxMsg.Align := alClient;
  BoxMsg.BevelOuter := bvNone;
  BoxMsg.ParentBackground := False;
  {$ENDIF}

  Icon := TImage.Create(Self);
  Icon.SetBounds(BRD_SPACE, BRD_SPACE, 32, 32);
  Icon.Parent := BoxMsg;
  {$IFDEF VCL}
  Icon.Proportional := True;
  {$ENDIF}
  Icon.Visible := not DamMsg.Dam.HideIcon;

  LbMsg := TDzHTMLText.Create(Self);
  LbMsg.SetBounds(IfThen(Icon.Visible, GetControlRight(Icon))+BRD_SPACE, BRD_SPACE, 0, 0);
  LbMsg.Parent := BoxMsg;
  LbMsg.OnLinkClick := LbMsgLinkClick;
  {$IFDEF VCL}
  LbMsg.ParentColor := True;
  LbMsg.ParentFont := False;
  LbMsg.Transparent := True; //while we can't get real background color of themes
  {$ENDIF}
  LbMsg.GeneratePlainText := True;

  BoxButtons := TBoxComps.Create(Self);
  BoxButtons.Height := BRD_SPACE+BTN_HEIGHT+BRD_SPACE;
  BoxButtons.Parent := Self;
  {$IFDEF FMX}
  BoxButtons.Align := TAlignLayout.{$IFDEF USE_NEW_ENUMS}Bottom{$ELSE}alBottom{$ENDIF};
  BoxButtons.Stroke.Kind := BRUSH_KIND_NONE; //remove border
  {$ELSE}
  BoxButtons.Align := alBottom;
  BoxButtons.BevelOuter := bvNone;
  BoxButtons.ParentBackground := False;
  {$ENDIF}

  BoxFloatBtns := TBoxComps.Create(Self);
  BoxFloatBtns.SetBounds(0, BRD_SPACE, 0, BTN_HEIGHT);
  BoxFloatBtns.Parent := BoxButtons;
  {$IFDEF FMX}
  BoxFloatBtns.Stroke.Kind := BRUSH_KIND_NONE; //remove border
  BoxFloatBtns.Fill.Kind := BRUSH_KIND_NONE; //transparent background
  {$ELSE}
  BoxFloatBtns.BevelOuter := bvNone;
  {$ENDIF}

  BtnHelp := TSpeedButton.Create(Self);
  BtnHelp.SetBounds(BRD_SPACE, BRD_SPACE, BTN_HEIGHT{width same as height}, BTN_HEIGHT);
  BtnHelp.Parent := BoxButtons;
  BtnHelp.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := '?';
  BtnHelp.OnClick := BtnHelpClick;
end;

procedure TFrmDamDialogDyn.LoadTextProps(const MsgText: string);
begin
  {$IFDEF USE_IMGLST}
  LbMsg.Images := DamMsg.Dam.Images;
  {$ENDIF}

  LbMsg.Font.Assign(DamMsg.Dam.MessageFont); //keep this always before automatic scaling
  {$IFDEF FMX}
  LbMsg.FontColor := DamMsg.Dam.MessageFontColor;
  {$ENDIF}

  LbMsg.Text := MsgText; //set TEXT
  {$IFDEF VCL}
  if LbMsg.LinkRefs.Count>0 then BoxMsg.DoubleBuffered := True; //** while using Transparent property
  {$ENDIF}
end;

procedure TFrmDamDialogDyn.SetFormCustomization;
begin
  case DamMsg.Dam.DialogPosition of
    dpScreenCenter: Position := {$IFDEF FMX}TFormPosition.{$IFDEF USE_NEW_ENUMS}ScreenCenter{$ELSE}poScreenCenter{$ENDIF}{$ELSE}poScreenCenter{$ENDIF};
    dpActiveFormCenter: Position := {$IFDEF FMX}TFormPosition.{$IFDEF USE_NEW_ENUMS}OwnerFormCenter{$ELSE}poOwnerFormCenter{$ENDIF}{$ELSE}poOwnerFormCenter{$ENDIF};
    dpMainFormCenter: Position := {$IFDEF FMX}TFormPosition.{$IFDEF USE_NEW_ENUMS}MainFormCenter{$ELSE}poMainFormCenter{$ENDIF}{$ELSE}poMainFormCenter{$ENDIF};
    else raise EDamInternalExcept.Create('Invalid dialog position property');
  end;

  //form border
  {$IFDEF FMX}
  if DamMsg.Dam.DialogBorder then
  begin
    BorderStyle := TFmxFormBorderStyle.{$IFDEF USE_NEW_ENUMS}Single{$ELSE}bsSingle{$ENDIF};
    BorderIcons := []; //How to remove only icon?
  end
  else
    BorderStyle := TFmxFormBorderStyle.{$IFDEF USE_NEW_ENUMS}None{$ELSE}bsNone{$ENDIF};
  {$ELSE}
  if DamMsg.Dam.DialogBorder then
    BorderStyle := bsDialog
  else
    BorderStyle := bsNone;
  {$ENDIF}

  //form theme colors
  {$IFDEF FMX}
  if DamMsg.Dam.MessageColor <> TAlphaColors.Null then
    BoxMsg.Fill.Color := DamMsg.Dam.MessageColor
  else
    BoxMsg.Fill.Kind := BRUSH_KIND_NONE;

  if DamMsg.Dam.ButtonsColor <> TAlphaColors.Null then
    BoxButtons.Fill.Color := DamMsg.Dam.ButtonsColor
  else
    BoxButtons.Fill.Kind := BRUSH_KIND_NONE;
  {$ELSE}
  BoxMsg.Color := DamMsg.Dam.MessageColor;
  BoxButtons.Color := DamMsg.Dam.ButtonsColor;
  {$ENDIF}
end;

procedure TFrmDamDialogDyn.SetFormTitle;

  function GetIconTitle: string;
  begin
    case DamMsg.Icon of
      diApp   : Result := Application.Title;
      diInfo  : Result := LangStrs.Info;
      diQuest : Result := LangStrs.Quest;
      diWarn  : Result := LangStrs.Warn;
      diError : Result := LangStrs.Error;
      diCustom: Result := LangStrs.Msg;
      else raise EDamInternalExcept.Create('Unknown icon kind property');
    end;
  end;

begin
  case DamMsg.Title of
    dtApp       : Caption := Application.Title;
    dtParentForm: Caption := TForm(DamMsg.Dam.Owner).Caption;
    dtMainForm  : Caption := Application.MainForm.Caption;
    dtByIcon    : Caption := GetIconTitle;
    dtCustom    : Caption := DamMsg.CustomTitle;
    else raise EDamInternalExcept.Create('Unknown title kind property');
  end;
end;

procedure TFrmDamDialogDyn.SetHelpButton;
begin
  BtnHelp.Visible :=
    {$IFDEF VCL}
    (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr)
    {$ELSE}
    False //** here we can implement help event for FMX ???
    {$ENDIF};
end;

procedure TFrmDamDialogDyn.BuildButtons;
var
  ButtonsList: TList<TButton>;
  NumButtons: Byte;
  I: Integer;
  X: TPixels;
  Btn: TButton;
  Names: array[1..3] of string;
begin
  case DamMsg.Buttons of
    dbOne, dbOK: NumButtons := 1;
    dbTwo, dbYesNo: NumButtons := 2;
    dbThree: NumButtons := 3;
    else raise EDamInternalExcept.Create('Unknown buttons kind property');
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

 ButtonsList := TList<TButton>.Create;
 try
    X := 0;
    for I := 1 to NumButtons do
    begin
      Btn := TButton.Create(Self);
      Btn.Parent := BoxFloatBtns;
      Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Names[I];
      Btn.OnClick := OnBtnClick;
      Btn.Tag := I;

      Btn.SetBounds(X, 0, CalcButtonWidth(Btn), BoxFloatBtns.Height);
      X := X + Btn.Width + BRD_SPACE;

      ButtonsList.Add(Btn);
    end;

    ButtonsList.Last.Cancel := True;
    if DamMsg.SwapFocus then
      ActiveControl := ButtonsList.Last
    else
      ActiveControl := ButtonsList.First; //In FMX, first control is not auto focused

    BoxFloatBtns.Width := GetControlRight(ButtonsList.Last);
 finally
   ButtonsList.Free;
 end;
end;

{$IFDEF VCL}
type TPictureAccess = class(TPicture); //needed in Delphi 10.1 Berlin and previous versions (LoadFromStream)
{$ENDIF}
procedure TFrmDamDialogDyn.SetIcon;

  {$IFDEF VCL_WIN}
  procedure LoadWindowsIcon(Code: Integer);
  begin
    Icon.Picture.Icon.Handle := LoadImage(GetModuleHandle('user32'),
      MAKEINTRESOURCE(Code), IMAGE_ICON, Icon.Width, Icon.Height, LR_DEFAULTCOLOR); //scaled
  end;
  {$ENDIF}

  procedure GetIconFromResource;
  var
    R: TResourceStream;
    ResName: string;
  begin
    ResName := string.Empty;
    case DamMsg.Icon of
      diApp   :
        {$IFDEF FMX}
        raise EDamInternalExcept.Create('Unsupported app icon in FMX environment');
        {$ELSE}
        Icon.Picture.Icon.Assign(Application.Icon);
        {$ENDIF}
      diCustom :
        {$IFDEF FMX}
        Icon.Bitmap.Assign(DamMsg.CustomIcon);
        {$ELSE}
        Icon.Picture.Icon.Assign(DamMsg.CustomIcon);
        {$ENDIF}
      diInfo  : ResName := 'IC_INFO';
      diQuest : ResName := 'IC_QUESTION';
      diWarn  : ResName := 'IC_WARNING';
      diError : ResName := 'IC_ERROR';
      else raise EDamInternalExcept.Create('Unknown icon kind property');
    end;

    if not ResName.IsEmpty then
    begin
      R := GetResource(ResName);
      try
        {$IFDEF FMX}
        Icon.Bitmap.LoadFromStream(R);
        {$ELSE}
        TPictureAccess(Icon.Picture).LoadFromStream(R);
        {$ENDIF}
      finally
        R.Free;
      end;
    end;
  end;

begin
  {$IFDEF VCL_WIN}
  case DamMsg.Icon of
    diApp   : Icon.Picture.Icon.Assign(Application.Icon);
    diCustom: Icon.Picture.Icon.Assign(DamMsg.CustomIcon);
    diInfo  : LoadWindowsIcon(104);
    diQuest : LoadWindowsIcon(102);
    diWarn  : LoadWindowsIcon(101);
    diError : LoadWindowsIcon(103);
    else raise EDamInternalExcept.Create('Unknown icon kind property');
  end;
  {$ELSE}
  GetIconFromResource;
  {$ENDIF}
end;

function TFrmDamDialogDyn.GetCurrentMonitorWidth: Integer;
var
  R: TRect;

{$IFDEF FMX}
  {$IFDEF USE_FMX_OLD_ENV}
  ScreenService: IFMXScreenService;
  {$ELSE}
  D: TDisplay;

  function GetPrimaryDisplay: TDisplay;
  var
    I: Integer;
  begin
    for I := 0 to Screen.DisplayCount-1 do
      if Screen.Displays[I].Primary then
        Exit(Screen.Displays[I]);

    raise EDamInternalExcept.Create('Primary display not found');
  end;
  {$ENDIF}
{$ELSE}
  M: TMonitor;
{$ENDIF}

begin
  {$IFDEF FMX}
    {$IFDEF USE_FMX_OLD_ENV}
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
      R := TRect.Create(0, 0, Round(ScreenService.GetScreenSize.X), Round(ScreenService.GetScreenSize.Y))
    else
      raise EDamInternalExcept.Create('Could not get Monitor Rect');
    {$ELSE}
    if Screen.ActiveForm <> nil then
      D := Screen.DisplayFromForm(Screen.ActiveForm)
    else
      D := GetPrimaryDisplay;

    R := D.BoundsRect{$IF CompilerVersion >= 35}.Round{$ENDIF}; //Round - Delphi 11
    {$ENDIF}
  {$ELSE}
    if Screen.ActiveForm <> nil then
      M := Screen.ActiveForm.Monitor
    else
      M := Screen.PrimaryMonitor;

    R := M.BoundsRect;
  {$ENDIF}

  Result := R.Width;
end;

procedure TFrmDamDialogDyn.CalcFormBounds;
var
  Brd, X, IconHeight, Y: TPixels;
  Delta: Extended;
begin
  //!!! All sizes must be scaled in this method
  Brd := GetControlLeft(Icon); //default border scaled

  //--WIDTH--

  if DamMsg.FixedWidth=0 then
  begin
    Delta := 0.75; //75% of screen width
    {$IF Defined(DCC) and (CompilerVersion >= 26)} //XE5
    if TOSVersion.Platform in [pfiOS, pfAndroid] then Delta := 0.95;
    {$ENDIF}
    X := Round(GetCurrentMonitorWidth * Delta)
  end
  else
    X := LbMsg.CalcScale(DamMsg.FixedWidth);

  //previous size is based on form, so here we convert to message size
  LbMsg.Width := X - GetControlLeft(LbMsg) - Brd;

  if (DamMsg.FixedWidth=0) and (LbMsg.TextWidth < LbMsg.Width) then //smaller than max screen size
  begin
    LbMsg.Width := Max(LbMsg.TextWidth, Max(
      LbMsg.CalcScale(300),
      BoxFloatBtns.Width+((GetControlRight(BtnHelp)+Brd)*2){Help button safe space} - GetControlLeft(LbMsg)
    ));
  end;

  ClientWidth := Round(GetControlRight(LbMsg) + Brd);

  //align FloatBtns
  X := ClientWidth{BoxButtons.Width does not work with Lazarus} - BoxFloatBtns.Width;
  if DamMsg.Dam.CenterButtons then
    X := GetDiv2(X) //center
  else
    X := X - Brd; //right

  BoxFloatBtns.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF} := X;

  //--HEIGHT--

  IconHeight := IfThen(Icon.Visible, Icon.Height);

  LbMsg.Height := LbMsg.TextHeight;
  ClientHeight := Round(
    Max(LbMsg.Height, IconHeight)+
    (Brd*2)+
    BoxButtons.Height);

  Y := Brd;
  if LbMsg.Height<IconHeight then //text smaller than icon
    Y := Y + GetDiv2(IconHeight-LbMsg.Height);

  LbMsg.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF} := Y;

  //

  {$IFDEF FMX}
  //workaround for: https://stackoverflow.com/questions/76164235/creating-forms-dynamically-border-behavior
  if not DamMsg.Dam.DialogBorder then
  begin
    Width := ClientWidth;
    Height := ClientHeight;
  end;
  {$ENDIF}
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
  aMsg := LbMsg.PlainText.ToString;

  {$IFDEF FMX}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    {$IFDEF USE_FMX_OLD_ENV}IInterface(uClipBoard){$ELSE}uClipBoard{$ENDIF}) then
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
    raise EDamInternalExcept.Create('Unknown help property');
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

{$IFDEF USE_DPICHANGE}
procedure TFrmDamDialogDyn.OnAfterDpiChanged(Sender: TObject; Old, New: Integer);
begin
  //only in VCL
  SetIcon;
  CalcFormBounds;
end;
{$ENDIF}

procedure TFrmDamDialogDyn.OnBtnClick(Sender: TObject);
begin
  DamResult := TButton(Sender).Tag;
  Close;
end;

end.
