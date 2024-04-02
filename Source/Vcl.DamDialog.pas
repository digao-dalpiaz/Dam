{$IFNDEF FMX}unit Vcl.DamDialog;{$ENDIF}

{$INCLUDE 'Defines.inc'}

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
  FMX.StdCtrls, FMX.Graphics,
    {$IFDEF MSWINDOWS}FMX.Platform.Win, {$ENDIF}
  {$ELSE}
  Vcl.DzHTMLText,
  Vcl.Forms, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Clipbrd,
  Vcl.Imaging.pngimage,
  {$ENDIF}
{$ENDIF}
  //
  DamLanguage, DamInternalExcept;

type
  TBoxComps = {$IFDEF FMX}TRectangle{$ELSE}TPanel{$ENDIF};

  TBmp =
  {$IFDEF FPC}
    Graphics
  {$ELSE}
    {$IFDEF FMX}
    FMX.Graphics
    {$ELSE}
    Vcl.Graphics
    {$ENDIF}
  {$ENDIF}.TBitmap;

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

    ButtonsList: TList<TButton>;

    VirtualBmp: TBmp;
    DisplayRect, OutAreaRect: TRect; //absolute rect - always integer even in FMX
    DisplayWidth: TPixels;
    {$IFDEF FMX_WIN}
    DisplayScale: Single;
    {$ENDIF}

    procedure BuildControls;
    procedure LoadTextProps(const MsgText: string);
    procedure SetFormCustomization;
    procedure SetFormTitle;
    procedure SetHelpButton;
    procedure BuildButtons;

    procedure GetDisplayRect;
    procedure SetIcon;
    procedure CalcFormBounds;
    procedure FormPositioning;

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
    destructor Destroy; override;
  end;

const
  BRD_SPACE = 8;
  DEFAULT_PPI = 96;

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

//

constructor TFrmDamDialogDyn.CreateNew;
begin
  //using CreateNew for resources to work correctly
  inherited CreateNew(Application);

  OnShow := FormShow;

  {$IFDEF USE_DPICHANGE}
  OnAfterMonitorDpiChanged := OnAfterDpiChanged;
  {$ENDIF}

  ButtonsList := TList<TButton>.Create;

  VirtualBmp := TBmp.Create;
end;

destructor TFrmDamDialogDyn.Destroy;
begin
  ButtonsList.Free;
  VirtualBmp.Free;
  inherited;
end;

function RunDamDialog(DamMsg: TDamMsg; const aText: string): TDamMsgRes;
var
  F: TFrmDamDialogDyn;
begin
  F := TFrmDamDialogDyn.CreateNew;
  try
    {$IFDEF FPC}
    //this does not work if placed in CreateNew
    F.PixelsPerInch := DEFAULT_PPI;
    {$ENDIF}

    F.DamMsg := DamMsg;
    F.LangStrs := LoadLanguage(DamMsg.Dam.Language);

    F.BuildControls;
    F.LoadTextProps(aText);
    F.SetFormCustomization;
    F.SetFormTitle;
    F.SetHelpButton;
    F.BuildButtons;

    F.GetDisplayRect;

    {$IFDEF VCL}
      {$IFDEF DCC}
        {$IF CompilerVersion >= 31} //Delphi 10.1 Berlin
        F.ScaleForCurrentDPI;
        {$ELSE}
        F.ChangeScale(F.Monitor.PixelsPerInch, DEFAULT_PPI);
        //there is an issue here: if form moved to another display with different dpi, scaling will be wrong
        {$ENDIF}
      {$ELSE}
      F.AutoScale;
      {$ENDIF}
    {$ENDIF}
    F.SetIcon;
    F.CalcFormBounds;

    F.FormPositioning;
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
  BtnHeight: TPixels;
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
  BoxMsg.Align := TAlignLayout.Client;
  BoxMsg.Stroke.Kind := TBrushKind.None; //remove border
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

  VirtualBmp.Canvas.Font.Assign(DamMsg.Dam.ButtonsFont);
  BtnHeight := VirtualBmp.Canvas.TextHeight('A')+8;

  BoxButtons := TBoxComps.Create(Self);
  BoxButtons.Height := BRD_SPACE+BtnHeight+BRD_SPACE;
  BoxButtons.Parent := Self;
  {$IFDEF FMX}
  BoxButtons.Align := TAlignLayout.Bottom;
  BoxButtons.Stroke.Kind := TBrushKind.None; //remove border
  {$ELSE}
  BoxButtons.Align := alBottom;
  BoxButtons.BevelOuter := bvNone;
  BoxButtons.ParentBackground := False;
  {$ENDIF}

  BoxFloatBtns := TBoxComps.Create(Self);
  BoxFloatBtns.SetBounds(0, BRD_SPACE, 0, BtnHeight);
  BoxFloatBtns.Parent := BoxButtons;
  {$IFDEF FMX}
  BoxFloatBtns.Stroke.Kind := TBrushKind.None; //remove border
  BoxFloatBtns.Fill.Kind := TBrushKind.None; //transparent background
  {$ELSE}
  BoxFloatBtns.BevelOuter := bvNone;
  {$ENDIF}

  BtnHelp := TSpeedButton.Create(Self);
  BtnHelp.SetBounds(BRD_SPACE, BRD_SPACE, 0, BtnHeight);
  BtnHelp.Parent := BoxButtons;
  BtnHelp.OnClick := BtnHelpClick;
  BtnHelp.Font.Assign(DamMsg.Dam.ButtonsFont);
  BtnHelp.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := '?';
end;

procedure TFrmDamDialogDyn.LoadTextProps(const MsgText: string);
begin
  LbMsg.Images := DamMsg.Dam.Images;
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
  //form border
  {$IFDEF FMX}
  if DamMsg.Dam.DialogBorder then
  begin
    BorderStyle := TFmxFormBorderStyle.Single;
    BorderIcons := []; //How to remove only icon?
  end
  else
    BorderStyle := TFmxFormBorderStyle.None;
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
    BoxMsg.Fill.Kind := TBrushKind.None;

  if DamMsg.Dam.ButtonsColor <> TAlphaColors.Null then
    BoxButtons.Fill.Color := DamMsg.Dam.ButtonsColor
  else
    BoxButtons.Fill.Kind := TBrushKind.None;
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
  BtnHelp.Visible := (DamMsg.HelpContext<>0) or (DamMsg.HelpKeyword<>EmptyStr);
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

  for I := 1 to NumButtons do
  begin
    Btn := TButton.Create(Self);
    Btn.Parent := BoxFloatBtns;
    Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF} := Names[I];
    Btn.OnClick := OnBtnClick;
    Btn.Tag := I;
    {$IFDEF FMX}
    Btn.TextSettings.Font.Assign(DamMsg.Dam.ButtonsFont);
    Btn.TextSettings.FontColor := DamMsg.Dam.ButtonsFontColor;
    Btn.StyledSettings := [];
    {$ELSE}
    Btn.Font.Assign(DamMsg.Dam.ButtonsFont);
    {$ENDIF}

    ButtonsList.Add(Btn);
  end;

  ButtonsList.Last.Cancel := True;
  if DamMsg.SwapFocus then
    ActiveControl := ButtonsList.Last
  else
    ActiveControl := ButtonsList.First; //In FMX, first control is not auto focused
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

procedure TFrmDamDialogDyn.GetDisplayRect;
var
  OutForm, FormToGetMonitor: {$IFDEF FMX}TCommonCustomForm{$ELSE}TForm{$ENDIF};

  {$IFDEF FMX}
  Display: TDisplay;
  {$ENDIF}
begin
  Position := {$IFDEF FMX}TFormPosition.Designed{$ELSE}poDesigned{$ENDIF};
  {$IFDEF VCL}
  DefaultMonitor := dmDesktop; //just to allow form show on manual monitor position
  {$ENDIF}

  case DamMsg.Dam.DialogPosition of
    dpScreenCenter: OutForm := nil;
    dpActiveFormCenter: OutForm := Screen.ActiveForm;
    dpMainFormCenter: OutForm := Application.MainForm;
    else raise EDamInternalExcept.Create('Invalid dialog position property');
  end;

  FormToGetMonitor := OutForm;
  if FormToGetMonitor=nil then FormToGetMonitor := Self;

  {$IFDEF FMX}
    Display := Screen.DisplayFromForm(FormToGetMonitor);

    DisplayRect := Display.PhysicalBounds;
    DisplayWidth := Display.Bounds.Width; //scaled
    {$IFDEF MSWINDOWS}DisplayScale := Display.Scale;{$ENDIF}
  {$ELSE}
  DisplayRect := FormToGetMonitor.Monitor.BoundsRect;
  DisplayWidth := DisplayRect.Width;

  //scaling must be based on correct target monitor, so let's put the form in destination monitor
  //there is no way to set Form.Monitor property, but this will do the job
  SetBounds(DisplayRect.Left, DisplayRect.Top, Width, Height);
  {$ENDIF};

  if DamMsg.Dam.PreferDisplayCenter or (OutForm=nil) then
    OutAreaRect := DisplayRect
  else
  begin
    {$IFDEF FMX_WIN}
    //in FMX, sizes are already scaled, including form bounds, so we need to call Win API to get physical bounds
    if not GetWindowRect(WindowHandleToPlatform(OutForm.Handle).Wnd, OutAreaRect) then
      raise EDamInternalExcept.Create('Error getting window rect');
    {$ELSE}
    OutAreaRect := OutForm.{$IFDEF FMX}Bounds{$ELSE}BoundsRect{$ENDIF};
    {$ENDIF}
  end;
end;

procedure TFrmDamDialogDyn.CalcFormBounds;
var
  Brd, X, IconHeight, Y: TPixels;
  Delta: Extended;
  Btn: TButton;
begin
  //!!! All sizes must be scaled in this method
  Brd := GetControlLeft(Icon); //default border scaled

  //--Buttons

  VirtualBmp.Canvas.Font.Assign(ButtonsList.First.Font); //font scaled

  BtnHelp.Width := VirtualBmp.Canvas.TextWidth(BtnHelp.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF})+(2*Brd);

  X := 0;
  for Btn in ButtonsList do
  begin
    Btn.SetBounds(X, 0, Max(VirtualBmp.Canvas.TextWidth(Btn.{$IFDEF FMX}Text{$ELSE}Caption{$ENDIF})+(2*Brd), LbMsg.CalcScale(75)), BoxFloatBtns.Height);
    X := X + Btn.Width + Brd;
  end;
  BoxFloatBtns.Width := GetControlRight(ButtonsList.Last);

  //--WIDTH--

  if DamMsg.FixedWidth=0 then
  begin
    Delta := 0.75; //75% of screen width
    {$IFDEF DCC}
    if TOSVersion.Platform in [pfiOS, pfAndroid] then Delta := 0.95;
    {$ENDIF}
    X := {$IFDEF VCL}Round{$ENDIF}(DisplayWidth * Delta);
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

procedure TFrmDamDialogDyn.FormPositioning;
var
  X, Y, W, H: TPixels;
begin
  {$IFDEF FMX_WIN}
  W := Width * DisplayScale;
  H := Height * DisplayScale;
  {$ELSE}
  W := Width;
  H := Height;
  {$ENDIF}

  X := OutAreaRect.Left + GetDiv2(OutAreaRect.Width - W);
  Y := OutAreaRect.Top + GetDiv2(OutAreaRect.Height - H);

  //--keep dialog inside display bounds
  if X < DisplayRect.Left then X := DisplayRect.Left;
  if Y < DisplayRect.Top then X := DisplayRect.Top;

  if X + W > DisplayRect.Right then X := DisplayRect.Right - W;
  if Y + H > DisplayRect.Bottom then Y := DisplayRect.Bottom - H;
  //--

  {$IFDEF FMX_WIN}
  SetWindowPos(WindowHandleToPlatform(Handle).Wnd, 0, Round(X), Round(Y), 0, 0, SWP_NOSIZE);
  {$ELSE}
    {$IFDEF FMX}SetBoundsF{$ELSE}SetBounds{$ENDIF}(X, Y, Width, Height);
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
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, uClipBoard) then
    raise EDamInternalExcept.Create('Error getting clipboard service');

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
var
  Handled: Boolean;
begin
  if Assigned(DamMsg.Dam.OnHelpClick) then
  begin
    Handled := False;
    DamMsg.Dam.OnHelpClick(DamMsg.Dam, DamMsg, Handled);
    if Handled then Exit;    
  end;

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
