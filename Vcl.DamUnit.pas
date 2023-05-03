{------------------------------------------------------------------------------
TDam component
Developed by Rodrigo Depine Dalpiaz (digao dalpiaz)
Non visual component to manage message dialogs

https://github.com/digao-dalpiaz/Dam

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

{$IFNDEF FMX}unit Vcl.DamUnit;{$ENDIF}

{$INCLUDE 'Defines.inc'}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  DamLanguage,
{$IFDEF FPC}
  Classes, SysUtils, Graphics, ImgList;
{$ELSE}
  System.Classes, System.SysUtils, System.UITypes
  {$IFDEF FMX}
    , FMX.Types
    {$IFDEF USE_NEW_UNITS}, FMX.Graphics{$ENDIF}
    {$IFDEF USE_IMGLST}, FMX.ImgList{$ENDIF}
  {$ELSE}
  , Vcl.Graphics, Vcl.ImgList
  {$ENDIF};
{$ENDIF}

const
  DAM_PARAM_EXCEPTION = '{except}';
  DAM_PARAM_IDENT = '%p';

  DEF_FONT_NAME = 'Segoe UI';
  DEF_FONT_SIZE = 10;

  DEF_MSG_BACKGROUND = {$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};
  DEF_BTN_BACKGROUND = {$IFDEF FMX}TAlphaColors.Null{$ELSE}clBtnFace{$ENDIF};

type
  {$IFDEF FMX}
  TAnyColor = TAlphaColor;
  TDamIconObj = TBitmap;
  {$ELSE}
  TAnyColor = TColor;
  TDamIconObj = TIcon;
  {$ENDIF}

  TDamDlgPosition = (dpScreenCenter, dpActiveFormCenter, dpMainFormCenter);

  TDamMsgRes = 1..3;
  TDamMsgTitle = (dtApp, dtParentForm, dtMainForm, dtByIcon, dtCustom);
  TDamMsgIcon = (diApp, diInfo, diQuest, diWarn, diError, diCustom);
  TDamMsgButtons = (dbOK, dbYesNo, dbOne, dbTwo, dbThree);
  TDamParams = TArray<Variant>;

  TDamMsg = class;
  TDamShowEvent = procedure(Sender: TObject; Msg: TDamMsg; var MsgText: string;
    var Handled: Boolean; var MsgResult: TDamMsgRes) of object;
  TDamLinkClickEvent = procedure(Sender: TObject; Msg: TDamMsg;
    const Target: string; var Handled: Boolean;
    var CloseMsg: Boolean; var MsgResult: TDamMsgRes) of object;

  TDam = class(TComponent)
  private
    FAbout: string;
    FLanguage: TDamLanguage;
    FRaises: Boolean;
    FSounds: Boolean;
    FHideIcon: Boolean;

    {$IFDEF USE_IMGLST}
    FImages: TCustomImageList;
    {$ENDIF}

    FFont: TFont;
    {$IFDEF FMX}
    FFontColor: TAnyColor;
    {$ENDIF}
    FDefault: Boolean;
    FUnit: string;
    FColorMsg, FColorBtn: TAnyColor;
    FCenterButtons: Boolean;
    FDialogPosition: TDamDlgPosition;
    FDialogBorder: Boolean;
    FShowEvent: TDamShowEvent;
    FLinkClick: TDamLinkClickEvent;

    {$IFDEF USE_IMGLST}
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}

    procedure SetFont(const Value: TFont);
    function GetFontStored: Boolean;

    function ShowDialog(Msg: TDamMsg; const Text: string): TDamMsgRes;
    procedure OnError(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property About: string read FAbout;
    property Language: TDamLanguage read FLanguage write FLanguage;
    property HandleExceptions: Boolean read FRaises write FRaises default False;
    {$IFDEF USE_IMGLST}
    property Images: TCustomImageList read FImages write SetImages;
    {$ENDIF}
    property MessageFont: TFont read FFont write SetFont stored GetFontStored;
    {$IFDEF FMX}
    property MessageFontColor: TAnyColor read FFontColor write FFontColor default TAlphaColors.Black;
    {$ENDIF}
    property DamDefault: Boolean read FDefault write FDefault default False;
    property DamUnitName: string read FUnit write FUnit;
    property PlaySounds: Boolean read FSounds write FSounds default True;
    property HideIcon: Boolean read FHideIcon write FHideIcon default False;
    property MessageColor: TAnyColor read FColorMsg write FColorMsg default DEF_MSG_BACKGROUND;
    property ButtonsColor: TAnyColor read FColorBtn write FColorBtn default DEF_BTN_BACKGROUND;
    property CenterButtons: Boolean read FCenterButtons write FCenterButtons default False;
    property DialogPosition: TDamDlgPosition read FDialogPosition write FDialogPosition default dpScreenCenter;
    property DialogBorder: Boolean read FDialogBorder write FDialogBorder default True;
    property OnShowMessage: TDamShowEvent read FShowEvent write FShowEvent;
    property OnLinkClick: TDamLinkClickEvent read FLinkClick write FLinkClick;
  end;

  TDamMsg = class(TComponent)
  private
    FCustomTitle: string;
    FCustomIcon: TDamIconObj;
    FTitle: TDamMsgTitle;
    FIcon: TDamMsgIcon;
    FMessage: string;
    FButton1: string;
    FButton2: string;
    FButton3: string;
    FButtons: TDamMsgButtons;
    FSwapFocus: Boolean;
    FRaise: Boolean;
    FFixedWidth: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: string;

    FDam: TDam;

    procedure SetIcon(const Value: TDamIconObj);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Preview;
    function Run(const Params: TDamParams = nil): TDamMsgRes;
    function RunAsBool(const Params: TDamParams = nil): Boolean;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    procedure Assign(Source: TPersistent); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property CustomTitle: string read FCustomTitle write FCustomTitle;
    property CustomIcon: TDamIconObj read FCustomIcon write SetIcon;
    property Title: TDamMsgTitle read FTitle write FTitle default dtByIcon;
    property Icon: TDamMsgIcon read FIcon write FIcon default diInfo;
    property Message: string read FMessage write FMessage;
    property Button1: string read FButton1 write FButton1;
    property Button2: string read FButton2 write FButton2;
    property Button3: string read FButton3 write FButton3;
    property Buttons: TDamMsgButtons read FButtons write FButtons default dbOK;
    property SwapFocus: Boolean read FSwapFocus write FSwapFocus default False;
    property RaiseExcept: Boolean read FRaise write FRaise default False;
    property FixedWidth: Integer read FFixedWidth write FFixedWidth default 0;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property HelpKeyword: string read FHelpKeyword write FHelpKeyword;

    property Dam: TDam read FDam write FDam;
  end;

  EDam = class(Exception)
  private
    FDamMsg: TDamMsg;
  public
    property DamMsg: TDamMsg read FDamMsg;

    constructor Create(const Msg: string; const Params: TDamParams = nil); overload;
    constructor Create(DamMsg: TDamMsg; const Params: TDamParams = nil); overload;
  end;

procedure MsgInfo(const Msg: string; const Params: TDamParams = nil);
procedure MsgWarn(const Msg: string; const Params: TDamParams = nil);
procedure MsgError(const Msg: string; const Params: TDamParams = nil);
function MsgQuest(const Msg: string; const Params: TDamParams = nil): Boolean;
procedure ShowErrorMsg;
function CaptureErrorMsg: string;
procedure MsgRaise(const Msg: string; const Params: TDamParams = nil);

function DamParams(const Params: array of Variant): TDamParams; //compatibility with old dynamic array

implementation

uses
{$IFDEF FMX}FMX.DzHTMLText, FMX.DamDialog{$ELSE}Vcl.DzHTMLText, Vcl.DamDialog{$ENDIF},
{$IFDEF FPC}
  Forms
{$ELSE}
  {$IFDEF FMX}
  FMX.Forms
  {$ELSE}
  Vcl.Forms
  {$ENDIF}
{$ENDIF};

const STR_VERSION = '6.0';

var ObjDefault: TDam = nil;

procedure CheckExceptObject;
begin
  if ExceptObject=nil then
    raise Exception.Create('DAM: ExceptObject nil');
end;

procedure CheckDamDefault;
begin
  if ObjDefault=nil then
    raise Exception.Create('DAM: Default TDam not found');
end;

function CaptureErrorMsg: string;
begin
  CheckExceptObject;

  Result := Exception(ExceptObject).Message;
end;

procedure ShowErrorMsg;
begin
  CheckExceptObject;
  CheckDamDefault;

  ObjDefault.OnError(nil, Exception(ExceptObject));
end;

function DamParams(const Params: array of Variant): TDamParams;
var I: Integer;
begin
  SetLength(Result, Length(Params));
  for I := Low(Params) to High(Params) do
    Result[I] := Params[I];
end;

function QuickMsg(const Msg: string; const Params: TDamParams; Kind: TDamMsgIcon): Boolean;
var M: TDamMsg;
begin
  CheckDamDefault;

  M := TDamMsg.Create(nil);
  try
    M.Dam := ObjDefault;
    M.Message := Msg;
    M.Icon := Kind;
    if Kind = diQuest then M.Buttons := dbYesNo;
    Result := M.RunAsBool(Params);
  finally
    M.Free;
  end;
end;

procedure MsgInfo(const Msg: string; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diInfo);
end;

procedure MsgWarn(const Msg: string; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diWarn);
end;

procedure MsgError(const Msg: string; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diError);
end;

function MsgQuest(const Msg: string; const Params: TDamParams): Boolean;
begin
  Result := QuickMsg(Msg, Params, diQuest);
end;

procedure MsgRaise(const Msg: string; const Params: TDamParams);
begin
  raise EDam.Create(Msg, Params);
end;

//

function PosOfAnyString(const Args: array of string; const Text: string; Offset: Integer;
  out ArgIdx: Integer; out iPos: Integer): Boolean;
var
  I, J: Integer;
begin
  for I := Offset to Length(Text) do
  begin
    for J := Low(Args) to High(Args) do
    begin
      if Copy(Text, I, Length(Args[J]))=Args[J] then
      begin
        ArgIdx := J;
        iPos := I;
        Exit(True);
      end;
    end;
  end;

  Exit(False);
end;

function ParseParams(const Msg: string; const Params: TDamParams): string;
const ARGS: array[0..1] of string = (DAM_PARAM_IDENT, DAM_PARAM_EXCEPTION);
var
  A, aPar: string;
  I, Offset, IdxPar, ArgIdx: Integer;
begin
  A := Msg;

  IdxPar := -1;
  Offset := 1;

  while PosOfAnyString(ARGS, A, Offset, ArgIdx, I) do
  begin
    if ArgIdx=0 then
    begin
      Inc(IdxPar);
      if IdxPar>High(Params) then
        raise Exception.CreateFmt('DAM: Parameter index %d not found', [IdxPar]);
    end;

    Delete(A, I, Length(ARGS[ArgIdx]));

    case ArgIdx of
      0: aPar := Params[IdxPar];
      1: aPar := CaptureErrorMsg;
    end;
    aPar := TDzHTMLText.EscapeTextToHTML(aPar);

    Insert(aPar, A, I);
    Offset := I+Length(aPar);
  end;

  Result := A;
end;

// -- TDamMsg

constructor TDamMsg.Create(AOwner: TComponent);
begin
  inherited;

  FCustomIcon := TDamIconObj.Create{$IFDEF USE_FMX_OLD_ENV}(0, 0){$ENDIF};
  FTitle := dtByIcon;
  FIcon := diInfo;
  FButtons := dbOK;
end;

destructor TDamMsg.Destroy;
begin
  FCustomIcon.Free;

  FDam := nil;

  inherited;
end;

procedure TDamMsg.Preview;
begin
  RunDamDialog(Self, FMessage);
end;

function TDamMsg.Run(const Params: TDamParams): TDamMsgRes;
begin
  if FRaise then
    raise EDam.Create(Self, Params);
  //else
  Result := Dam.ShowDialog(Self, ParseParams(FMessage, Params));
end;

function TDamMsg.RunAsBool(const Params: TDamParams): Boolean;
begin
  Result := Run(Params)=1;
end;

procedure TDamMsg.Assign(Source: TPersistent);
var SourceMsg: TDamMsg;
begin
  if Source is TDamMsg then
  begin
    SourceMsg := TDamMsg(Source);

    FCustomTitle := SourceMsg.FCustomTitle;
    FCustomIcon.Assign(SourceMsg.FCustomIcon);
    FTitle := SourceMsg.FTitle;
    FIcon := SourceMsg.FIcon;
    FMessage := SourceMsg.FMessage;
    FButton1 := SourceMsg.FButton1;
    FButton2 := SourceMsg.FButton2;
    FButton3 := SourceMsg.FButton3;
    FButtons := SourceMsg.FButtons;
    FSwapFocus := SourceMsg.FSwapFocus;
    FRaise := SourceMsg.FRaise;
  end
    else raise Exception.Create('Source must be TDamMsg');
end;

function TDamMsg.HasParent: Boolean;
begin
  Result := True;
end;

function TDamMsg.GetParentComponent: TComponent;
begin
  Result := FDam;
end;

procedure TDamMsg.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
    if (AComponent = FDam) then Free;
end;

procedure TDamMsg.SetIcon(const Value: TDamIconObj);
begin
  FCustomIcon.Assign(Value);
end;

// -- TDam

constructor TDam.Create(AOwner: TComponent);
begin
  inherited;

  FAbout := 'Digao Dalpiaz / Version '+STR_VERSION;

  FFont := TFont.Create;
  FFont.{$IFDEF FMX}Family{$ELSE}Name{$ENDIF} := DEF_FONT_NAME;
  {$IFDEF FMX}
  FFontColor := TAlphaColors.Black;
  {$ENDIF}
  FFont.Size := DEF_FONT_SIZE;

  FSounds := True;

  FColorMsg := DEF_MSG_BACKGROUND;
  FColorBtn := DEF_BTN_BACKGROUND;

  FDialogBorder := True;

  SetDamLangBySysLang(FLanguage);
end;

destructor TDam.Destroy;
begin
  FFont.Free;

  if not (csDesigning in ComponentState) then
  begin
    if FRaises then Application.OnException := nil; //avoid AV
  end;

  inherited;
end;

procedure TDam.GetChildren(Proc: TGetChildProc; Root: TComponent);
var C: TComponent;
begin
  for C in Owner do
    if C.GetParentComponent = Self then Proc(C);
end;

procedure TDam.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    if FRaises then
      Application.OnException := OnError;

    if FDefault then
      ObjDefault := Self;
  end;
end;

procedure TDam.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  {$IFDEF USE_IMGLST}
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      FImages := nil;
  end;
  {$ENDIF}
end;

{$IFDEF USE_IMGLST}
procedure TDam.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);
  end;
end;
{$ENDIF}

procedure TDam.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

function TDam.GetFontStored: Boolean;
begin
  Result := not (
   {$IFDEF FMX}
   (FFont.Family = DEF_FONT_NAME) and
   (FFont.Size = DEF_FONT_SIZE) and
   (FFont.Style = [])
   {$ELSE}
   (FFont.Charset = GetDefFontCharSet) and
   (FFont.Color = clWindowText) and
   (FFont.Name = DEF_FONT_NAME) and
   (FFont.Orientation = 0) and
   (FFont.Pitch = fpDefault) and
   (FFont.Size = DEF_FONT_SIZE) and
   (FFont.Style = []) and
   (FFont.Quality = fqDefault)
   {$ENDIF}
  );
end;

function TDam.ShowDialog(Msg: TDamMsg; const Text: string): TDamMsgRes;
var
  newMsg: string;
  Handled: Boolean; HndRes: TDamMsgRes;
begin
  newMsg := Text;

  if Assigned(FShowEvent) then
  begin
    Handled := False;
    HndRes := 1; //default
    FShowEvent(Self, Msg, newMsg, Handled, HndRes);
    if Handled then Exit(HndRes);
  end;

  Result := RunDamDialog(Msg, newMsg);
end;

procedure TDam.OnError(Sender: TObject; E: Exception);
var
  Msg: TDamMsg;
  Text: string;
begin
  if (E is EDam) and (EDam(E).DamMsg<>nil) then
  begin
    ShowDialog(EDam(E).DamMsg, E.Message);
  end else
  begin
    if E is EDam then
      Text := E.Message
    else
      Text := TDzHTMLText.EscapeTextToHTML(E.Message);

    Msg := TDamMsg.Create(nil);
    try
      Msg.Dam := Self;
      Msg.Icon := diError;
      ShowDialog(Msg, Text);
    finally
      Msg.Free;
    end;
  end;
end;

constructor EDam.Create(const Msg: string; const Params: TDamParams);
begin
  inherited Create(ParseParams(Msg, Params));
end;

constructor EDam.Create(DamMsg: TDamMsg; const Params: TDamParams);
begin
  inherited Create(ParseParams(DamMsg.Message, Params));
  FDamMsg := DamMsg;
end;

initialization
  if DZHTMLTEXT_INTERNAL_VERSION <> 707 then
    raise Exception.Create('Please, update DzHTMLText component.');

end.
