{------------------------------------------------------------------------------
TDam component
Developed by Rodrigo Depine Dalpiaz (digao dalpiaz)
Non visual component to manage message dialogs

https://github.com/digao-dalpiaz/Dam

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

unit DamUnit;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF FPC}
  Classes, SysUtils, Graphics, ImgList;
{$ELSE}
  System.Classes, System.SysUtils, Vcl.Graphics, Vcl.ImgList;
{$ENDIF}

const
  DAM_PARAM_EXCEPTION = '{except}';
  DAM_PARAM_IDENT = '%p';

type
  TDamLanguage = (dgEnglish, dgPortuguese, dgSpanish, dgGerman, dgItalian,
    dgChinese, dgJapanese, dgGreek, dgRussian, dgFrench, dgPolish, dgDutch);

  TDamDlgPosition = (dpScreenCenter, dpActiveFormCenter, dpMainFormCenter);

  TDamMsgRes = 1..3;
  TDamMsgTitle = (dtApp, dtParentForm, dtMainForm, dtByIcon, dtCustom);
  TDamMsgIcon = (diApp, diInfo, diQuest, diWarn, diError, diCustom);
  TDamMsgButtons = (dbOK, dbYesNo, dbOne, dbTwo, dbThree);
  TDamParams = TArray<Variant>;

  TDamMsg = class;
  TDamMsgShowEvent = procedure(Sender: TObject; Msg: TDamMsg; var MsgText: String;
    var Handled: Boolean; var MsgResult: TDamMsgRes) of object;

  TDam = class(TComponent)
  private
    FAbout: String;
    FLanguage: TDamLanguage;
    FRaises: Boolean;
    FSounds: Boolean;
    FImages: TCustomImageList;
    FFont: TFont;
    FDefault: Boolean;
    FUnit: String;
    FColorMsg, FColorBtn: TColor;
    FCenterButtons: Boolean;
    FDialogPosition: TDamDlgPosition;
    FDialogBorder: Boolean;
    FShowEvent: TDamMsgShowEvent;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetFont(const Value: TFont);
    function GetFontStored: Boolean;

    function ShowDialog(Msg: TDamMsg; const Text: String): TDamMsgRes;
    procedure OnError(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property About: String read FAbout;
    property Language: TDamLanguage read FLanguage write FLanguage;
    property HandleExceptions: Boolean read FRaises write FRaises default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MessageFont: TFont read FFont write SetFont stored GetFontStored;
    property DamDefault: Boolean read FDefault write FDefault default False;
    property DamUnitName: String read FUnit write FUnit;
    property PlaySounds: Boolean read FSounds write FSounds default True;
    property MessageColor: TColor read FColorMsg write FColorMsg default clWhite;
    property ButtonsColor: TColor read FColorBtn write FColorBtn default clBtnFace;
    property CenterButtons: Boolean read FCenterButtons write FCenterButtons default False;
    property DialogPosition: TDamDlgPosition read FDialogPosition write FDialogPosition default dpScreenCenter;
    property DialogBorder: Boolean read FDialogBorder write FDialogBorder default True;
    property OnShowMessage: TDamMsgShowEvent read FShowEvent write FShowEvent;
  end;

  TDamMsg = class(TComponent)
  private
    FCustomTitle: String;
    FCustomIcon: TIcon;
    FTitle: TDamMsgTitle;
    FIcon: TDamMsgIcon;
    FMessage: String;
    FButton1: String;
    FButton2: String;
    FButton3: String;
    FButtons: TDamMsgButtons;
    FSwapFocus: Boolean;
    FRaise: Boolean;
    FFixedWidth: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: String;

    FDam: TDam;

    procedure SetIcon(const Value: TIcon);
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
    property CustomTitle: String read FCustomTitle write FCustomTitle;
    property CustomIcon: TIcon read FCustomIcon write SetIcon;
    property Title: TDamMsgTitle read FTitle write FTitle default dtByIcon;
    property Icon: TDamMsgIcon read FIcon write FIcon default diInfo;
    property Message: String read FMessage write FMessage;
    property Button1: String read FButton1 write FButton1;
    property Button2: String read FButton2 write FButton2;
    property Button3: String read FButton3 write FButton3;
    property Buttons: TDamMsgButtons read FButtons write FButtons default dbOK;
    property SwapFocus: Boolean read FSwapFocus write FSwapFocus default False;
    property RaiseExcept: Boolean read FRaise write FRaise default False;
    property FixedWidth: Integer read FFixedWidth write FFixedWidth default 0;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property HelpKeyword: String read FHelpKeyword write FHelpKeyword;

    property Dam: TDam read FDam write FDam;
  end;

procedure MsgInfo(const Msg: String; const Params: TDamParams = nil);
procedure MsgWarn(const Msg: String; const Params: TDamParams = nil);
procedure MsgError(const Msg: String; const Params: TDamParams = nil);
function MsgQuest(const Msg: String; const Params: TDamParams = nil): Boolean;
procedure ShowErrorMsg;
function CaptureErrorMsg: String;
procedure MsgRaise(const Msg: String; const Params: TDamParams = nil);

function DamParams(const Params: array of Variant): TDamParams; //compatibility with old dynamic array

implementation

uses
  DamDialog, DzHTMLText,
{$IFDEF FPC}
  Forms, StrUtils, Windows;
{$ELSE}
  Vcl.Forms, Winapi.Windows, System.UITypes, System.StrUtils;
{$ENDIF}

const STR_VERSION = '4.5';

var ObjDefault: TDam = nil;

type EDamException = class(Exception)
  DamMsg: TDamMsg;
  constructor Create(aDamMsg: TDamMsg; const aText: String);
end;

constructor EDamException.Create(aDamMsg: TDamMsg; const aText: String);
begin
  inherited Create(aText);
  DamMsg := aDamMsg;
end;

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

function CaptureErrorMsg: String;
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

function QuickMsg(const Msg: String; const Params: TDamParams; Kind: TDamMsgIcon;
  RaiseExcept: Boolean = False): Boolean;
var M: TDamMsg;
begin
  CheckDamDefault;

  M := TDamMsg.Create(nil);
  try
    M.Dam := ObjDefault;
    M.Message := Msg;
    M.Icon := Kind;
    if Kind = diQuest then M.Buttons := dbYesNo;
    M.RaiseExcept := RaiseExcept;
    Result := M.RunAsBool(Params);
  finally
    M.Free;
  end;
end;

procedure MsgInfo(const Msg: String; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diInfo);
end;

procedure MsgWarn(const Msg: String; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diWarn);
end;

procedure MsgError(const Msg: String; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diError);
end;

function MsgQuest(const Msg: String; const Params: TDamParams): Boolean;
begin
  Result := QuickMsg(Msg, Params, diQuest);
end;

procedure MsgRaise(const Msg: String; const Params: TDamParams);
begin
  QuickMsg(Msg, Params, diError, True);
end;

//

function ParseParams(const Msg: String; const Params: TDamParams): String;
var
  OffSet, I, IdxPar: Integer;
  A, aPar: String;
begin
  A := Msg;

  IdxPar := -1;
  OffSet := 1;

  while True do
  begin
    I := PosEx(DAM_PARAM_IDENT, A, OffSet);
    if I=0 then Break;

    Inc(IdxPar);
    if IdxPar>High(Params) then
      raise Exception.CreateFmt('DAM: Parameter index %d not found', [IdxPar]);

    Delete(A, I, Length(DAM_PARAM_IDENT));

    aPar := TDzHTMLText.EscapeTextToHTML(Params[IdxPar]);

    Insert(aPar, A, I);
    OffSet := I+Length(aPar);
  end;

  if A.Contains(DAM_PARAM_EXCEPTION) then
    A := StringReplace(A, DAM_PARAM_EXCEPTION, TDzHTMLText.EscapeTextToHTML(CaptureErrorMsg), [rfReplaceAll]);

  Result := A;
end;

// -- TDamMsg

constructor TDamMsg.Create(AOwner: TComponent);
begin
  inherited;

  FCustomIcon := TIcon.Create;
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
var
  newMsg: String;
begin
  newMsg := ParseParams(FMessage, Params);

  if FRaise then
    raise EDamException.Create(Self, newMsg);
  //else
  Result := Dam.ShowDialog(Self, newMsg);
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

procedure TDamMsg.SetIcon(const Value: TIcon);
begin
  FCustomIcon.Assign(Value);
end;

// -- TDam

constructor TDam.Create(AOwner: TComponent);
begin
  inherited;

  FAbout := 'Digao Dalpiaz / Version '+STR_VERSION;

  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 10;

  FSounds := True;

  FColorMsg := clWhite;
  FColorBtn := clBtnFace;

  FDialogBorder := True;

  case SysLocale.PriLangID of
    LANG_ENGLISH: FLanguage := dgEnglish;
    LANG_PORTUGUESE: FLanguage := dgPortuguese;
    LANG_SPANISH: FLanguage := dgSpanish;
    LANG_GERMAN: FLanguage := dgGerman;
    LANG_ITALIAN: FLanguage := dgItalian;
    LANG_CHINESE: FLanguage := dgChinese;
    LANG_JAPANESE: FLanguage := dgJapanese;
    LANG_GREEK: FLanguage := dgGreek;
    LANG_RUSSIAN: FLanguage := dgRussian;
    LANG_FRENCH: FLanguage := dgFrench;
    LANG_POLISH: FLanguage := dgPolish;
    LANG_DUTCH: FLanguage := dgDutch;
  end;
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
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      FImages := nil;
  end;
end;

procedure TDam.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);
  end;
end;

procedure TDam.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

function TDam.ShowDialog(Msg: TDamMsg; const Text: String): TDamMsgRes;
var
  newMsg: String;
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
var Msg: TDamMsg;
begin
  if E is EDamException then
  begin
    ShowDialog(EDamException(E).DamMsg, EDamException(E).Message);
  end else
  begin
    Msg := TDamMsg.Create(nil);
    try
      Msg.Dam := Self;
      Msg.Icon := diError;
      ShowDialog(Msg, TDzHTMLText.EscapeTextToHTML(E.Message));
    finally
      Msg.Free;
    end;
  end;
end;

function TDam.GetFontStored: Boolean;
begin
  Result := not (
   (FFont.Charset = DEFAULT_CHARSET) and
   (FFont.Color = clWindowText) and
   (FFont.Name = 'Segoe UI') and
   (FFont.Orientation = 0) and
   (FFont.Pitch = fpDefault) and
   (FFont.Size = 10) and
   (FFont.Style = []) and
   (FFont.Quality = fqDefault)
  );
end;

initialization
  {$IFNDEF FPC}System.{$ENDIF}Classes.RegisterClass(TDamMsg);

  if DZHTMLTEXT_INTERNAL_VERSION <> 701 then
    raise Exception.Create('Please, update DzHTMLText component.');

end.
