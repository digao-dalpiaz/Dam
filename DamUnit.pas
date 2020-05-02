{------------------------------------------------------------------------------
TDam component
Developed by Rodrigo Depiné Dalpiaz (digão dalpiaz)
Non visual component to manage message dialogs

https://github.com/digao-dalpiaz/Dam

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

unit DamUnit;

interface

uses System.Classes, System.SysUtils, Vcl.Graphics;

type
  TDamLanguage = (dgEnglish, dgPortuguese, dgSpanish, dgGerman, dgItalian,
    dgChinese, dgJapanese, dgGreek, dgRussian, dgFrench, dgPolish);

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
    FFont: TFont;
    FDefault: Boolean;
    FUnit: String;
    FColorMsg, FColorBtn: TColor;
    FCenterButtons: Boolean;
    FDialogPosition: TDamDlgPosition;
    FDialogBorder: Boolean;
    FShowEvent: TDamMsgShowEvent;
    procedure SetFont(const Value: TFont);
    function GetFontStored: Boolean;

    procedure OnError(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
  published
    property About: String read FAbout;
    property Language: TDamLanguage read FLanguage write FLanguage;
    property HandleExceptions: Boolean read FRaises write FRaises default False;
    property MessageFont: TFont read FFont write SetFont stored GetFontStored;
    property DamDefault: Boolean read FDefault write FDefault default False;
    property DamUnitName: String read FUnit write FUnit;
    property PlaySounds: Boolean read FSounds write FSounds default True;
    property MessageColor: TColor read FColorMsg write FColorMsg default clWhite;
    property ButtonsColor: TColor read FColorBtn write FColorBtn default clBtnFace;
    property CenterButtons: Boolean read FCenterButtons write FCenterButtons default True;
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

uses DamDialog, Vcl.Forms, Winapi.Windows, System.UITypes, System.StrUtils;

var ObjDefault: TDam = nil;

type EDamException = class(Exception)
  DamMsg: TDamMsg;
  constructor Create(const aText: String; aDamMsg: TDamMsg);
end;

constructor EDamException.Create(const aText: String; aDamMsg: TDamMsg);
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

function QuickMsg(const Msg: String; const Params: TDamParams; Kind: TDamMsgIcon): Boolean;
var M: TDamMsg;
begin
  CheckDamDefault;

  M := TDamMsg.Create(nil);
  try
    M.Dam := ObjDefault;
    M.Icon := Kind;
    M.Message := Msg;
    if Kind = diQuest then M.Buttons := dbYesNo;
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

procedure ParseParams(var A: String; const Params: TDamParams);
const PARAM_EXCEPTION = '{except}';
      PARAM_ID = '%p';
var OffSet, I, IdxPar: Integer;
    aPar: String;
begin
  IdxPar := 0;
  OffSet := 1;

  repeat
    I := PosEx(PARAM_ID, A, OffSet);
    if I>0 then
    begin
      Delete(A, I, Length(PARAM_ID));

      if IdxPar>High(Params) then
        raise Exception.CreateFmt('DAM: Parameter index %d not found', [IdxPar]);

      aPar := Params[IdxPar];
      Inc(IdxPar);

      Insert(aPar, A, I);
      OffSet := I+Length(aPar);
    end;
  until I=0;

  if A.Contains(PARAM_EXCEPTION) then
    A := StringReplace(A, PARAM_EXCEPTION, CaptureErrorMsg, [rfReplaceAll]);
end;

procedure MsgRaise(const Msg: String; const Params: TDamParams);
var newMsg: String;
begin
  newMsg := Msg;
  ParseParams(newMsg, Params);

  raise Exception.Create(newMsg);
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
  Handled: Boolean; HndRes: TDamMsgRes;
begin
  newMsg := FMessage;
  ParseParams(newMsg, Params);

  if Assigned(FDam.FShowEvent) then
  begin
    Handled := False;
    HndRes := 1; //default
    FDam.FShowEvent(FDam, Self, newMsg, Handled, HndRes);
    if Handled then Exit(HndRes);
  end;

  if FRaise then
    raise EDamException.Create(newMsg, Self);
  //else
  Result := RunDamDialog(Self, newMsg);
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

  FAbout := 'Digão Dalpiaz / Version 3.0';

  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 10;

  FSounds := True;

  FColorMsg := clWhite;
  FColorBtn := clBtnFace;

  FCenterButtons := True;
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

procedure TDam.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDam.OnError(Sender: TObject; E: Exception);
var Msg: TDamMsg;
begin
  if E is EDamException then
  begin
    RunDamDialog(EDamException(E).DamMsg, EDamException(E).Message);
  end else
  begin
    Msg := TDamMsg.Create(nil);
    try
      Msg.Dam := Self;
      Msg.Icon := diError;
      Msg.Message := E.Message;
      Msg.Run;
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
  System.Classes.RegisterClass(TDamMsg);

end.
