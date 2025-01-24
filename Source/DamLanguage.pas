unit DamLanguage;

interface

uses
{$IFDEF FPC}
  Classes
{$ELSE}
  System.Classes
{$ENDIF};

{$R Dam_Resource.res}

type
  TDamLanguage = (dgEnglish, dgPortuguese, dgSpanish, dgGerman, dgItalian,
    dgChinese, dgJapanese, dgGreek, dgRussian, dgFrench, dgPolish, dgDutch,
    dgTurkish, dgFarsi);

  TDamLanguageDefinition = record
    OK, Yes, No, Info, Quest, Warn, Error, Msg: string;
  end;

  TDamResourceAccess = class(TObject);

function GetResource(const Name: string): TResourceStream;

procedure SetDamLangBySysLang(var DamLang: TDamLanguage);
function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;

implementation

uses
  DamInternalExcept,
{$IFDEF FPC}
  SysUtils, IniFiles
{$ELSE}
  System.SysUtils, System.Types, System.IniFiles
{$ENDIF};

type
  TLanguageParams = record
    Name: string;
    DamLang: TDamLanguage;
    SysLang: ShortInt;
  end;

const
  LANGUAGES_PARAMS: array[0..13] of TLanguageParams = (
    (Name: 'English'   ; DamLang: dgEnglish   ; SysLang: $09 ),
    (Name: 'Portuguese'; DamLang: dgPortuguese; SysLang: $16 ),
    (Name: 'Spanish'   ; DamLang: dgSpanish   ; SysLang: $0a ),
    (Name: 'German'    ; DamLang: dgGerman    ; SysLang: $07 ),
    (Name: 'Italian'   ; DamLang: dgItalian   ; SysLang: $10 ),
    (Name: 'Chinese'   ; DamLang: dgChinese   ; SysLang: $04 ),
    (Name: 'Japanese'  ; DamLang: dgJapanese  ; SysLang: $11 ),
    (Name: 'Greek'     ; DamLang: dgGreek     ; SysLang: $08 ),
    (Name: 'Russian'   ; DamLang: dgRussian   ; SysLang: $19 ),
    (Name: 'French'    ; DamLang: dgFrench    ; SysLang: $0c ),
    (Name: 'Polish'    ; DamLang: dgPolish    ; SysLang: $15 ),
    (Name: 'Dutch'     ; DamLang: dgDutch     ; SysLang: $13 ),
    (Name: 'Turkish'   ; DamLang: dgTurkish   ; SysLang: $1f ),
    (Name: 'Farsi'     ; DamLang: dgFarsi     ; SysLang: $29 )
  );

procedure SetDamLangBySysLang(var DamLang: TDamLanguage);
var
  P: TLanguageParams;
begin
  for P in LANGUAGES_PARAMS do
    if P.SysLang = SysLocale.PriLangID then
    begin
      DamLang := P.DamLang;
      Exit;
    end;

  DamLang := dgEnglish; //default
end;

function GetLangNameByDamLang(DamLang: TDamLanguage): string;
var
  P: TLanguageParams;
begin
  for P in LANGUAGES_PARAMS do
    if P.DamLang = DamLang then Exit(P.Name);

  raise EDamInternalExcept.Create('Invalid language');
end;

function GetResource(const Name: string): TResourceStream;
begin
  Result := TResourceStream.Create(
    {$IFDEF FPC}HInstance{$ELSE}FindClassHInstance(TDamResourceAccess){$ENDIF},
    Name,
    {$IFDEF FPC}MAKEINTRESOURCE(10){$ELSE}RT_RCDATA{$ENDIF}
  );
end;

function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;
var
  aLang: string;
  R: TResourceStream;
  S: TStringList;
  Ini: TMemIniFile;
begin
  aLang := GetLangNameByDamLang(Language);

  S := TStringList.Create;
  try
    R := GetResource('DAM_LANG');
    try
      S.LoadFromStream(R, TEncoding.UTF8);
    finally
      R.Free;
    end;

    Ini := TMemIniFile.Create(EmptyStr);
    try
      Ini.SetStrings(S);
      S.Clear;
      Ini.ReadSectionValues(aLang, S);
    finally
      Ini.Free;
    end;

    if S.Count=0 then
      raise EDamInternalExcept.CreateFmt('Language "%s" not found in resource', [aLang]);

    with Result do
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
