unit DamLanguage;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses DamUnit;

{$R Dam_Resource.res}

type
  TDamLanguageDefinition = record
    OK, Yes, No, Info, Quest, Warn, Error, Msg: string;
  end;

procedure SetDamLangBySysLang(var DamLang: TDamLanguage);
function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;

implementation

uses
{$IFDEF FPC}
  Classes, SysUtils, IniFiles, Windows
{$ELSE}
  System.Classes, System.SysUtils, System.IniFiles, Winapi.Windows
{$ENDIF};

type
  TLanguageParams = record
    Name: string;
    DamLang: TDamLanguage;
    SysLang: ShortInt;
  end;

const
  LANGUAGES_PARAMS: array[0..12] of TLanguageParams = (
    (Name: 'English'   ; DamLang: dgEnglish   ; SysLang: LANG_ENGLISH   ),
    (Name: 'Portuguese'; DamLang: dgPortuguese; SysLang: LANG_PORTUGUESE),
    (Name: 'Spanish'   ; DamLang: dgSpanish   ; SysLang: LANG_SPANISH   ),
    (Name: 'German'    ; DamLang: dgGerman    ; SysLang: LANG_GERMAN    ),
    (Name: 'Italian'   ; DamLang: dgItalian   ; SysLang: LANG_ITALIAN   ),
    (Name: 'Chinese'   ; DamLang: dgChinese   ; SysLang: LANG_CHINESE   ),
    (Name: 'Japanese'  ; DamLang: dgJapanese  ; SysLang: LANG_JAPANESE  ),
    (Name: 'Greek'     ; DamLang: dgGreek     ; SysLang: LANG_GREEK     ),
    (Name: 'Russian'   ; DamLang: dgRussian   ; SysLang: LANG_RUSSIAN   ),
    (Name: 'French'    ; DamLang: dgFrench    ; SysLang: LANG_FRENCH    ),
    (Name: 'Polish'    ; DamLang: dgPolish    ; SysLang: LANG_POLISH    ),
    (Name: 'Dutch'     ; DamLang: dgDutch     ; SysLang: LANG_DUTCH     ),
    (Name: 'Turkish'   ; DamLang: dgTurkish   ; SysLang: LANG_TURKISH   )
  );

procedure SetDamLangBySysLang(var DamLang: TDamLanguage);
var
  P: TLanguageParams;
begin
  for P in LANGUAGES_PARAMS do
    if P.SysLang = SysLocale.PriLangID then
    begin
      DamLang := P.DamLang;
      Break;
    end;

  //if not found, it's another unsupported language - leave initial language
end;

function GetLangNameByDamLang(DamLang: TDamLanguage): string;
var
  P: TLanguageParams;
begin
  for P in LANGUAGES_PARAMS do
    if P.DamLang = DamLang then Exit(P.Name);

  raise Exception.Create('Invalid language');
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
    R := TResourceStream.Create({$IFDEF FPC}HInstance{$ELSE}FindClassHInstance(TDam){$ENDIF}, 'DAM_LANG', RT_RCDATA);
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
      raise Exception.CreateFmt('Language "%s" not found in resource', [aLang]);

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
