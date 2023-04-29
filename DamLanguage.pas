unit DamLanguage;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

{$R Dam_Resource.res}

type
  TDamLanguage = (dgEnglish, dgPortuguese, dgSpanish, dgGerman, dgItalian,
    dgChinese, dgJapanese, dgGreek, dgRussian, dgFrench, dgPolish, dgDutch,
    dgTurkish);

  TDamLanguageDefinition = record
    OK, Yes, No, Info, Quest, Warn, Error, Msg: string;
  end;

  TDamResourceAccess = class(TObject);

procedure SetDamLangBySysLang(var DamLang: TDamLanguage);
function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;

implementation

uses
{$IFDEF FPC}
  Classes, SysUtils, IniFiles
{$ELSE}
  System.Classes, System.SysUtils, System.Types, System.IniFiles
{$ENDIF};

type
  TLanguageParams = record
    Name: string;
    DamLang: TDamLanguage;
    SysLang: ShortInt;
  end;

const
  LANGUAGES_PARAMS: array[0..12] of TLanguageParams = (
    (Name: 'English'   ; DamLang: dgEnglish   ; SysLang: $09 ),
    (Name: 'Portuguese'; DamLang: dgPortuguese; SysLang: $16 ),
    (Name: 'Spanish'   ; DamLang: dgSpanish   ; SysLang: $0a ),
    (Name: 'German'    ; DamLang: dgGerman    ; SysLang: $07 ),
    (Name: 'Italian'   ; DamLang: dgItalian   ; SysLang: $10 ),
    (Name: 'Chinese'   ; DamLang: dgChinese   ; SysLang: $04 ),
    (Name: 'Japanese'  ; DamLang: dgJapanese  ; SysLang: $11 ),
    (Name: 'Greek'     ; DamLang: dgGreek     ; SysLang: $08 ),
    (Name: 'Russian'   ; DamLang: dgRussian   ; SysLang: $19 ),
    (Name: 'French'    ; DamLang: dgFrench    ; SysLang: $01 ),
    (Name: 'Polish'    ; DamLang: dgPolish    ; SysLang: $15 ),
    (Name: 'Dutch'     ; DamLang: dgDutch     ; SysLang: $13 ),
    (Name: 'Turkish'   ; DamLang: dgTurkish   ; SysLang: $1f )
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
    R := TResourceStream.Create({$IFDEF FPC}HInstance{$ELSE}FindClassHInstance(TDamResourceAccess){$ENDIF}, 'DAM_LANG', RT_RCDATA);
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
