unit DamLanguage;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses DamUnit;

{$R Dam_Resource.res}

type
  TDamLanguageDefinition = record
    OK, Yes, No, Info, Quest, Warn, Error, Msg: string;
  end;

function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;

implementation

uses
{$IFDEF FPC}
  Classes, SysUtils, IniFiles, Windows
{$ELSE}
  System.Classes, System.SysUtils, System.IniFiles, Winapi.Windows
{$ENDIF};

function LoadLanguage(Language: TDamLanguage): TDamLanguageDefinition;
var
  aLang: string;
  R: TResourceStream;
  S: TStringList;
  Ini: TMemIniFile;
begin
  case Language of
    dgEnglish: aLang := 'English';
    dgPortuguese: aLang := 'Portuguese';
    dgSpanish: aLang := 'Spanish';
    dgGerman: aLang := 'German';
    dgItalian: aLang := 'Italian';
    dgChinese: aLang := 'Chinese';
    dgJapanese: aLang := 'Japanese';
    dgGreek: aLang := 'Greek';
    dgRussian: aLang := 'Russian';
    dgFrench: aLang := 'French';
    dgPolish: aLang := 'Polish';
    dgDutch: aLang := 'Dutch';
    else raise Exception.Create('Unknown language');
  end;

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
