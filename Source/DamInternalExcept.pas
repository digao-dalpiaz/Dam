unit DamInternalExcept;

interface

uses
{$IFDEF FPC}
  SysUtils
{$ELSE}
  System.SysUtils
{$ENDIF};


type
  EDamInternalExcept = class(Exception)
  public
    constructor Create(const Msg: string); overload;
  end;

implementation

constructor EDamInternalExcept.Create(const Msg: string);
begin
  inherited Create('DAM: ' + Msg);
end;

end.
