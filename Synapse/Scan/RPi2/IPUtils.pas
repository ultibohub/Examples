unit IPUtils;

{$IFDEF FPC}
  {$mode delphi}
{$endif}

interface

uses SysUtils;

type TIPAddress = record
                    Oct1,
                    Oct2,
                    Oct3,
                    Oct4:Byte;
                   end;

function StrToIP(const Value:String):TIPAddress;
function IPToStr(const Address:TIPAddress):String;
function IPToCardinal(const Address:TIPAddress):Cardinal;
function CardinalToIP(const Value:Cardinal):TIPAddress;
function IsIPAddress(const Value:String):Boolean;

implementation

// IPAddress in Cardinal umwandeln
function IPToCardinal(const Address:TIPAddress):Cardinal;
begin
  Result :=  (Address.Oct1*16777216)
            +(Address.Oct2*65536)
            +(Address.Oct3*256)
            +(Address.Oct4);
end;

// Cardinal in IP-Address umwandeln
function CardinalToIP(const Value:Cardinal):TIPAddress;
begin
  Result.Oct1 := Value div 16777216;
  Result.Oct2 := Value div 65536;
  Result.Oct3 := Value div 256;
  Result.Oct4 := Value mod 256;
end;

// IP-Address in String umwandeln
function IPToStr(const Address:TIPAddress):String;
begin
  Result := IntToStr(Address.Oct1) + '.' +
            IntToStr(Address.Oct2) + '.' +
            IntToStr(Address.Oct3) + '.' +
            IntToStr(Address.Oct4);
end;

function StrToIP(const Value:String):TIPAddress;
var
  n,x: Integer;
  Posi:array[1..4] of Integer;
  Octet:array[1..4] of String;
begin
  x := 0;
  // es dürfen nur Zahlen und Punkte vorhanden sein
  for n := 1 to Length(Value) do
    begin
      // Zähle die Punkte
      if Value[n] = '.'
        then
          begin
            Inc(x);
            Posi[x] := n;
          end
        else Octet[x+1] := Octet[x+1] + Value[n];
    end;
  Result.Oct1 := StrToInt(Octet[1]);
  Result.Oct2 := StrToInt(Octet[2]);
  Result.Oct3 := StrToInt(Octet[3]);
  Result.Oct4 := StrToInt(Octet[4]);
end;

function IsIPAddress(const Value:String):Boolean;
var 
  n,x,i: Integer;
  Posi:array[1..4] of Integer;
  Octet:array[1..4] of String;
begin
  Result := true;
  x := 0;

  // es dürfen nur Zahlen und Punkte vorhanden sein
  for n := 1 to Length(Value) do
    if not (Value[n] in ['0'..'9','.'])
      then
        begin
          // ungültiges Zeichen -> keine IP-Address
          Result := false;
          break;
        end
      else
        begin
          // Zähle die Punkte
          if Value[n] = '.'
            then
              begin
                Inc(x);
                Posi[x] := n;
              end
            else
              begin
                 Octet[x+1] := Octet[x+1] + Value[n];
              end;
        end;

  for i := 1 to 4 do
    if (StrToInt(Octet[i])>255)then Result := false;

  // es müssen genau 3 Punkte vorhanden sein
  if x <> 3
    then
      begin
        // Anzahl der Punkte <> 3 -> keine IP-Address
        Result := false;
      end;
end;

end.
