unit MailSend;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Console, Classes, SysUtils, 
  dnssend, smtpsend, synautil, synamisc;

const
 LineEnd = Chr(13) + Chr(10); {CR LF} 
 
var
 EmailHost:String = ''; {Fill in this if your network requires a relay host to send email}
 EmailSubject:String = 'Hello from Synapse';
 EmailMessage:String = 'Hello,' + LineEnd + LineEnd + 'This is an email sent from Synapse.' + LineEnd + LineEnd + 'Thanks for reading!' + LineEnd;
 
function SendEmail(const ToAddress,FromAddress:String):Boolean;

implementation

function SendEmail(const ToAddress,FromAddress:String):Boolean;
var
 SMTPClient:TSMTPsend;
 Domain:String;
 User:String;
 Address:String;
 DnsServers:TStringList;
 MailServers:TStringList;
 MailData:TStringList;
 Count:Integer;
 Success:Boolean;
begin
 Result:=False;
 
 Address:=GetEmailAddr(ToAddress);
 if Pos('@',Address) <= 0 then
  begin
   ConsoleWriteLn('Error: Invalid address - ' + Address);
   Exit; {Invalid address}
  end; 
 
 Domain:=SeparateRight(Address,'@');
 User:=SeparateLeft(Address,'@');
 if (Domain = '') or (User = '') then
  begin
   ConsoleWriteLn('Error: Invalid address - ' + Address);
   Exit; {Invalid address}
  end; 

 SMTPClient:=TSMTPsend.Create;
 DnsServers:=TStringList.Create;
 MailServers:=TStringList.Create;
 MailData:=TStringList.Create;
 try
  MailData.Add('Date: ' + FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "+0000 (UTC)"',Now));
  MailData.Add('Subject: ' + EmailSubject);
  MailData.Add('From: ' + FromAddress);
  MailData.Add('To: ' + ToAddress);
  MailData.Add('');
  MailData.Add(EmailMessage);
 
  if Length(EmailHost) <> 0 then
   begin
    Success:=False;
    
    SMTPClient.TargetHost:=EmailHost;
    if SMTPClient.Login then
     begin
      Success:=True;
        
      if not SMTPClient.MailFrom(FromAddress,0) then
       begin
        ConsoleWriteLn('Error: MailFrom failed for address - ' + FromAddress);
        Exit;
       end;
       
      if not SMTPClient.MailTo(ToAddress) then
       begin
        ConsoleWriteLn('Error: MailTo failed for address - ' + ToAddress);
        Exit;
       end;
      
      if not SMTPClient.MailData(MailData) then
       begin
        ConsoleWriteLn('Error: MailData failed');
        Exit;
       end;
     end
    else
     begin
      ConsoleWriteLn('Error: Login failed for mail server ' + EmailHost);
     end;       
    
    if not Success then
     begin
      ConsoleWriteLn('Error: Email could not be sent');
      Exit; {Email could not be sent}
     end; 
  
    Result:=True;
   end
  else
   begin  
    DnsServers.CommaText:=GetDNS;
    if DnsServers.Count = 0 then
     begin
      ConsoleWriteLn('Error: No DNS servers available');
      Exit; {No DNS servers available}
     end;
    ConsoleWriteLn('DNS servers are ' + DnsServers.CommaText);
    
    Success:=False;
    for Count:=0 to DnsServers.Count - 1 do
     begin
      if GetMailServers(DnsServers[Count],Domain,MailServers) then
       begin
        Success:=True;
        Break;
      end;
     end;   
    if not Success then
     begin
      ConsoleWriteLn('Error: No DNS could be contacted');
      Exit; {No DNS could be contacted}
     end; 
    
    if MailServers.Count = 0 then
     begin
      ConsoleWriteLn('Error: No mail servers found from DNS');
      Exit; {No mail servers found from DNS}
     end;
    ConsoleWriteLn('Mail servers are ' + MailServers.CommaText);
    
    Success:=False;
    for Count:=0 to MailServers.Count - 1 do
     begin
      SMTPClient.TargetHost:=MailServers[Count];
      if SMTPClient.Login then
       begin
        Success:=True;
        
        if not SMTPClient.MailFrom(FromAddress,0) then
         begin
          ConsoleWriteLn('Error: MailFrom failed for address - ' + FromAddress);
          Break;
         end;
         
        if not SMTPClient.MailTo(ToAddress) then
         begin
          ConsoleWriteLn('Error: MailTo failed for address - ' + ToAddress);
          Break;
         end;
        
        if not SMTPClient.MailData(MailData) then
         begin
          ConsoleWriteLn('Error: MailData failed');
          Break;
         end;
        
        Break;
       end
      else
       begin
        ConsoleWriteLn('Error: Login failed for mail server ' + MailServers[Count]);
       end;       
     end;
    
    if not Success then
     begin
      ConsoleWriteLn('Error: Email could not be sent');
      Exit; {Email could not be sent}
     end; 
  
    Result:=True;
   end;
 finally
  MailData.Free;
  MailServers.Free;
  DnsServers.Free;
  SMTPClient.Free;
 end;
end;

end.
